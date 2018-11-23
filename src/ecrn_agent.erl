%%% @copyright Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the BSD License; you may not use
%%% this file except in compliance with the License.
%%%-------------------------------------------------------------------
%%% @doc
%%%  Agent for cronish testing
-module(ecrn_agent).

-behaviour(gen_server).

%% API
-export([start_link/2,
         cancel/1,
         get_datetime/1,
         set_datetime/3,
         recalculate/1,
         validate/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {job,
                alarm_ref,
                referenced_seconds,
                seconds_at_reference,
                timeout_type,
                fast_forward=false}).

-define(MILLISECONDS, 1000).
-define(WAIT_BEFORE_RUN, 2000).

%%%===================================================================
%%% Types
%%%===================================================================

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Starts the server with the apropriate job and the appropriate ref
-spec start_link(JobRef::erlcron:job_ref(), Job::erlcron:job()) -> ignore | {error, term()} | {ok, pid()}.
start_link(JobRef, Job) ->
    gen_server:start_link(?MODULE, [JobRef, Job], []).

-spec get_datetime(Pid::pid()) -> calendar:datetime().
get_datetime(Pid) ->
    gen_server:call(Pid, get_datetime).

-spec cancel(Pid::pid()) -> ok.
cancel(Pid) ->
    gen_server:cast(Pid, shutdown).

-spec set_datetime(Pid::pid(), DateTime::calendar:datetime(), Actual::erlcron:seconds()) -> ok.
set_datetime(Pid, DateTime, Actual) ->
    gen_server:cast(Pid, {set_datetime, DateTime, Actual}).

-spec recalculate(Pid::pid()) -> ok.
recalculate(Pid) ->
    gen_server:cast(Pid, recalculate).

%% @doc
%%  Validate that a run_when spec specified is correct.
-spec validate(Spec::erlcron:run_when()) -> valid | invalid.
validate(Spec) ->
    State = #state{job=undefined,
                   alarm_ref=undefined},
    {DateTime, Actual} = ecrn_control:datetime(),
    NewState = set_internal_time(State, DateTime, Actual),
    try
        until_next_time(NewState, {Spec, undefined}),
        valid
    catch
        _Error:_Reason ->
            invalid
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([JobRef, Job]) ->
    State = #state{job=Job,
                   alarm_ref=JobRef},
    {DateTime, Actual} = ecrn_control:datetime(),
    NewState = set_internal_time(State, DateTime, Actual),
    case until_next_milliseconds(NewState, Job) of
        {ok, Millis} when is_integer(Millis) ->
            ecrn_reg:register(JobRef, self()),
            {ok, NewState, Millis};
        {error, _}  ->
            {stop, normal}
    end.

%% @private
handle_call(_Msg, _From, State) ->
    case until_next_milliseconds(State, State#state.job) of
        {ok, Millis} ->
            {reply, ok, State, Millis};
        {error, _}  ->
            {stop, normal, ok, State}
    end.

%% @private
handle_cast(shutdown, State) ->
    {stop, normal, State};
handle_cast({set_datetime, DateTime, Actual}, State) ->
    fast_forward(State#state{fast_forward=true}, DateTime),
    NewState = set_internal_time(State, DateTime, Actual),
    case until_next_milliseconds(NewState, NewState#state.job) of
        {ok, Millis} ->
            {noreply, NewState, Millis};
        {error, _}  ->
            {stop, normal, NewState}
    end.

%% @private
handle_info(timeout, State = #state{job = {{once, _}, _}}) ->
    do_job_run(State, State#state.job),
    {stop, normal, State};
handle_info(timeout, State = #state{timeout_type=wait_before_run}) ->
    NewState = State#state{timeout_type=normal},
    case until_next_milliseconds(NewState, NewState#state.job) of
        {ok, Millis} ->
            {noreply, NewState, Millis};
        {error, _}  ->
            {stop, normal, NewState}
    end;
handle_info(timeout, State = #state{job = Job}) ->
    do_job_run(State, Job),
    NewState = State#state{timeout_type=wait_before_run},
    {noreply, NewState, ?WAIT_BEFORE_RUN}.


%% @private
terminate(_Reason, #state{alarm_ref=Ref}) ->
    ecrn_reg:unregister(Ref),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_job_run(State, {_, Job})
  when is_record(State, state), is_function(Job) ->
    RunFun = fun() ->
                     Job(State#state.alarm_ref, current_date(State))
             end,
    proc_lib:spawn(RunFun);
do_job_run(State, {_, {M, F, A}})
  when is_record(State, state) ->
    proc_lib:spawn(M, F, A).

%% @doc Returns the current time, in seconds past midnight.
-spec current_time(State::state()) -> erlcron:seconds().
current_time(State) ->
    {_, {H, M, S}} = current_date(State),
    S + M * 60 + H * 3600.

current_date(State = #state{fast_forward=true}) ->
    calendar:gregorian_seconds_to_datetime(State#state.referenced_seconds);
current_date(State) ->
    Elapsed = ecrn_util:epoch_seconds() - State#state.seconds_at_reference,
    calendar:gregorian_seconds_to_datetime(ceiling(Elapsed +
                                                 State#state.referenced_seconds)).

%% @doc Calculates the duration in milliseconds until the next time
%% a job is to be run.
-spec until_next_milliseconds(State::state(), Job::erlcron:job()) ->
          {ok, erlcron:seconds()} | {error, invalid_one_exception}.
until_next_milliseconds(State, Job) ->
    try
        Millis = until_next_time(State, Job) * ?MILLISECONDS,
        {ok, Millis}
    catch
        throw:invalid_once_exception ->
            {error, invalid_once_exception}
    end.

normalize_seconds(State, Seconds) ->
    case Seconds - current_time(State) of
        Value when Value >= 0 ->
            Value;
        _ ->
            throw(invalid_once_exception)
    end.

%% @doc Calculates the duration in seconds until the next time
%% a job is to be run.
-spec until_next_time(State::state(), {erlcron:run_when(), term()}) -> erlcron:seconds().
until_next_time(_State, {{once, Seconds}, _What}) when is_integer(Seconds) ->
    Seconds;
until_next_time(State, {{once, {H, M, S}}, _What}) when is_integer(H), is_integer(M), is_integer(S) ->
    normalize_seconds(State, S + (M + (H * 60)) * 60);
until_next_time(State, {{once, Period}, _What}) ->
    normalize_seconds(State, resolve_time(Period));
until_next_time(State, {{daily, Period}, _What}) ->
    until_next_daytime(State, Period);
until_next_time(State, {{weekly, DoW, Period}, _What}) ->
    {ThisDate, ThisTime} = current_date(State),
    until_days_from_now(calendar:day_of_the_week(ThisDate), resolve_dow(DoW), ThisTime, Period, 7);
until_next_time(State, {{monthly, DoM, Period}, _What}) ->
    {{ThisYear, ThisMonth, Today}, ThisTime} = current_date(State),
    until_days_from_now(Today, DoM, ThisTime, Period, calendar:last_day_of_the_month(ThisYear, ThisMonth)).

%% @doc Calculates the duration in seconds until the next time this
%% period is to occur during the day.
-spec until_next_daytime(State::state(), Period::erlcron:period()) -> erlcron:seconds().
until_next_daytime(State, Period) ->
    StartTime = first_time(Period),
    EndTime = last_time(Period),
    case current_time(State) of
        T when T > EndTime ->
            until_tomorrow(State, StartTime);
        T ->
            next_time(Period, T) - T
    end.

%% @doc Calculates the last time in a given period.
-spec last_time(Period::erlcron:period()) -> erlcron:seconds().
last_time(Period) ->
    hd(lists:reverse(lists:sort(resolve_period(Period)))).

%% @doc Calculates the first time in a given period.
-spec first_time(Period::erlcron:period()) -> erlcron:seconds().
first_time(Period) ->
    hd(lists:sort(resolve_period(Period))).

%% @doc Calculates the first time in the given period after the given time.
-spec next_time(Period::erlcron:period(), Time::erlcron:seconds()) -> erlcron:seconds().
next_time(Period, Time) ->
    R = lists:sort(resolve_period(Period)),
    lists:foldl(fun(X, A) ->
                        case X of
                            T when T >= Time, T < A ->
                                T;
                            _ ->
                                A
                        end
                end, 24*3600, R).

%% @doc Returns a list of times given a periodic specification.
-spec resolve_period([erlcron:period()] | erlcron:period()) -> [erlcron:seconds()].
resolve_period([]) ->
    [];
resolve_period([H | T]) ->
    resolve_period(H) ++ resolve_period(T);
resolve_period({every, Duration, {between, TimeA, TimeB}}) ->
    Period = resolve_dur(Duration),
    StartTime = resolve_time(TimeA),
    EndTime = resolve_time(TimeB),
    resolve_period0(Period, StartTime, EndTime, []);
resolve_period(Time) ->
    [resolve_time(Time)].

resolve_period0(_, Time, EndTime, Acc) when Time >= EndTime ->
    Acc;
resolve_period0(Period, Time, EndTime, Acc) ->
    resolve_period0(Period, Time + Period, EndTime, [Time | Acc]).

%% @doc Returns seconds past midnight for a given time.
-spec resolve_time(erlcron:cron_time()) -> erlcron:seconds().
resolve_time({H, M, S}) when H < 24, M < 60, S < 60  ->
    S + M * 60 + H * 3600;
resolve_time({H, M, S, X}) when  H < 24, M < 60, S < 60, is_atom(X) ->
    resolve_time({H, X}) + M * 60 + S;
resolve_time({H, M, X}) when  H < 24, M < 60, is_atom(X) ->
    resolve_time({H, X}) + M * 60;
resolve_time({12, am}) ->
    0;
resolve_time({H,  am}) when H < 12 ->
    H * 3600;
resolve_time({12, pm}) ->
    12 * 3600;
resolve_time({H,  pm}) when H < 12->
    (H + 12) * 3600.

%% @doc Returns seconds for a given duration.
-spec resolve_dur(erlcron:duration()) -> erlcron:seconds().
resolve_dur({Hour, hr}) ->
    Hour * 3600;
resolve_dur({Min, min}) ->
    Min * 60;
resolve_dur({Sec, sec}) ->
    Sec.

%% @doc Returns the number of the given day of the week. See the calendar
%% module for day numbers.
-spec resolve_dow(erlcron:dow()) -> integer().
resolve_dow(mon) ->
    1;
resolve_dow(tue) ->
    2;
resolve_dow(wed) ->
    3;
resolve_dow(thu) ->
    4;
resolve_dow(fri) ->
    5;
resolve_dow(sat) ->
    6;
resolve_dow(sun) ->
    7.

%% @doc Calculates the duration in seconds until the given time occurs
%% tomorrow.
-spec until_tomorrow(State::state(), StartTime::erlcron:seconds()) -> erlcron:seconds().
until_tomorrow(State, StartTime) ->
    (StartTime + 24*3600) - current_time(State).

%% @doc Calculates the duration in seconds until the given period
%% occurs several days from now.
-spec until_days_from_now(Today::1..7|1..31, Day::1..7|1..31, ThisTime::calendar:time(),
                          Period::erlcron:period(), Days::7|28..31) ->
          erlcron:seconds().
until_days_from_now(Today, Day, ThisTime, Period, Days) ->
    ThisSeconds = calendar:time_to_seconds(ThisTime),
    PeriodSeconds = resolve_time(Period),
    if
        Today =:= Day, ThisSeconds < PeriodSeconds -> PeriodSeconds;
        Today < Day -> (Day - Today) * (24 * 3600) + PeriodSeconds;
        true -> (Days + Day - Today) * (24 * 3600) + PeriodSeconds
    end - ThisSeconds.

set_internal_time(State, RefDate, CurrentSeconds) ->
    NewSeconds = calendar:datetime_to_gregorian_seconds(RefDate),
    State#state{referenced_seconds=NewSeconds,
                seconds_at_reference=CurrentSeconds}.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

fast_forward(State, NewDate) ->
    try
        Seconds = until_next_time(State, State#state.job),
        NewSeconds = calendar:datetime_to_gregorian_seconds(NewDate),
        Span = NewSeconds - State#state.referenced_seconds,
        case Span > Seconds of
            true ->
                RefSecs = State#state.referenced_seconds,
                NewState = State#state{referenced_seconds = RefSecs + Seconds + 2},
                do_job_run(State, State#state.job),
                fast_forward(NewState, NewDate);
            false ->
                ok
        end
    catch
        throw:invalid_once_exception ->
            {error, invalid_once_exception}
    end.
