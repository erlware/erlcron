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
-export([start_link/3,
         cancel/1,
         next_run/1,
         get_datetime/1,
         get_datetime/2,
         set_datetime/3,
         set_datetime/4,
         recalculate/1,
         validate/1,
         until_next_time/2,
         normalize/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("internal.hrl").

-record(state, {job,
                opts,
                job_ref,
                ref_epoch,
                epoch_at_ref,
                fast_forward=false,
                last_time,
                next_run,  % Time in EPOCH seconds until task's next execution
                last_run,  % Time in EPOCH seconds when the task was last run
                orig_when
               }).

-define(MILLISECONDS,      1000).
-define(WAIT_BEFORE_RUN,   1000).
-define(MAX_TIMEOUT_SEC,   1800).
-define(MAX_TIMEOUT_MSEC,  1800000).
-define(SEC_IN_A_DAY,      86400).

-include("erlcron.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Starts the server with the appropriate job and the appropriate ref
-spec start_link(erlcron:job_ref(), erlcron:job(), erlcron:job_opts()) ->
                             ignore | {error, Reason::term()} | {ok, pid()}.
start_link(JobRef, {When, _Task} = Job, JobOpts) when (is_reference(JobRef) orelse is_binary(JobRef))
                                                    , is_map(JobOpts) ->
    {Sched, DateTime, ActualMsec} = normalize_when(When),
    gen_server:start_link(?MODULE, [JobRef, Job, Sched, DateTime, ActualMsec, JobOpts], []);
start_link(JobRef, {When, _Task} = Job, JobOpts) when is_atom(JobRef), is_map(JobOpts) ->
    {Sched, DateTime, ActualMsec} = normalize_when(When),
    gen_server:start_link({local, JobRef}, ?MODULE, [JobRef, Job, Sched, DateTime, ActualMsec, JobOpts], []).

-spec get_datetime(pid()) -> calendar:datetime().
get_datetime(Pid) ->
    get_datetime(Pid, local).

-spec get_datetime(pid(), local|universal) -> calendar:datetime().
get_datetime(Pid, local) ->
    erlang:universaltime_to_localtime(get_datetime(Pid, universal));
get_datetime(Pid, universal) ->
    gen_server:call(Pid, get_datetime).

-spec cancel(pid()) -> ok.
cancel(Pid) when is_pid(Pid); is_atom(Pid) ->
    gen_server:cast(Pid, shutdown).

-spec set_datetime(pid(), calendar:datetime(), erlcron:milliseconds()) -> ok.
set_datetime(Pid, DateTime, Actual) ->
    set_datetime(Pid, DateTime, Actual, local).

-spec set_datetime(pid(), calendar:datetime(), erlcron:milliseconds(), UTC::local|universal) ->
        ok | {error, term()}.
set_datetime(Pid, DateTime, Actual, UTC) ->
    DT =  case UTC of
              local     -> erlang:localtime_to_universaltime(DateTime);
              universal -> DateTime
          end,
    Res = gen_server:call(Pid, {set_datetime, DT, Actual}, infinity),
    Res.

-spec recalculate(pid()) -> ok.
recalculate(Pid) ->
    gen_server:cast(Pid, recalculate).

%% @doc
%% Get job's next execution time
-spec next_run(pid()) -> erlang:timestamp().
next_run(Pid) ->
    gen_server:call(Pid, next_run).

%% @doc
%%  Validate that a run_when spec specified is correct.
-spec validate(erlcron:run_when()) -> ok | {error, term()}.
validate(Spec) ->
    State = #state{job=undefined, job_ref=undefined},
    {DateTime, ActualMsec} = ecrn_control:ref_datetime(universal),
    NewState = set_internal_time(State, DateTime, ActualMsec),
    try
        NormalSpec = normalize(Spec, DateTime),
        {Msec,_}   = until_next_time(NewState#state{job={NormalSpec, undefined}}),
        Msec > 0 orelse throw({specified_time_past_seconds_ago, to_seconds(Msec)}),
        ok
    catch
        _Error:Reason ->
            {error, Reason}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @private
init([JobRef, {When, Task}, Sched, DateTime, ActualMsec, JobOpts]) ->
    try
        State = set_internal_time(#state{job_ref=JobRef, job={Sched, Task}, opts=JobOpts,
                                         last_run=0, next_run=0, orig_when=When},
                                  DateTime, ActualMsec),
        case until_next_milliseconds(State) of
            {ok, {NewState, Timeout}} ->
                ecrn_reg:register(JobRef, self()),
                {ok, NewState, Timeout};
            {error, {_, Reason}}  ->
                throw(Reason)
        end
    catch ?EXCEPTION(_, E, ST) ->
        ?LOG_ERROR([{error, "Error setting timeout for job"},
                    {job_ref, JobRef}, {run_when, When},
                    {reason, E},
                    {stack,  ?GET_STACK(ST)}]),
        {stop, normal}
    end.

%% @private
handle_call(next_run, _From, #state{next_run=Time} = State) ->
    reply_and_wait(Time, State);
handle_call(get_datetime, _From, State) ->
    Epoch = ecrn_util:epoch_milliseconds(),
    Msec  = current_epoch(Epoch, State),
    DT    = to_universal_datetime(Msec),
    reply_and_wait(DT, State#state{last_time=Epoch});
handle_call({set_datetime, DateTime, CurrEpochMsec}, _From, State) ->
    try
        NewRefEpoch = to_milliseconds(erlang:universaltime_to_posixtime(DateTime)),
        case fast_forward(State#state{fast_forward=true}, NewRefEpoch, DateTime, 1) of
            true ->
                NewState = set_internal_time(State#state{next_run=0}, DateTime, CurrEpochMsec),
                reply_and_wait(ok, NewState);
            false ->
                {stop, normal, ok, State}
        end
    catch ?EXCEPTION(_, E, ST) ->
        ?LOG_ERROR([{error, "Error setting timeout for job"},
                    {job_ref, State#state.job_ref},
                    {run_when, element(1, State#state.orig_when)},
                    {reason, E},
                    {stack,  ?GET_STACK(ST)}]),
        reply_and_wait({error, E}, State)
    end;
handle_call(Msg, _From, State) ->
    {stop, State, {not_implemented, Msg}}.

%% @private
handle_cast(shutdown, State) ->
    {stop, normal, State};
handle_cast(Other, State) ->
    {stop, {not_implemented, Other}, State}.

%% @private
handle_info(timeout, S) ->
    process_timeout(S);

handle_info({'EXIT', _, Reason}, State) ->
    {stop, Reason, State};
handle_info(_Msg, State) ->
    {noreply, State, 1000}.


%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
normalize_when(When) ->
    {DateTime, ActualMsec} = ecrn_control:ref_datetime(universal),
    Sched = normalize(When, erlang:universaltime_to_localtime(DateTime)),
    {Sched, DateTime, ActualMsec}.

process_timeout(#state{last_time=LastTime, next_run=NextRun, last_run=LastRun}=S) ->
    Epoch   = ecrn_util:epoch_milliseconds(),
    NowMsec = current_epoch(Epoch, S),
    {Reply, State} =
        if
            LastTime < Epoch, NextRun > LastRun, NowMsec >= NextRun ->
                {do_job_run(S), S#state{last_run=NextRun, next_run=0}};
            NextRun == LastRun ->
                S#state.fast_forward orelse timer:sleep(1),
                {noreply, S#state{next_run=0}};
            true ->
                {noreply, S#state{last_time=Epoch}}
        end,
    reply_and_wait(Reply, State).

do_job_run(#state{job_ref=Ref, next_run=Time, job={When, Job}, opts=Opts} = S) ->
    Res  = do_job_start(Ref, maps:get(on_job_start, Opts, undefined)),
    Res /= ignore andalso execute(Job, Ref, Time, maps:get(on_job_end, Opts, undefined)),
    case When of
        {once, _} ->
            stop;
        _ ->
            S#state.fast_forward orelse timer:sleep(1),
            noreply
    end.

do_job_start(_,   undefined)                -> ok;
do_job_start(Ref, {M,F})                    -> M:F(Ref);
do_job_start(Ref, F) when is_function(F, 1) -> F(Ref).

execute(Job, Ref, Time, OnEnd) when is_function(Job, 2) ->
    safe_spawn(Ref, fun() -> Job(Ref, Time) end, OnEnd);
execute(Job, Ref, _, OnEnd) when is_function(Job, 0) ->
    safe_spawn(Ref, Job, OnEnd);
execute({M, F}, Ref, Time, OnEnd) when is_atom(M), is_atom(F) ->
    %% The exported function is checked for arity 0 or 2 when the job is added
    case erlang:function_exported(M, F, 2) of
        true  -> safe_spawn(Ref, fun() -> M:F(Ref, Time) end, OnEnd);
        false -> safe_spawn(Ref, fun() -> M:F()          end, OnEnd)
    end;
execute({M, F, A}, Ref, _Time, OnEnd) when is_atom(M), is_atom(F), is_list(A) ->
    safe_spawn(Ref, fun() -> apply(M, F, A) end, OnEnd).

safe_spawn(Ref, Fun, _OnEnd = {M,F}) when is_atom(M), is_atom(F) ->
    safe_spawn(Ref, Fun, fun(JobRef, Res) -> M:F(JobRef, Res) end);

safe_spawn(Ref, Fun, OnEnd) when is_function(OnEnd, 2) ->
    proc_lib:spawn(fun() ->
      try
          OnEnd(Ref, {ok, Fun()})
      catch _:Reason:Stack ->
          OnEnd(Ref, {error, {Reason, Stack}})
      end
    end);

safe_spawn(_Ref, Fun, undefined) ->
    proc_lib:spawn(Fun).

reply_and_wait(stop, State) ->
    {stop, normal, State};
reply_and_wait(Reply, #state{next_run=0} = State) ->
    reply_and_wait2(Reply, until_next_milliseconds(State));
reply_and_wait(Reply, #state{next_run=Next, last_run=Last} = State) when Next-Last =< 0 ->
    reply_and_wait2(Reply, {ok, {State, 1000}});
reply_and_wait(Reply, #state{last_time=Now, next_run=NextMsec} = State) ->
    NowTime = current_epoch(Now, State),
    Timeout = round_timeout(NextMsec-NowTime),
    reply_and_wait2(Reply, {ok, {State, Timeout}}).

reply_and_wait2(noreply, {ok, {State, Timeout}}) -> {noreply, State, Timeout};
reply_and_wait2(Reply,   {ok, {State, Timeout}}) -> {reply, Reply, State, Timeout};
reply_and_wait2(noreply, {error, {State,    _}}) -> {stop, normal, State};
reply_and_wait2(Reply,   {error, {State,    _}}) -> {stop, normal, Reply, State}.

%% @doc Calculates the duration in milliseconds until the next time
%% a job is to be run.
%% The returned duration is set to the maximum of 1 hour at which
%% point the duration is recalculated, so that daylight savings time
%% adjustments don't affect the precision of when a task is run.
-spec until_next_milliseconds(#state{}) ->
    {ok,    {#state{}, erlcron:milliseconds()}} |
    {error, {#state{}, term()}}.
until_next_milliseconds(#state{} = S) ->
    try
        Epoch      = ecrn_util:epoch_milliseconds(),
        {Msec, S1} = until_next_time(S#state{last_time=Epoch}),
        {ok, update_next_run(S1, max(0, Msec))}
    catch
        ?EXCEPTION(_, E, ST) ->
            {error, {S, {E, ?GET_STACK(ST)}}}
    end.

-spec update_next_run(#state{}, erlcron:seconds()) -> {#state{}, erlcron:milliseconds()}.
update_next_run(#state{} = State, ?SEC_IN_A_DAY) ->
    {State, ?MAX_TIMEOUT_MSEC};
update_next_run(#state{last_time=Now} = State, Milliseconds) ->
    Timeout = round_timeout(Milliseconds),
    EpochS  = current_epoch(Now, State),
    {State#state{next_run=EpochS+Milliseconds}, Timeout}.

round_timeout(Milliseconds) ->
    trunc(max(0, min(Milliseconds, ?MAX_TIMEOUT_MSEC))).

-type normalized_period() :: [{integer(), integer(), integer()}].
-type normalized_sched()  ::
        {once,   {relative|absolute, integer()}} |
        {daily,  normalized_period()} |
        {weekly, [integer()], normalized_period()} |
        {monthly,integer(),   normalized_period()}.

-spec normalize(erlcron:run_when(), calendar:datetime()) -> normalized_sched().
normalize({once,  I}, _) when is_integer(I) ->
    {once, {relative, to_milliseconds(I)}};
normalize({once, {H,M,S}=T}, {_, Time}) when is_integer(H), is_integer(M), is_integer(S)
                                           , H >= 0, H < 24, M >= 0, M < 59, S >= 0, S < 59 ->
    T < Time andalso throw({time_given_in_the_past, T}),
    {once, {absolute, resolve_time(T)}};
normalize({once, Period}, _) ->
    {once, {absolute, resolve_time(Period)}};
normalize({daily, Period}, _) ->
    {daily, lists:sort(resolve_period(Period))};
normalize({weekly, DoW, Period}, _) ->
    {weekly, resolve_dow(DoW), lists:sort(resolve_period(Period))};
normalize({monthly, Day, Period}, TodayDate) when is_integer(Day) ->
    normalize({monthly, [Day], Period}, TodayDate);
normalize({monthly, Days, Period}=T, {{Y,M,_},_}) when is_list(Days) ->
    case [Day || Day <- Days, not is_integer(Day) orelse Day < -30 orelse Day > 31] of
        [] -> ok;
        _  -> throw({invalid_days_in_schedule, T})
    end,
    {PosDays, NegDays} = lists:partition(fun(I) -> I > 0 end, Days),
    {ThisNextMonthDays, _State} =
        update_dom(Y,M, {undefined, {0,0, PosDays, NegDays}}, #state{job={T, undefined}}),
    {monthly, ThisNextMonthDays, lists:sort(resolve_period(Period))};
normalize(Other, _) ->
    throw({invalid_schedule, Other}).

update_dom(Y,M, {_, {Y,M,_,_}}=When, State) ->
    {When, State};
update_dom(Y,M, {_, {_,_,PosDays,NegDays}},
           State = #state{job = {{monthly, _, _Period}, Task}}) ->
    {_,  _, ThisMonthDays}   = month_days(Y, M, PosDays, NegDays, true),
    {NY,NM, NextMonFirstDay} = next_month_days(Y, M, PosDays, NegDays),
    ThisNextMonthDays = {ThisMonthDays, NY, NM, NextMonFirstDay},
    When              = {ThisNextMonthDays, {Y,M,PosDays,NegDays}},
    {When, State#state{job={{monthly, When}, Task}}}.


next_month_days(Y, 12, PosDays, NegDays) ->
    month_days(Y+1, 1, PosDays, NegDays, false);
next_month_days(Y, M,  [], NegDays) ->
    month_days(Y, M+1, [], NegDays, false);
next_month_days(Y, M,  PosDays, NegDays) ->
    % When checking next month, and PosDays contains days 29,30,31
    % make sure that combining that with {Y,M} gives a valid date
    D  = lists:min(PosDays),
    M1 = M+1,
    case calendar:valid_date({Y,M1,D}) of
        true ->
            month_days(Y, M1, PosDays, NegDays, false);
        false ->
            next_month_days(Y, M1, PosDays, NegDays)
    end.

month_days(Y, M, PosDays, NegDays, false) ->
    LastDay = calendar:last_day_of_the_month(Y, M),
    Days    = [I+LastDay || I <- NegDays] ++ PosDays,
    case [I || I <- Days, I =< LastDay, I > 0] of
        [] -> next_month_days(Y, M, PosDays, NegDays);
        L  -> {Y, M, lists:min(L)}
    end;
month_days(Y, M, PosDays, [], true) ->
    LastDay = calendar:last_day_of_the_month(Y, M),
    {Y, M, [I || I <- lists:sort(PosDays), I =< LastDay]};
month_days(Y, M, PosDays, NegDays, true) ->
    LastDay = calendar:last_day_of_the_month(Y, M),
    AllDays = PosDays ++ [I+LastDay || I <- NegDays],
    UniqDays= lists:sort(sets:to_list(sets:from_list(AllDays))),
    check_days(Y, M, UniqDays),
    {Y, M, [I || I <- UniqDays, I =< LastDay, I > 0]}.

check_days(Y,M,Days) ->
    lists:foreach(fun(I) ->
        Date = {Y,M,I},
        calendar:valid_date(Date) orelse throw({invalid_date, Date})
    end, lists:takewhile(fun(I) -> I > 28 end, lists:reverse(Days))).

%% @doc Calculates the duration in milliseconds until the next time
%% a job is to be run.
-spec until_next_time(erlcron:milliseconds(), erlcron:run_when()) -> erlcron:milliseconds().
until_next_time(NowEpochTime, Sched) when is_integer(NowEpochTime), is_tuple(Sched) ->
    DateTime  = erlang:posixtime_to_universaltime(to_ceiling_seconds(NowEpochTime)),
    NormSched = normalize(Sched, DateTime),
    State     = set_internal_time(#state{job={NormSched, undefined}},
                                  DateTime, NowEpochTime),
    {Res, _State1} = until_next_time(State),
    Res.

%% @doc Calculates the duration in milliseconds until the next time
%% a job is to be run.
%% @private
-spec until_next_time(state()) -> {erlcron:milliseconds(), state()}.
until_next_time(State = #state{job={Sched, _Task}}) ->
    until_next_time2(State, Sched).

until_next_time2(State, {once, {relative, Msec}}) when is_integer(Msec) ->
    {Msec, State};
until_next_time2(State, {once, {absolute, Msec}}) when is_integer(Msec) ->
    {normalize_to_relative(State, Msec), State};
until_next_time2(State, {daily, Period}) ->
    {until_next_daytime(State, Period, current_time(State)), State};
until_next_time2(State, {weekly, DoW, Period}) ->
    {Date, _} = current_date(State),
    Today     = calendar:day_of_the_week(Date),
    Days      = find_dow(Today, DoW),
    NextDays  = fun() ->
                  I = Today+1,
                  N = if I>7 -> 1; true -> I end,
                  find_dow(N, DoW)
                end,
    {until_next_time_from_now(State, Period, Days, NextDays), State};
until_next_time2(State, {monthly, When, Period}) ->
    {{ThisYear, ThisMonth, Today}=YMD, _} = current_date(State),
    {{{ThisMonthDays, NextY, NextM, NextMonthFirstDOM}, _}, State1} =
        update_dom(ThisYear, ThisMonth, When, State),

    {YMD1, YMD2} =
        case lists:dropwhile(fun(I) -> I < Today end, ThisMonthDays) of
            [DOM1, DOM2 | _] ->
                {{ThisYear,ThisMonth,DOM1}, {ThisYear,ThisMonth,DOM2}};
            [DOM|_] ->
                {{ThisYear,ThisMonth,DOM},  {NextY,NextM,NextMonthFirstDOM}};
            [] ->
                {{NextY, NextM, NextMonthFirstDOM}, {0,0,0}}
        end,

    TodayGreg = to_greg_days(YMD),
    NextGreg  = to_greg_days(YMD1),
    Days      = NextGreg - TodayGreg,
    NextDays  = fun() -> to_greg_days(YMD2) - TodayGreg end,
    {until_next_time_from_now(State1, Period, Days, NextDays), State1}.

until_next_time_from_now(State, Period, 0, NextDays) when is_function(NextDays, 0) ->
    CurrentTime = current_time(State),
    F = fun() -> until_days_from_now(State, Period, NextDays()) end,
    case last_time(Period) of
        T when CurrentTime > T ->
            F();
        _ ->
            EndTime = last_time(Period),
            case next_time(Period, CurrentTime) of
                I when I > EndTime ->
                    F();
                I ->
                    I - CurrentTime
            end
    end;
until_next_time_from_now(State, Period, Days, _NextDays) when Days > 0 ->
    until_days_from_now(State, Period, Days-1).

to_greg_days({Y,M,D}) ->
    calendar:date_to_gregorian_days(Y,M,D).

%% @doc Calculates the time since midnight in milliseconds until the next time this
%% period is to occur during the day.
-spec until_next_daytime(state(), normalized_period(), erlcron:milliseconds()) ->
        erlcron:milliseconds().
until_next_daytime(State, Period, MsecsFromMidnight) ->
    StartTime = first_time(Period),
    EndTime   = last_time(Period),
    if MsecsFromMidnight > EndTime ->
        until_tomorrow(State, StartTime);
    true ->
        case next_time(Period, MsecsFromMidnight) of
            T when T > EndTime ->
                until_tomorrow(State, StartTime);
            T ->
                T - MsecsFromMidnight
        end
    end.

%% @doc Calculates the duration in seconds until the given time occurs
%% tomorrow.
-spec until_tomorrow(state(), erlcron:milliseconds()) -> erlcron:milliseconds().
until_tomorrow(State, StartTime) ->
    StartTime + 24*3600000 - current_time(State).

%% @doc Calculates the duration in milliseconds until the given period
%% occurs several days from now.
-spec until_days_from_now(state(), normalized_period(), integer()) ->
                                       erlcron:milliseconds().
until_days_from_now(State, Period, Days) ->
    MsecsUntilTomorrow = until_tomorrow(State, 0),
    MsecsUntilTomorrow + Days * 24 * 3600000 + until_next_daytime(State, Period, 0).

%% @doc Calculates the first time in a given period.
-spec first_time(normalized_period()) -> erlcron:milliseconds().
first_time([]) ->
    ?SEC_IN_A_DAY;
first_time([{FromTime, _ToTime, _RepeatSec}|_]) ->
    FromTime.

%% @doc Calculates the last time in a given period.
-spec last_time(normalized_period()) -> erlcron:milliseconds().
last_time([]) ->
    ?SEC_IN_A_DAY;
last_time(Period) when is_list(Period) ->
    F = fun({FromTime, _ToTime, 0}) ->
                FromTime;
           ({FromTime, ToTime, RepeatSec}) ->
                FromTime + ((ToTime-FromTime) div RepeatSec) * RepeatSec
        end,
    lists:max([F(R) || R <- Period]).

%% @doc Calculates the first time in the given period after the given time.
-spec next_time(normalized_period(), erlcron:milliseconds()) -> erlcron:milliseconds().
next_time([], _Time) ->
    ?SEC_IN_A_DAY;
next_time(Period, Time) when is_list(Period), is_integer(Time) ->
    F = fun({FromTime, _ToTime, _RepeatSec}) when Time =< FromTime ->
              FromTime;
           ({_FromTime, _ToTime, 0}) ->
              ?SEC_IN_A_DAY;
           ({FromTime, _ToTime, RepeatSec}) ->
              A1 = ((Time-FromTime) div RepeatSec) * RepeatSec,
              A  = if A1 < Time -> A1 + RepeatSec; true -> A1 end,
              FromTime + A
        end,
    case [F(R) || R = {_, ToTm, _} <- Period, Time =< ToTm] of
        [] -> ?SEC_IN_A_DAY;
        LL -> lists:min(LL)
    end.

%% @doc Returns the current time, in milliseconds past midnight in local time zone.
-spec current_time(state()) -> erlcron:milliseconds().
current_time(State = #state{last_time=Now}) ->
    % Take ceiling of time to the next second
    Msec = case (Now div 1000) * 1000 of
               Now -> 0;
               N   -> Now-N
           end,
    {_, {H,M,S}} = current_date(State),
    to_milliseconds(S + M * 60 + H * 3600) + Msec.

%% @doc Returns current date in local time
current_date(#state{last_time=Now, ref_epoch=E, epoch_at_ref=S}) when is_integer(Now), Now > 0 ->
    Elapsed = Now - S,
    to_local_datetime(Elapsed + E).

current_epoch(EpochMsec, #state{ref_epoch=RefEpoch, epoch_at_ref=Epoch}) ->
    Elapsed = EpochMsec - Epoch,
    Elapsed + RefEpoch.

normalize_to_relative(State, Milliseconds) ->
    NowSec = current_time(State),
    Milliseconds - NowSec.

%% @doc Returns a list of times given a periodic specification.
-spec resolve_period([erlcron:period()] | erlcron:period()) ->
        [{erlcron:milliseconds(), erlcron:milliseconds(), erlcron:milliseconds()}].
resolve_period([]) ->
    [];
resolve_period([H | T]) ->
    resolve_period(H) ++ resolve_period(T);
resolve_period({every, Duration}) ->
    resolve_period({every, Duration, {between, {0,0,0}, {23,59,59}}});
resolve_period({every, Duration, {between, TimeA, TimeB}} = P) ->
    Period    = resolve_dur(Duration),
    StartTime = resolve_time(TimeA),
    EndTime   = resolve_time(TimeB),
    StartTime =< EndTime orelse throw({invalid_period, P}),
    [{StartTime, EndTime, Period}];
resolve_period(Time) ->
    T = resolve_time(Time),
    [{T, T, 0}].

%% @doc Returns seconds past midnight for a given time.
-spec resolve_time(erlcron:cron_time()) -> erlcron:milliseconds().
resolve_time({H, M, S}) when H < 24, M < 60, S < 60  ->
    to_milliseconds(S + M * 60 + H * 3600);
resolve_time({H, M, S, X}) when  H < 24, M < 60, S < 60, is_atom(X) ->
    resolve_time({H, X}) + to_milliseconds(M*60 + S);
resolve_time({H, M, X}) when  H < 24, M < 60, is_atom(X) ->
    resolve_time({H, X}) + to_milliseconds(M*60);
resolve_time({12, am}) ->
    0;
resolve_time({H,  am}) when H < 12 ->
    to_milliseconds(H  * 3600);
resolve_time({12, pm}) ->
    to_milliseconds(12 * 3600);
resolve_time({H,  pm}) when H < 12 ->
    to_milliseconds((H + 12) * 3600);
resolve_time(Other) ->
    throw({invalid_time, Other}).

%% @doc Returns seconds for a given duration.
-spec resolve_dur(erlcron:duration())    -> erlcron:milliseconds().
resolve_dur({Hour, H}) when H==h; H==hr  -> to_milliseconds(Hour * 3600);
resolve_dur({Min,  M}) when M==m; M==min -> to_milliseconds(Min  * 60);
resolve_dur({Sec,  S}) when S==s; S==sec -> to_milliseconds(Sec);
resolve_dur({Mil,  M}) when M==ms; M==milli; M==millisecond -> Mil;
resolve_dur(Other)                       -> throw({invalid_duration, Other}).

%% @doc Returns the number of the given day of the week. See the calendar
%% module for day numbers.
-spec resolve_dow(erlcron:dow()) -> [integer()].
resolve_dow(I) when is_atom(I) ->
    [resolve_dow2(I)];
resolve_dow([I]) ->
    [resolve_dow2(I)];
resolve_dow(L) when is_list(L) ->
    lists:sort([resolve_dow2(I) || I <- L]).

resolve_dow2(mon) -> 1;
resolve_dow2(tue) -> 2;
resolve_dow2(wed) -> 3;
resolve_dow2(thu) -> 4;
resolve_dow2(fri) -> 5;
resolve_dow2(sat) -> 6;
resolve_dow2(sun) -> 7.

-spec find_dow(integer(), [integer()]) -> integer().
find_dow(_TodayDOW, []) ->
    throw(invalid_dow_period);
find_dow(TodayDOW, [FirstDOW|_]=DOWs) ->
    find_dow(TodayDOW, DOWs, FirstDOW).

find_dow(TodayDOW, [TodayDOW|_], _) ->
    0;
find_dow(TodayDOW, [DOW|_], _FirstDOW) when TodayDOW < DOW ->
    DOW-TodayDOW;
find_dow(TodayDOW, [DOW|T],  FirstDOW) when TodayDOW > DOW ->
    find_dow(TodayDOW, T, FirstDOW);
find_dow(TodayDOW, [], FirstDOW) ->
    FirstDOW+7 - TodayDOW.

set_internal_time(State, RefDate, CurEpochMsec) ->
    EpochS = to_seconds(CurEpochMsec),
    FloorE = to_milliseconds(EpochS),
    Msec   = to_milliseconds(erlang:universaltime_to_posixtime(RefDate)),
    State1 = State#state{ref_epoch=Msec, epoch_at_ref=FloorE},
    %Now    = Msec + CurEpochMsec - FloorE,
    State1#state{last_time=CurEpochMsec}.

%% @doc NewDate is in universal time.
fast_forward(#state{ref_epoch=OldRefEpoch, next_run=NextRun}=S, NewRefEpoch, NewDate, Count) ->
    {Msec, State1} = until_next_time(S#state{last_time=OldRefEpoch, epoch_at_ref=OldRefEpoch}),
    Span = NewRefEpoch - OldRefEpoch,
    if Span =< Msec; Msec =:= 0 ->
        true;
    true ->
        RefMsec = OldRefEpoch + Msec,
        State2  = State1#state{ref_epoch = RefMsec+1, last_run=NextRun, next_run=RefMsec},
        do_job_run(State2),
        case State2#state.job of
            {{once, _}, _} when Count =:= 1 ->
                Msec =< 0 andalso
                    ?LOG_WARNING([{info, "One-time job executed immediately due to negative time shift"},
                                {time_shift_secs, to_seconds(Msec)},
                                {job_ref,  State2#state.job_ref},
                                {job_when, State2#state.orig_when}]),
                false;
            _ ->
                fast_forward(State2, NewRefEpoch, NewDate, Count+1)
        end
    end.

to_local_datetime(Milliseconds) when is_integer(Milliseconds) ->
    DT = to_universal_datetime(Milliseconds),
    erlang:universaltime_to_localtime(DT).

to_universal_datetime(Milliseconds) ->
    erlang:posixtime_to_universaltime(to_seconds(Milliseconds)).

to_milliseconds(Secs) ->
    Secs * 1000.

to_seconds(MilliSec) ->
    MilliSec div 1000.

to_ceiling_seconds(Msec) ->
    S  = to_seconds(Msec),
    case to_milliseconds(S) of
        Msec -> S;
        _    -> S+1
    end.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(EUNIT).

dow_test() ->
    DOWs = resolve_dow([tue,wed,thu]),
    ?assertEqual([7],     resolve_dow(sun)),
    ?assertEqual([7],     resolve_dow([sun])),
    ?assertEqual([1,2],   resolve_dow([mon,tue])),
    ?assertEqual([2,3,4], DOWs),
    ?assertEqual(1,       find_dow(1, DOWs)),
    ?assertEqual(0,       find_dow(2, DOWs)),
    ?assertEqual(0,       find_dow(3, DOWs)),
    ?assertEqual(0,       find_dow(4, DOWs)),
    ?assertEqual(4,       find_dow(5, DOWs)),
    DOWs1 = resolve_dow([thu,sat]),
    ?assertEqual([4,6],   DOWs1),
    ?assertEqual(3,       find_dow(1, DOWs1)),
    ?assertEqual(2,       find_dow(2, DOWs1)),
    ?assertEqual(1,       find_dow(3, DOWs1)),
    ?assertEqual(0,       find_dow(4, DOWs1)),
    ?assertEqual(1,       find_dow(5, DOWs1)),
    ?assertEqual(0,       find_dow(6, DOWs1)),
    ?assertEqual(4,       find_dow(7, DOWs1)),
    ok.

normalize_test() ->
    Date = {date(), {0,0,0}},
    ?assertEqual({once,{relative,1000}},            normalize({once, 1}, Date)),
    ?assertEqual({once,{absolute,43200000}},        normalize({once, {12,pm}}, Date)),
    ?assertThrow({time_given_in_the_past,{2,3,4}},  normalize({once, {2,3,4}}, {date(), {2,3,5}})),
    ?assertEqual({once,{absolute,7384000}},         normalize({once, {2,3,4}}, Date)),
    ?assertEqual({daily,[{50400000,50400000,0}]},   normalize({daily, {2,pm}}, Date)),
    ?assertEqual({daily,[{0,86399000,20000}]},      normalize({daily, {every, {20,s}}}, Date)),
    ?assertEqual({daily,[{0,86399000,200}]},        normalize({daily, {every, {200,ms}}}, Date)),
    ?assertEqual({daily,[{0,86399000,2000}]},       normalize({daily, {every, {2000,millisecond}}}, Date)),
    ?assertEqual({daily,[{3600000,3600000,0},{7200000,7200000,0}]},
                 normalize({daily, [{2,am},{1,am}]}, Date)),
    ?assertEqual({daily,[{0,86399000,5000},{0,86399000,20000}]},
                 normalize({daily, [{every, {5,s}}, {every, {20,s}}]}, Date)),
    ?assertEqual({daily,[{3600000,7200000,20000}]},
                 normalize({daily, {every, {20,s}, {between, {1,am}, {2,am}}}}, Date)),
    ?assertEqual({weekly,[1],  [{3600000,3600000,0}]}, normalize({weekly, mon,       {1,am}}, Date)),
    ?assertEqual({weekly,[1,3],[{3600000,3600000,0}]}, normalize({weekly, [mon,wed], {1,am}}, Date)),
    ?assertEqual({weekly,[1,3],[{3600000,3600000,0}]}, normalize({weekly, [wed,mon], {1,am}}, Date)),
    ?assertEqual({monthly,{{[31],2019,2,28},{2019,1, [],[0]}},[{3600000,3600000,0}]},   normalize({monthly,   0, {1,am}}, {{2019,1,1},{0,0,0}})),
    ?assertEqual({monthly,{{[31],2020,1,31},{2019,12,[],[0]}},[{3600000,3600000,0}]},   normalize({monthly, [0], {1,am}}, {{2019,12,1},{0,0,0}})),
    ?assertEqual({monthly,{{[],  2019,3,31},{2019,2,[31],[]}},[{3600000,3600000,0}]},   normalize({monthly,[31], {1,am}}, {{2019,2,1},{0,0,0}})),
    ?assertEqual({monthly,{{[1, 31],2020,1,1}, {2019,12,[1],[0]}},[{3600000,3600000,0}]},normalize({monthly,[1,0],{1,am}},{{2019,12,1},{0,0,0}})),
    ?assertEqual({monthly,{{[30,31],2020,1,30},{2019,12,[30,31],[]}},[{3600000,3600000,0}]},normalize({monthly,[30,31],{1,am}},{{2019,12,1},{0,0,0}})),
    ?assertEqual({monthly,{{[31],2019,3,31},{2019,1,[31],[]}},[{3600000,3600000,0}]}, normalize({monthly,  31, {1,am}},    {{2019,1,1},{0,0,0}})),
    ?assertEqual({monthly,{{[29],2024,2,29},{2024,1,[29],[]}},[{3600000,3600000,0}]}, normalize({monthly,  29, {1,am}},    {{2024,1,1},{0,0,0}})),
    ?assertEqual({monthly,{{[31],2020,1,31},{2019,12,[31],[]}},[{3600000,3600000,0}]}, normalize({monthly,  31, {1,am}},   {{2019,12,1},{0,0,0}})),
    ?assertEqual({monthly,{{[1], 2019,2,1}, {2019,1,[1],[-30]}},[{3600000,3600000,0}]}, normalize({monthly,[1,-30],{1,am}},{{2019,1,1},{0,0,0}})),
    ?assertEqual({monthly,{{[1], 2019,3,1}, {2019,1,[],[-30]}}, [{3600000,3600000,0}]}, normalize({monthly,-30,{1,am}},    {{2019,1,1},{0,0,0}})),
    ?assertEqual({monthly,{{[30],2019,2,27},{2019,1,[],[-1]}},  [{0,86399000,20000}]},  normalize({monthly, -1, {every, {20,s}}}, {{2019,1,1},{0,0,0}})),
    ?assertEqual({monthly,{{[28],2019,2,25},{2019,1,[],[-3]}},  [{0,0,0},{3600000,3600000,0}]},
                 normalize({monthly, -3, [{12,am}, {1,am}]}, {{2019,1,1},{0,0,0}})),
    ok.

until_next_time_month_test() ->
    T    = ecrn_util:localtime_to_epoch({{2019,1,1},{0,0,0}}),
    Test = fun(Epoch, Days, When) ->
              I = ecrn_agent:until_next_time(Epoch, {monthly, Days, When}),
              ecrn_util:epoch_to_datetime_string(Epoch+I)
           end,
    ?assertEqual("2019-01-31 01:00:00.000", Test(T, [0],    {every, {20,s}, {between, {1,am}, {2,am}}})),
    ?assertEqual("2019-01-10 01:00:00.000", Test(T, [0,10], {every, {20,s}, {between, {1,am}, {2,am}}})),
    ?assertEqual("2019-01-03 00:00:00.000", Test(T, 3,      {every, {20,s}})),
    ok.

-endif.
