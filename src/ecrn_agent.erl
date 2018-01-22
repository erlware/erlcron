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
-export([ start_link/2
        , cancel/1
        , get_datetime/1
        , set_datetime/3
        , recalculate/1
        , validate/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-include("internal.hrl").

-record(state, { job :: undefined | erlcron:job()
               , alarm_ref :: undefined | reference()
               , referenced_seconds :: undefined | erlcron:seconds()
               , seconds_at_reference :: undefined | erlcron:seconds()
               , timeout_type :: undefined | normal | wait_before_run
               , fast_forward = false :: boolean()
               }).

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
-spec start_link(erlcron:job_ref(), erlcron:job()) ->
                        ignore | {error, Reason::term()} | {ok, pid()}.
start_link(JobRef, Job) ->
    gen_server:start_link(?MODULE, [JobRef, Job], []).

-spec get_datetime(pid()) -> calendar:datetime().
get_datetime(Pid) ->
    gen_server:call(Pid, get_datetime).

-spec cancel (pid()) -> ok.
cancel(Pid) ->
    gen_server:cast(Pid, shutdown).

-spec set_datetime(pid(), calendar:datetime(), erlcron:seconds()) -> ok.
set_datetime(Pid, DateTime, Actual) ->
    gen_server:cast(Pid, {set_datetime, DateTime, Actual}).

-spec recalculate(pid()) -> ok.
recalculate(Pid) ->
    gen_server:cast(Pid, recalculate).

%% @doc
%%  Validate that a run_when spec specified is correct.
-spec validate(erlcron:run_when()) -> valid | invalid.
validate(Spec) ->
    {DateTime, _} = ecrn_control:datetime(),
    try
        ecrn_time:until_next_time(DateTime, Spec),
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

-spec current_date(state()) -> calendar:datetime().
current_date(State = #state{fast_forward=true}) ->
    calendar:gregorian_seconds_to_datetime(State#state.referenced_seconds);
current_date(State) ->
    Elapsed = ecrn_util:epoch_seconds() - State#state.seconds_at_reference,
    calendar:gregorian_seconds_to_datetime(ceiling(Elapsed +
                                                 State#state.referenced_seconds)).

%% @doc Calculates the duration in milliseconds until the next time
%% a job is to be run.
-spec until_next_milliseconds(state(), erlcron:job()) ->
                                      {ok, erlcron:seconds()} | {error, invalid_one_exception}.
until_next_milliseconds(State, {When, _Job}) ->
    try
        Now = current_date(State),
        Millis = ecrn_time:until_next_time(Now, When) * ?MILLISECONDS,
        {ok, Millis}
    catch
        throw:invalid_once_exception ->
            {error, invalid_once_exception}
    end.

fast_forward(State, NewDate) ->
    try
        Now = current_date(State),
        {When, _} = State#state.job,
        Seconds = ecrn_time:until_next_time(Now, When),
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

