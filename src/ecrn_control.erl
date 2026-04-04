%%% @copyright Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the BSD License; you may not use
%%% this file except in compliance with the License.
%%%-------------------------------------------------------------------
-module(ecrn_control).
-behaviour(gen_server).

-moduledoc """
Global clock controller for the erlcron scheduler.

`ecrn_control` is a singleton gen_server that maintains the reference
datetime used by all job agents.  During normal operation the reference
time tracks the real system clock.  For testing, the clock can be
advanced to any future point via `set_datetime/2`; every `ecrn_agent`
process will then fast-forward, executing any jobs whose scheduled time
falls within the elapsed interval before settling at the new reference
time.

The server also provides the `cancel/1` entry point that delegates to
`ecrn_reg` and is exposed as `erlcron:cancel/1`.
""".

%% API
-export([start_link/0,
         cancel/1,
         datetime/0,
         datetime/1,
         ref_datetime/0,
         ref_datetime/1,
         set_datetime/1,
         set_datetime/2,
         reset_datetime/0,
         multi_set_datetime/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("internal.hrl").

-record(state, {ref_time :: calendar:datetime(),
                epoch_at_ref :: erlcron:seconds()}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, Error::term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec cancel(erlcron:job_ref()) -> boolean().
cancel(AlarmRef) ->
    ecrn_reg:cancel(AlarmRef).

-doc "Return the current datetime (universal) with the reference-clock adjustment.".
-spec datetime() -> {calendar:datetime(), erlcron:milliseconds()}.
datetime() ->
    gen_server:call(?SERVER, datetime).

-doc "Return the current datetime in universal or local time with the reference-clock adjustment.".
-spec datetime(local|universal) -> {calendar:datetime(), erlcron:milliseconds()}.
datetime(universal) ->
    datetime();
datetime(local) ->
    {DT, Epoch} = datetime(),
    {erlang:universaltime_to_localtime(DT), Epoch}.

-doc "Return the reference datetime (universal) used as the scheduler's clock base.".
-spec ref_datetime() -> {calendar:datetime(), erlcron:milliseconds()}.
ref_datetime() ->
    gen_server:call(?SERVER, ref_datetime).

-doc "Return the reference datetime in universal or local time.".
-spec ref_datetime(local|universal) -> {calendar:datetime(), erlcron:milliseconds()}.
ref_datetime(universal) ->
    ref_datetime();
ref_datetime(local) ->
    {DT, Epoch} = ref_datetime(),
    {erlang:universaltime_to_localtime(DT), Epoch}.

-doc "Set the scheduler clock to `DateTime` (local time).".
-spec set_datetime(calendar:datetime()) -> ok | {error, term()}.
set_datetime(DateTime) ->
    set_datetime(DateTime, local).

-spec set_datetime(calendar:datetime(), local|universal) -> ok | {error, term()}.
set_datetime(DT={_,_}, local) ->
    gen_server:call(?SERVER, {set_datetime, erlang:localtime_to_universaltime(DT)}, infinity);
set_datetime(DateTime={_,_}, universal) ->
    gen_server:call(?SERVER, {set_datetime, DateTime}, infinity).

-doc "Reset the scheduler clock to the current real system time.".
-spec reset_datetime() -> ok | {error, term()}.
reset_datetime() ->
    gen_server:call(?SERVER, reset_datetime, infinity).

-doc "Set the scheduler clock to `DateTime` on the given nodes.".
-spec multi_set_datetime([node()], calendar:datetime()) -> {Replies, BadNodes} when
    Replies :: [{node(), ok | {error, term()}}],
    BadNodes :: [node()].
multi_set_datetime(Nodes, DateTime={_,_}) ->
    gen_server:multi_call(Nodes, ?SERVER, {set_datetime, DateTime}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    DateTime = erlang:universaltime(),
    {ok, #state{ref_time=DateTime,
                epoch_at_ref=ecrn_util:epoch_milliseconds()}}.

%% @private
handle_call(datetime, _From, State = #state{ref_time = DateTime,
                                            epoch_at_ref = Actual}) ->
    DT     = erlang:universaltime_to_posixtime(DateTime),
    Now    = ecrn_util:epoch_milliseconds(),
    Diff   = Now - Actual,
    DiffS  = to_seconds(Diff),
    RefNow = DT   + DiffS,
    Msecs  = Diff - DiffS*1000,
    NowDT  = erlang:posixtime_to_universaltime(RefNow),
    {reply, {NowDT, RefNow*1000 + Msecs}, State};
handle_call(ref_datetime, _From, State = #state{ref_time = DateTime,
                                                epoch_at_ref = Actual}) ->
    {reply, {DateTime, Actual}, State};
handle_call({set_datetime, DateTime}, _From, State) ->
    NewState = State#state{ref_time=DateTime,
                           epoch_at_ref=ecrn_util:epoch_milliseconds()},
    {reply, call_all(NewState), NewState};

handle_call(reset_datetime, _From, State) ->
    Now      = ecrn_util:epoch_milliseconds(),
    DateTime = erlang:posixtime_to_universaltime(to_seconds(Now)),
    NewState = State#state{ref_time=DateTime, epoch_at_ref=Now},
    {reply, call_all(NewState), NewState}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%notify_all(#state{ref_time=DateTime, epoch_at_ref=Now}=State) ->
%    [ecrn_agent:set_datetime(P, DateTime, Now, universal) || P <- ecrn_reg:get_all_pids()],
%    State.

call_all(#state{ref_time=DateTime, epoch_at_ref=Now}) ->
    Res = lists:foldl(fun(P, A) ->
        case ecrn_agent:set_datetime(P, DateTime, Now, universal) of
            ok  ->
                A;
            {error, Err} ->
                Ref = try
                          ecrn_reg:get_refs(P)
                      catch _:_ ->
                          []
                      end,
                [{Ref, P, Err} | A]
        end
    end, [], ecrn_reg:get_all_pids()),
    case Res of
        [] -> ok;
        _  -> {error, {failed_to_set_time, Res}}
    end.

to_seconds(MilliSec) ->
    MilliSec div 1000.
