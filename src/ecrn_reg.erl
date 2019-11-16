%%% @copyright Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the BSD License; you may not use
%%% this file except in compliance with the License.
%%%-------------------------------------------------------------------
%%% @doc
%%%   This provides simple pid registration for the server.
-module(ecrn_reg).

-behaviour(gen_server).

%% API
-export([start_link/0,
         register/2,
         unregister/1,
         get/1,
         get_refs/1,
         cancel/1,
         stop/0,
         get_all/0,
         get_all_pids/0,
         get_all_refs/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {pid2ref, ref2pid}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, Pid::pid()} | ignore | {error, Error::term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc
%%   Register an arbitrary value with the system, under a set of keys
-spec register(erlcron:job_ref(), term()) -> boolean().
register(Key, Pid) when (is_atom(Key) orelse is_reference(Key)), is_pid(Pid) ->
    gen_server:call(?SERVER, {register, Key, Pid}).

%% @doc
%%   Remove the value registered under a que or set of keys
-spec unregister(erlcron:job_ref()) -> ok.
unregister(Key) when is_atom(Key); is_reference(Key) ->
    gen_server:cast(?SERVER, {unregister, Key}).

%% @doc
%% Get a pid by reference key.
-spec get(erlcron:job_ref()) -> pid() | undefined.
get(Key) when is_atom(Key); is_reference(Key) ->
    gen_server:call(?SERVER, {get, Key}).

%% @doc
%% Get job refs associated with the pid
-spec get_refs(pid()) -> [erlcron:job_ref()].
get_refs(Pid) when is_pid(Pid) ->
    lists:sort(gen_server:call(?SERVER, {get_refs, Pid})).

%% @doc
%% Cancel all jobs assigned to the given key
-spec cancel(term()) -> boolean().
cancel(Key) when is_atom(Key); is_reference(Key) ->
    gen_server:call(?SERVER, {cancel, Key}).

%% @doc
%%  Get all the values.
-spec get_all() -> [{term(), term()}].
get_all() ->
    gen_server:call(?SERVER, get_all).

%% @doc
%%  Get all registered Pids.
-spec get_all_pids() -> [pid()].
get_all_pids() ->
    gen_server:call(?SERVER, get_all_pids).

%% @doc
%%  Get all registered job references.
-spec get_all_refs() -> [erlcron:job_ref()].
get_all_refs() ->
    gen_server:call(?SERVER, get_all_refs).

%% @doc
%%  stop this server
-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    {ok, #state{pid2ref=#{}, ref2pid=#{}}}.

%% @private
handle_call({register, Ref, Pid}, _From, #state{pid2ref=M1, ref2pid=M2}=State) ->
    Ref1 = monitor(process, Pid),
    case maps:find(Pid, M1) of
        {ok, Refs} when is_list(Refs) ->
            demonitor(Ref1, [flush]),
            case lists:member(Ref, Refs) of
                true ->
                    {reply, false, State};
                false ->
                    {reply, true,  State#state{ref2pid=M2#{Ref=>Pid}, pid2ref=M1#{Pid=>[Ref|Refs]}}}
            end;
        error ->
            {reply, true, State#state{ref2pid=M2#{Ref=>Pid}, pid2ref=M1#{Pid=>[Ref]}}}
    end;
handle_call({get, Key}, _From, State) ->
    case get_for_key(Key, State) of
        {ok, V}   -> {reply, V,         State};
        undefined -> {reply, undefined, State}
    end;
handle_call({get_refs, Pid}, _From, State) ->
    case get_for_key(Pid, State) of
        {ok, V}   -> {reply, V,  State};
        undefined -> {reply, [], State}
    end;
handle_call({cancel, Pid}, _From, State) ->
    {Found, State1} = find_and_remove(Pid, State, fun(P) -> ecrn_agent:cancel(P) end),
    {reply, Found, State1};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(get_all, _From, State = #state{ref2pid=Map}) ->
    {reply, maps:to_list(Map), State};
handle_call(get_all_refs, _From, State = #state{ref2pid=Map}) ->
    {reply, maps:keys(Map), State};
handle_call(get_all_pids, _From, State = #state{pid2ref=Map}) ->
    {reply, maps:keys(Map), State}.

%% @private
handle_cast({unregister, Key}, State) ->
    {_, NewState} = find_and_remove(Key, State, undefined),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', _Ref, process, Pid, _Info}, State) ->
    handle_cast({unregister, Pid}, State);
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
get_for_key(Ref, #state{ref2pid=Map}) when is_reference(Ref); is_atom(Ref) ->
    case maps:find(Ref, Map) of
        error   -> undefined;
        OkValue -> OkValue
    end;
get_for_key(Pid, #state{pid2ref=Map}) when is_pid(Pid) ->
    case maps:find(Pid, Map) of
        error   -> undefined;
        OkValue -> OkValue
    end.

-spec find_and_remove(reference()|atom()|pid(), #state{}, undefined|fun((pid())->ok)) ->
        {boolean(), #state{}}.
find_and_remove(Ref, S = #state{ref2pid=M1}, Fun) when is_reference(Ref); is_atom(Ref) ->
    case maps:find(Ref, M1) of
        {ok, Pid} ->
            is_function(Fun, 1) andalso Fun(Pid),
            {true, find_and_remove2(Pid, Ref, S)};
        error   ->
            {false, S}
    end;
find_and_remove(Pid, State = #state{ref2pid=M1, pid2ref=M2}, Fun) when is_pid(Pid) ->
    case maps:find(Pid, M2) of
        {ok, Refs} ->
            is_function(Fun, 1) andalso Fun(Pid),
            NewM1 = lists:foldl(fun(R, M) -> maps:remove(R, M) end, M1, Refs),
            {true, State#state{ref2pid=NewM1, pid2ref=maps:remove(Pid,M2)}};
        error   ->
            {false, State}
    end.

find_and_remove2(Pid, Ref, S = #state{ref2pid=M1, pid2ref=M2}) when is_pid(Pid) ->
    case maps:find(Pid, M2) of
        {ok, Refs} ->
            case lists:delete(Ref, Refs) of
                [] ->
                    S#state{ref2pid=maps:remove(Ref,M1), pid2ref=maps:remove(Pid,M2)};
                L  ->
                    S#state{ref2pid=maps:remove(Ref,M1), pid2ref=M2#{Pid => L}}
            end;
        error ->
            S#state{ref2pid=maps:remove(Ref,M1)}
    end.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(EUNIT).

generate_test_() ->
    {setup,
     fun ()  -> ecrn_reg:start_link() end,
     fun (_) -> ecrn_reg:stop() end,
     {with,
      [fun general_tests/1]}}.

general_tests(_) ->
    Self = self(),
    ?assertMatch(true,       ecrn_reg:register(a, Self)),
    ?assertMatch(Self,       ecrn_reg:get(a)),
    ecrn_reg:unregister(a),
    Ref = make_ref(),
    ?assertMatch(undefined,  ecrn_reg:get(a)),
    ?assertMatch(true,       ecrn_reg:register(b, Self)),
    ?assertMatch(true,       ecrn_reg:register(c, Self)),
    ?assertMatch(true,       ecrn_reg:register(Ref, Self)),
    ?assertMatch(Self,       ecrn_reg:get(c)),
    ?assertMatch(Self,       ecrn_reg:get(b)),
    ?assertMatch(Self,       ecrn_reg:get(Ref)),
    ?assertMatch([b,c,Ref],  ecrn_reg:get_refs(Self)),
    ?assertMatch(false,      ecrn_reg:register(b,   Self)),
    ?assertMatch(false,      ecrn_reg:register(c,   Self)),
    ?assertMatch(false,      ecrn_reg:register(Ref, Self)),
    ?assertMatch([b,c,Ref],  ecrn_reg:get_all_refs()),
    ?assertMatch([Self],     ecrn_reg:get_all_pids()),
    ecrn_reg:unregister(b),
    ?assertMatch([c,Ref],    ecrn_reg:get_all_refs()),
    ?assertMatch([Self],     ecrn_reg:get_all_pids()),
    ok.

-endif.
