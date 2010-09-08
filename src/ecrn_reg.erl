%%%-------------------------------------------------------------------
%%% @author Eric Merritt <emerritt@ecdmarket.com>
%%% @doc
%%%   This provides simple pid registration for the server.
%%% @end
%%%-------------------------------------------------------------------
-module(ecrn_reg).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 register/2,
	 unregister/1,
	 get/1,
	 stop/0,
	 get_all/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-define(SERVER, ?MODULE).

-record(state, {registered}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%%   Register an arbitrary value with the system, under a set of keys
%% @end
%%--------------------------------------------------------------------
-spec register(term() | [term()], term()) -> ok.
register(Keys, Body) when is_list(Keys) ->
    gen_server:call(?SERVER, {register, Keys, Body});
register(Key, Body)  ->
    gen_server:call(?SERVER, {register, [Key], Body}).

%%--------------------------------------------------------------------
%% @doc
%%   Remove the value registered under a que or set of keys
%% @end
%%--------------------------------------------------------------------
-spec unregister(term() | [term()]) -> ok.
unregister(Keys) when is_list(Keys) ->
    gen_server:call(?SERVER, {unregister, Keys});
unregister(Key) ->
    gen_server:call(?SERVER, {unregister, [Key]}).

%%--------------------------------------------------------------------
%% @doc
%%  Get a value buy key.
%% @end
%%--------------------------------------------------------------------
-spec get(term()) -> {ok, term()} | undefined.
get(Key) ->
    gen_server:call(?SERVER, {get, Key}).


%%--------------------------------------------------------------------
%% @doc
%%  Get all the values.
%% @end
%%--------------------------------------------------------------------
-spec get_all() -> [{term(), term()}].
get_all() ->
    gen_server:call(?SERVER, get_all).


%%--------------------------------------------------------------------
%% @doc
%%  stop this server
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{registered=dict:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%          <                         {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({register, Keys, Body}, _From, #state{registered=Dict}) ->
    {Result, Dict2} = add_for_keys(Keys, Body, Dict, ok),
    {reply, Result,  #state{registered=Dict2}};
handle_call({unregister, Keys}, _From, #state{registered=Dict}) ->
    Dict2 = remove_for_keys(Keys, Dict),
    {reply, ok, #state{registered=Dict2}};
handle_call({get, Key}, _From, State = #state{registered=Dict}) ->
    Value = get_for_key(Key, Dict),
    {reply, Value, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(get_all, _From, State = #state{registered=Dict}) ->
    {reply, dict:to_list(Dict), State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
discarded_keys(Key, {discarded_keys, Keys}) ->
    {discarded_keys, [Key | Keys]};
discarded_keys(Key, ok) ->
    {discarded_keys, [Key]}.

add_for_keys([Key | Rest], Body, Dict, Result) ->
    case dict:find(Key, Dict) of
	error ->
	    add_for_keys(Rest, Body, dict:append(Key, Body, Dict), Result);
	_Value ->
	    add_for_keys(Rest, Body, dict:append(Key, Body, Dict),
			 discarded_keys(Key, Result))
    end;
add_for_keys([], _, Dict, Result) ->
    {Result, Dict}.

remove_for_keys([Key | Rest], Dict) ->
    remove_for_keys(Rest, dict:erase(Key, Dict));
remove_for_keys([],Dict) ->
    Dict.

get_for_key(Key, Dict) ->
    case dict:find(Key, Dict) of
	error ->
	    undefined;
	Value ->
	    Value
    end.

%%%===================================================================
%%% Tests
%%%===================================================================

generate_test_() ->
    {setup,
     fun () ->
	     ecrn_reg:start_link()
     end,
     fun (_) ->
	     ecrn_reg:stop()
     end,
     {with,
      [fun general_tests/1]}}.

general_tests(_) ->
    ?assertMatch(ok, ecrn_reg:register([a, b], boo)),
    ?assertMatch({ok, [boo]}, ecrn_reg:get(a)),
    ecrn_reg:unregister([a, b]),
    ?assertMatch(undefined, ecrn_reg:get(a)),
    ?assertMatch(ok, ecrn_reg:register([a, b], boo)),
    ?assertMatch(ok, ecrn_reg:register([c, d], boo2)),
    ?assertMatch({ok, [boo2]}, ecrn_reg:get(c)),
    ?assertMatch({ok, [boo]}, ecrn_reg:get(b)),
    ?assertMatch({discarded_keys, [d, c]},
     		 ecrn_reg:register([c, d], boo2)).








