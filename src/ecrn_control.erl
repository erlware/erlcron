%%%-------------------------------------------------------------------
%%% @author Eric Merritt <emerritt@ecdmarket.com>
%%% @doc
%%%  Provides testing/fast forward control for the system
%%% @end
%%%-------------------------------------------------------------------
-module(ecrn_control).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 cancel/1,
	 datetime/0,
	 set_datetime/1,
	 multi_set_datetime/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("erlcron-internal.hrl").

-record(state, {reference_datetime, datetime_at_reference}).

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

cancel(AlarmRef) ->
    gen_server:call(?SERVER, {cancel, AlarmRef}).

datetime() ->
    gen_server:call(?SERVER, get_datetime).

%% @doc sets the date-time for the erlcron
set_datetime(DateTime={_,_}) ->
    gen_server:call(?SERVER, {set_datetime, DateTime}, infinity).

%% @doc sets the date-time with the erlcron on all nodes
multi_set_datetime(Nodes, DateTime={_,_}) ->
    gen_server:multi_call(Nodes, ?SERVER, {set_datetime, DateTime}).

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
    DateTime = erlang:localtime(),
    {ok, #state{reference_datetime=DateTime, datetime_at_reference=?EPOC_SECONDS}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({cancel, AlarmRef}, _From, State) ->
    {reply, internal_cancel(AlarmRef), State};
handle_call(get_datetime, _From, State = #state{reference_datetime = DateTime,
						datetime_at_reference = Actual}) ->
    {reply, {DateTime, Actual}, State};
handle_call({set_datetime, DateTime}, _From, State) ->
    NewState = State#state{reference_datetime=DateTime,
			   datetime_at_reference=?EPOC_SECONDS},
    case lists:foldl(fun({_, [Pid]}, Acc) ->

			     ecrn_agent:set_datetime(Pid, DateTime,
						      NewState#state.datetime_at_reference),
			     Acc;
			({Ref, X}, Acc) when is_list(X) ->
			     [Ref | Acc]
		     end,
		     [],
		     ecrn_reg:get_all()) of
	[] ->
	    {reply, ok, NewState};
	ErrorRefs ->
	    {reply, {error, ErrorRefs}, NewState}
    end.

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
internal_cancel(AlarmRef) ->
    case ecrn_reg:get(AlarmRef) of
	undefined ->
	    undefined;
	{ok, [Pid]} ->
	    {ok, ecrn_agent:cancel(Pid)}
    end.
