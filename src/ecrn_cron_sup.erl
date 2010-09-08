%%%-------------------------------------------------------------------
%%% @author Eric Merritt <emerritt@ecdmarket.com>
%%% @doc
%%%   Simple one for one supervisor for ecd_chron jobs.
%%% @end
%%%-------------------------------------------------------------------
-module(ecrn_cron_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
	 add_job/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%--------------------------------------------------------------------
%% @doc
%%  Add a chron job to be supervised
%% @end
%%--------------------------------------------------------------------

add_job(AlarmRef, Task) ->
    supervisor:start_child(?SERVER, [AlarmRef, Task]).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient,
    Shutdown = 2000,
    Type = worker,

    AChild = {ecrn_agent, {ecrn_agent, start_link, []},
	      Restart, Shutdown, Type, [ecrn_agent]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
