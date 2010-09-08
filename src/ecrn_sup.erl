%%%----------------------------------------------------------------
%%% @author  Eric Newhuis <enewhuis@ecdmarket.com>
%%% @doc
%%% @end
%%% @copyright 2009 eCD Market
%%%----------------------------------------------------------------
-module(ecrn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("erlcron-internal.hrl").

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
    RestartStrategy = one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy,
		MaxRestarts,
		MaxSecondsBetweenRestarts},

    ChildSup =  {ecrn_cron_sup, {ecrn_cron_sup, start_link, []},
		 permanent, 1000, supervisor, [ecrn_cron_sup]},
    RegistrationServer  =  {ecrn_reg_server, {ecrn_reg, start_link, []},
			    permanent, 1000, worker, [ecrn_reg]},
    BroadcastServer  =  {ecrn_control, {ecrn_control, start_link, []},
			 permanent, 1000, worker, [ecrn_control]},


    {ok, {SupFlags, [ChildSup, RegistrationServer, BroadcastServer]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
