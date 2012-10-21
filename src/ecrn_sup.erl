%%% @copyright Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the BSD License; you may not use
%%% this file except in compliance with the License.
-module(ecrn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, Pid::term()} | ignore | {error, Error::term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
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
