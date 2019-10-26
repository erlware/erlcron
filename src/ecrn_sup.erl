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
    SupFlags = #{
        strategy  => one_for_one,
        intensity => application:get_env(erlcron, sup_intensity, 3),
        period    => application:get_env(erlcron, sup_period,   10)
    },

    ChildSup     = #{id       => ecrn_cron_sup,
                     start    => {ecrn_cron_sup, start_link, []},
                     restart  => permanent,
                     shutdown => 1000,
                     type     => supervisor,
                     modules  => [ecrn_cron_sup]},

    RegServer    = #{id       => ecrn_reg,
                     start    => {ecrn_reg, start_link, []},
                     restart  => permanent,
                     shutdown => 1000,
                     type     => worker,
                     modules  => [ecrn_reg]},

    CtrlServer   = #{id       => ecrn_control,
                     start    => {ecrn_control, start_link, []},
                     restart  => permanent,
                     shutdown => 1000,
                     type     => worker,
                     modules  => [ecrn_control]},

    {ok, {SupFlags, [ChildSup, RegServer, CtrlServer]}}.
