%%% @copyright Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the BSD License; you may not use
%%% this file except in compliance with the License.
%%%-------------------------------------------------------------------
%%% @doc
%%%   Simple one for one supervisor for ecd_chron jobs.
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

-spec start_link() -> {ok, pid()} | ignore | {error, Error::term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%% @doc
%%  Add a chron job to be supervised
-spec add_job(erlcron:job_ref(), erlcron:job()) -> erlcron:job_ref().
add_job(JobRef, Task) ->
    {ok, _} = supervisor:start_child(?SERVER, [JobRef, Task]),
    JobRef.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
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
