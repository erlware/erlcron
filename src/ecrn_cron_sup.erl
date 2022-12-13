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
         add_job/2,
         all_jobs/0,
         terminate/1]).

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
%% Add a chron job to be supervised
-spec add_job(erlcron:job_ref(), erlcron:job()) -> erlcron:job_ref().
add_job(JobRef, Job = {_, Task}) when is_function(Task, 2)
                                    ; is_function(Task, 0)
                                    ; is_tuple(Task), tuple_size(Task)==3
                                      , is_atom(element(1,Task))
                                      , is_atom(element(2,Task))
                                      , is_list(element(3,Task)) ->
    case supervisor:start_child(?SERVER, [JobRef, Job]) of
        {ok, _}                       -> JobRef;
        {error, {already_started, _}} -> {error, already_started};
        Other                         -> Other
    end.

%% @doc
%% Get a list of all active jobs
-spec all_jobs() -> [pid()].
all_jobs() ->
    [P || {_,P,_,_} <- supervisor:which_children(?SERVER)].

%% @doc
%% Terminate a job
terminate(Pid) when is_pid(Pid) ->
    supervisor:terminate_child(?SERVER, Pid).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init([]) ->
    SupFlags = #{
        strategy  => simple_one_for_one,
        intensity => application:get_env(erlcron, sup_job_intensity, 3),
        period    => application:get_env(erlcron, sup_job_period,   10)
    },

    AChild   = #{id       => ecrn_agent,
                 start    => {ecrn_agent, start_link, []},
                 restart  => transient,
                 shutdown => brutal_kill,
                 type     => worker,
                 modules  => [ecrn_agent]},

    {ok, {SupFlags, [AChild]}}.
