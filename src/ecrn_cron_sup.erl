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
         add_job/3,
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
%% Add a cron job to be supervised
-spec add_job(erlcron:job_ref(), erlcron:job()) -> erlcron:job_ref().
add_job(JobRef, Job) ->
    add_job(JobRef, Job, #{}).

%% @doc
%% Add a cron job to be supervised
-spec add_job(erlcron:job_ref(), erlcron:job(), erlcron:cron_opts()) ->
    erlcron:job_ref().
add_job(JobRef, Job = {_, _Task}, CronOpts) when is_map(CronOpts) ->
    add_job2(JobRef, Job, CronOpts);
add_job(JobRef, {When, Task, JobOpts}, CronOpts) when is_map(JobOpts) ->
    add_job2(JobRef, {When, Task}, maps:merge(CronOpts, JobOpts)).

add_job2(JobRef, Job = {_, Task}, Opts)
    when is_function(Task, 2)
       ; is_function(Task, 0)
       ; is_tuple(Task), tuple_size(Task)==3
         , is_atom(element(1,Task))
         , is_atom(element(2,Task))
         , is_list(element(3,Task)) ->
    case check_host(Opts) of
        true ->
            case supervisor:start_child(?SERVER, [JobRef, Job]) of
                {ok, _}                       -> JobRef;
                {error, {already_started, _}} -> {error, already_started};
                Other                         -> Other
            end;
        false ->
            false
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_host(Opts) ->
    case maps:find(hostnames, Opts) of
        {ok, Hosts} when is_list(Hosts) ->
            {ok, Host} = inet:gethostname(),
            lists:member(Host, [to_list(H) || H <- Hosts]);
        error ->
            true
    end.

to_list(H) when is_binary(H) -> binary_to_list(H);
to_list(H) when is_list(H)   -> H.
