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
    erlcron:job_ref() | ignored | already_started | {error, term()}.
add_job(JobRef, Job = {_, _Task}, CronOpts) when is_map(CronOpts) ->
    add_job2(JobRef, Job, check_opts(JobRef, CronOpts));
add_job(JobRef, {When, Task, JobOpts}, CronOpts) when is_map(JobOpts) ->
    add_job2(JobRef, {When, Task}, check_opts(JobRef, maps:merge(CronOpts, JobOpts))).

add_job2(JobRef, Job = {_, Task}, Opts) ->
    case check_host(Opts) of
        true ->
            check_task(JobRef, Task),
            case supervisor:start_child(?SERVER, [JobRef, Job, Opts]) of
                {ok, _}                       -> JobRef;
                {error, {already_started, _}} -> already_started;
                Other                         -> Other
            end;
        false ->
            ignored
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
check_opts(JobRef, Map) ->
    maps:foreach(fun
        (hostnames, L) when is_list(L) ->
            ok;
        (on_job_start, MF) when tuple_size(MF)==2; is_function(MF, 1) ->
            ok;
        (on_job_end, MF) when tuple_size(MF)==2; is_function(MF, 2) ->
            ok;
        (id, ID) when is_atom(ID); is_binary(ID); is_reference(ID) ->
            ok;
        (K, V) ->
            erlang:error({invalid_option_value, JobRef, {K, V}})
    end, Map),
    Map.

check_host(Opts) ->
    case maps:find(hostnames, Opts) of
        {ok, Hosts} when is_list(Hosts) ->
            {ok, Host} = inet:gethostname(),
            lists:member(Host, [to_list(H) || H <- Hosts]);
        error ->
            true
    end.

check_task(JobRef, Task) when is_tuple(Task), (tuple_size(Task)==2 orelse tuple_size(Task)==3) ->
    M = element(1, Task),
    case code:ensure_loaded(M) of
        {module, M} ->
            ok;
        {error, Err1} ->
            erlang:error({module_not_loaded, JobRef, Task, Err1})
    end,
    check_exists(JobRef, Task);
check_task(_, Task) when is_function(Task, 0) ->
    ok;
check_task(_, Task) when is_function(Task, 2) ->
    ok;
check_task(JobRef, Task) ->
    erlang:error({invalid_job_task, JobRef, Task}).

check_exists(JobRef, {M,F}) ->
    check_exists2(JobRef, {M,F,undefined});
check_exists(JobRef, {_,_,A} = MFA) when is_list(A) ->
    check_exists2(JobRef, MFA).

check_exists2(JobRef, {M,F,A} = Task) ->
    case erlang:module_loaded(M) of
        false ->
            case code:ensure_loaded(M) of
                {module, M} ->
                    ok;
                {error, Err1} ->
                    erlang:error({module_not_loaded, JobRef, Task, Err1})
            end;
        true ->
            ok
    end,
    case A of
        undefined ->
            check_arity(JobRef, M, F, [0,2]);
        _ when is_list(A) ->
            check_arity(JobRef, M, F, [length(A)])
    end.

check_arity(JobRef, M, F, Lengths) ->
    lists:any(fun(Arity) -> erlang:function_exported(M,F,Arity) end, Lengths)
        orelse erlang:error({wrong_arity_of_job_task, JobRef, {M,F,Lengths}}).

to_list(H) when is_binary(H) -> binary_to_list(H);
to_list(H) when is_list(H)   -> H.
