%%% @copyright Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the BSD License; you may not use
%%% this file except in compliance with the License.
%%%-------------------------------------------------------------------
-module(ecrn_cron_sup).
-behaviour(supervisor).

-moduledoc """
Simple-one-for-one supervisor that owns all running cron job processes.

Each call to `add_job/3` starts one `ecrn_agent` child under this
supervisor.  Jobs are transient: a job that finishes normally (e.g. a
`once` job after it has run) is not restarted.  A job that crashes
unexpectedly is restarted up to the intensity limit configured by the
`sup_job_intensity` and `sup_job_period` application environment
variables (defaults: 3 restarts in 10 seconds).
""".

%% API
-export([start_link/0,
         add_job/1,
         add_job/2,
         add_job/3,
         all_jobs/0,
         terminate/1,
         check_task/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, Error::term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-doc "Add a cron job under supervision (using default cron options).".
-spec add_job(map()) -> erlcron:job_ref().
add_job(Job = #{id := JobRef}) ->
    add_job(JobRef, Job, #{}).

-doc "Add a cron job under supervision (using default cron options).".
-spec add_job(erlcron:job_ref(), erlcron:job()) -> erlcron:job_ref().
add_job(JobRef, Job) ->
    add_job(JobRef, Job, #{}).

-doc """
Add a cron job under supervision with the given options.

Job-level options in `Job` (when it is a map or tuple-with-opts) are
merged with `CronOpts`, with job-level options taking precedence.
Returns `ignored` when the job is excluded by hostname restrictions.
""".
-spec add_job(erlcron:job_ref(), erlcron:job(), erlcron:cron_opts()) ->
    erlcron:job_ref() | ignored | already_started | {error, term()}.
add_job(JobRef, Job = #{}, CronOpts) when is_map(CronOpts) ->
    {When, Task, JobOpts} = parse_job(Job),
    add_job2(JobRef, When, Task, check_opts(JobRef, maps:merge(CronOpts, JobOpts)));
add_job(JobRef, {When, Task}, CronOpts) when is_map(CronOpts) ->
    add_job2(JobRef, When, Task, check_opts(JobRef, CronOpts));
add_job(JobRef, {When, Task, JobOpts}, CronOpts) when is_map(JobOpts) ->
    add_job2(JobRef, When, Task, check_opts(JobRef, maps:merge(CronOpts, JobOpts))).

add_job2(JobRef, Schedule, Task, Opts) when is_reference(JobRef); is_atom(JobRef); is_binary(JobRef) ->
    maybe
        true        ?= check_host(Opts),
        {ok, Sched} ?= ecrn_util:parse_schedule(Schedule),
        Fun          = check_task(JobRef, Task),
        {ok, _}     ?= supervisor:start_child(?SERVER, [JobRef, {Sched, Task}, Fun, Opts]),
        JobRef
    else
        false                         -> ignored;
        {error, {already_started, _}} -> already_started;
        {error, _} = Err              -> Err
    end.

get_opt(Opt, Map) when is_atom(Opt) ->
    case maps:take(Opt, Map) of
        {V, Map1} -> {V, Map1};
        error     -> erlang:error({missing_job_option, Opt, Map})
    end;

get_opt([Opt], Map) ->
    get_opt(Opt, Map);
get_opt([Opt|T], Map) ->
    case maps:take(Opt, Map) of
        {V, Map1} -> {V, Map1};
        error     -> get_opt(T, Map)
    end.

parse_job(Job) ->
    %% Support 'interval' and 'execute' for backward compatibility,
    %% but prefer 'schedule' and 'task' as more descriptive option names.
    {When, Opts1} = get_opt([schedule, interval], Job),
    {Fun,  Opts2} = get_opt([task,     execute],  Opts1),
    {When, Fun, Opts2}.

-doc "Return PIDs of all currently supervised job processes.".
-spec all_jobs() -> [pid()].
all_jobs() ->
    [P || {_,P,_,_} <- supervisor:which_children(?SERVER)].

-doc "Terminate the job process identified by `Pid`.".
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
            Info =
                if is_function(V) ->
                    [Name, Arity, Mod, Env0] =
                        [element(2, erlang:fun_info(V, I)) || I <- [name, arity, module, env]],
                    Fun = lists:flatten(io_lib:format("~w/~w", [Name, Arity])),
                    case Env0 of
                        [T|_] when is_tuple(T) ->
                            {Mod, element(1,T), Fun}; %% {Module, {Line, Pos}, Fun}
                        _ ->
                            {Mod, Fun}
                    end;
                true ->
                    V
                end,
            erlang:error({invalid_option_value, JobRef, {K, Info}})
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

check_task(JobRef, {M, F}) when is_atom(M), is_atom(F) ->
    check_exists(JobRef, {M, F, [0,2]}, callback);
check_task(JobRef, {M, F, []}) when is_atom(M), is_atom(F) ->
    check_exists(JobRef, {M, F, [0]}, callback);
check_task(JobRef, {M, F, A}) when is_atom(M), is_atom(F), is_list(A) ->
    check_exists(JobRef, {M, F, [length(A)]}, A);
check_task(_, Task) when is_function(Task, 0) ->
    Task;
check_task(_, Task) when is_function(Task, 2) ->
    Task;
check_task(JobRef, Task) ->
    erlang:error({invalid_job_task, JobRef, Task}).

check_exists(JobRef, {M,F,Arities} = Task, Args) ->
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
    check_arity(JobRef, M, F, Arities, Args).

check_arity(JobRef, M, F, Arities, Args) ->
    {module, M} == code:ensure_loaded(M)
        orelse erlang:error({job_task_module_not_loaded, JobRef, M}),
    case lists:dropwhile(fun(Arity) -> not erlang:function_exported(M,F,Arity) end, Arities) of
        [A|_] when Args == callback; Args == [] -> fun M:F/A;
        [_|_] when is_list(Args)                -> fun() -> apply(M, F, Args) end;
        [] -> erlang:error({wrong_arity_of_job_task, JobRef, report_arity(M,F,Arities)})
    end.

report_arity(M, F, [A]) ->
    lists:flatten(io_lib:format("~w:~w/~w", [M, F, A]));
report_arity(M, F, A) when is_list(A) ->
    Arities = string:join([integer_to_list(I) || I <- A], ","),
    lists:flatten(io_lib:format("~w:~w/[~s]", [M, F, Arities])).

to_list(H) when is_binary(H) -> binary_to_list(H);
to_list(H) when is_list(H)   -> H.
