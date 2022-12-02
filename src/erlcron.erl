%%% @copyright Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the BSD License; you may not use
%%% this file except in compliance with the License.
-module(erlcron).

-export([validate/1,
         cron/1,   cron/2,    cron/3,
         at/2,     at/3,      at/4,
         daily/2,  daily/3,   daily/4,
         weekly/3, weekly/4,  weekly/5,
         monthly/3,monthly/4, monthly/5,
         cancel/1,
         epoch/0,
         epoch_seconds/0,
         datetime/0,
         ref_datetime/0,
         set_datetime/1,
         set_datetime/2,
         reset_datetime/0,
         get_all_jobs/0,
         multi_set_datetime/1,
         multi_set_datetime/2]).

-export_type([job/0,
              job_ref/0,
              run_when/0,
              callable/0,
              dow/0,
              dom/0,
              period/0,
              duration/0,
              constraint/0,
              cron_time/0,
              seconds/0,
              milliseconds/0]).


%%%===================================================================
%%% Types
%%%===================================================================

-type seconds()     :: integer().
-type milliseconds():: integer().

-type cron_time()   :: {integer(), am | pm}
                     | {integer(), integer(), am | pm}
                     | calendar:time().
-type constraint() :: {between, cron_time(), cron_time()}.
-type duration()   :: {integer(), hr | h | min | m | sec | s}.
-type period()     :: cron_time() | {every, duration()}
                                  | {every, duration(), constraint()}.
-type dom()        :: integer().
-type dow_day()    :: mon | tue | wed | thu | fri | sat | sun.
-type dow()        :: dow_day() | [dow_day()].
-type callable()   :: {M :: module(), F :: atom(), A :: [term()]} |
                      fun(() -> term()) |
                      fun((JobRef::atom()|reference(), calendar:datetime()) -> term()).
-type run_when()   :: {once, cron_time()}
                    | {once, seconds()}
                    | {daily, period()}
                    | {weekly, dow(), period()}
                    | {monthly, dom()|[dom()], period()}.

-type  job()      :: {run_when(), callable()}
                   | {run_when(), callable(), job_opts()}.

%% should be opaque but dialyzer does not allow it
-type job_ref()   :: reference() | atom() | binary().
%% A job reference.

-type job_opts()  ::
    #{hostnames    => [binary()|string()],
      id           => term(),
      on_job_start => {Mod::atom(), Fun::atom()}
                      | fun((JobRef::job_ref()) -> any()),
      on_job_end   => {Mod::atom(), Fun::atom()}
                      | fun((JobRef::job_ref(),
                             Res :: {ok, term()} | {error, {Reason::term(), Stack::list()}})
                            -> term())
    }.
%% Job options:
%% <dl>
%% <dt>hostnames => [Hostname]</dt>
%%   <dd>List of hostnames where the job is allowed to run</dd>
%% <dt>id => ID</dt>
%%   <dd>An identifier of the job passed to `on_job_start' and `on_job_end'
%%   callbacks. It can be any term.</dd>
%% <dt>on_job_start => {Mod, Fun} | fun(JobRef, ID) -> any()</dt>
%%   <dd>`Mod:Fun(Ref::job_ref(), ID::any())' function to call on job start.
%%   The result is ignored.</dd>
%% <dt>{on_job_start, {Mod, Fun}} | fun(JobRef, ID, Result) -> term()</dt>
%%   <dd>`Mod:Fun(Ref::job_ref(), ID::any(), Result)' function to call after
%%   a job has ended.  `Result' is `{ok, JobResult::term()}' or
%%   `{error, `{Reason, StackTrace}}' if there is an exception.</dd>
%% </dl>

-type cron_opts() :: job_opts().
%% Cron default options applicable to all jobs. See job_opts().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%%  Check that the spec specified is valid or invalid
-spec validate(run_when()) -> ok | {error, term()}.
validate(Spec) ->
    ecrn_agent:validate(Spec).

%% @doc
%%  Adds a new job to the cron system. Jobs are described in the job()
%%  spec. It returns the JobRef that can be used to manipulate the job
%%  after it is created.

-spec cron(job()) ->
        job_ref() | ignored | already_started | {error, term()}.
cron(Job) ->
    cron(make_ref(), Job, #{}).

-spec cron(job()|job_ref(), job()|cron_opts()) ->
        job_ref() | ignored | already_started | {error, term()}.
cron(Job, DefOpts) when is_map(DefOpts) ->
    cron(make_ref(), Job, DefOpts);

cron(JobRef, Job) when (is_atom(JobRef) orelse is_reference(JobRef) orelse is_binary(JobRef))
                     , is_tuple(Job) ->
    cron(JobRef, Job, #{}).

%% @doc Schedule a job identified by a `JobRef'.
%% A job reference can be a reference, atom, or binary. If it is
%% an atom, a job will be locally registered by that name.
%% Returns false if the job is not permitted to run on the current host
-spec cron(job_ref(), job(), job_opts()) ->
        job_ref() | ignored | already_started | {error, term()}.
cron(JobRef, Job, Opts) when (is_atom(JobRef) orelse is_reference(JobRef) orelse is_binary(JobRef))
                           , is_tuple(Job), is_map(Opts) ->
    ecrn_cron_sup:add_job(JobRef, Job, Opts).

%% @doc
%% Convenience method to specify a job to run once at the given time
%% or after the amount of time specified.
-spec at(cron_time() | seconds(), callable()) ->
        job_ref() | ignored | already_started | {error, term()}.
at(When, Fun) ->
    at(make_ref(), When, Fun).

-spec at(job_ref(), cron_time() | seconds(), function()) ->
        job_ref() | ignored | already_started | {error, term()}.
at(JobRef, When, Fun) ->
    cron(JobRef, {{once, When}, Fun}).

-spec at(job_ref(), cron_time() | seconds(), function(), job_opts()) ->
        job_ref() | ignored | already_started | {error, term()}.
at(JobRef, When, Fun, #{} = Opts) ->
    cron(JobRef, {{once, When}, Fun}, Opts).

%% @doc
%% Convenience method to specify a job run to run on a daily basis
%% at a specific time.
-spec daily(cron_time() | seconds(), function()) ->
        job_ref() | ignored | already_started | {error, term()}.
daily(When, Fun) ->
    daily(make_ref(), When, Fun).

-spec daily(job_ref(), cron_time() | seconds(), function()) ->
        job_ref() | ignored | already_started | {error, term()}.
daily(JobRef, When, Fun) ->
    cron(JobRef, {{daily, When}, Fun}).

-spec daily(job_ref(), cron_time() | seconds(), function(), job_opts()) ->
        job_ref() | ignored | already_started | {error, term()}.
daily(JobRef, When, Fun, #{} = Opts) ->
    cron(JobRef, {{daily, When}, Fun}, Opts).

%% @doc
%% Convenience method to specify a job run to run on a weekly basis
%% at a specific time.
-spec weekly(dow(), cron_time() | seconds(), function()) ->
        job_ref() | ignored | already_started | {error, term()}.
weekly(DOW, When, Fun) ->
    weekly(make_ref(), DOW, When, Fun).

-spec weekly(job_ref(), dow(), cron_time() | seconds(), function()) ->
        job_ref() | ignored | already_started | {error, term()}.
weekly(JobRef, DOW, When, Fun) ->
    cron(JobRef, {{weekly, DOW, When}, Fun}).

-spec weekly(job_ref(), dow(), cron_time() | seconds(), function(), job_opts()) ->
        job_ref() | ignored | already_started | {error, term()}.
weekly(JobRef, DOW, When, Fun, #{} = Opts) ->
    cron(JobRef, {{weekly, DOW, When}, Fun}, Opts).

%% @doc
%% Convenience method to specify a job run to run on a weekly basis
%% at a specific time.
-spec monthly(dom(), cron_time() | seconds(), function()) ->
        job_ref() | ignored | already_started | {error, term()}.
monthly(DOM, When, Fun) ->
    monthly(make_ref(), DOM, When, Fun).

-spec monthly(job_ref(), dom(), cron_time() | seconds(), function()) ->
        job_ref() | ignored | already_started | {error, term()}.
monthly(JobRef, DOM, When, Fun) ->
    cron(JobRef, {{monthly, DOM, When}, Fun}).

-spec monthly(job_ref(), dom(), cron_time() | seconds(), function(), job_opts()) ->
        job_ref() | ignored | already_started | {error, term()}.
monthly(JobRef, DOM, When, Fun, #{} = Opts) ->
    cron(JobRef, {{monthly, DOM, When}, Fun}, Opts).

%% @doc
%% Cancel the job specified by the jobref.
-spec cancel(job_ref()) -> boolean().
cancel(JobRef) ->
    ecrn_control:cancel(JobRef).

%% @doc
%% Get the current date time in seconds millisince epoch.
-spec epoch() -> milliseconds().
epoch() ->
    ecrn_util:epoch_milliseconds().

%% @doc
%% Get the current date time in seconds since epoch.
-spec epoch_seconds() -> seconds().
epoch_seconds() ->
    ecrn_util:epoch_seconds().

%% @doc
%% Get the current date time of the erlcron system adjusted to reference.
%%
-spec datetime() -> {calendar:datetime(), milliseconds()}.
datetime() ->
    ecrn_control:datetime().

%% @doc
%% Get the reference date time of the erlcron system.
%%
-spec ref_datetime() -> {calendar:datetime(), milliseconds()}.
ref_datetime() ->
    ecrn_control:ref_datetime().

%% @doc
%% Set the current date time of the running erlcron system using local time.
-spec set_datetime(calendar:datetime()) -> ok.
set_datetime(DateTime) ->
    set_datetime(DateTime, local).

%% @doc
%% Set the current date time of the running erlcron system using either local
%% or universal time. The `TZ` parameter must contain an atom `local|universal`.
-spec set_datetime(calendar:datetime(), local|universal) -> ok.
set_datetime({D,T} = DateTime, TZ) when tuple_size(D)==3, tuple_size(T)==3 ->
    ecrn_control:set_datetime(DateTime, TZ).

%% @doc
%% Reset the reference datetime of erlcron system to current epoch time.
-spec reset_datetime() -> ok.
reset_datetime() ->
    ecrn_control:reset_datetime().

%% Get references of all running jobs
-spec get_all_jobs() -> [job_ref()].
get_all_jobs() ->
    ecrn_reg:get_all_refs().

%% @doc
%% Set the current date time of the erlcron system running on different nodes.
-spec multi_set_datetime(calendar:datetime()) -> {Replies, BadNodes} when
    Replies :: [{node(), ok | {error, term()}}],
    BadNodes :: [node()].
multi_set_datetime({D,T} = DateTime) when tuple_size(D)==3, tuple_size(T)==3 ->
    ecrn_control:multi_set_datetime([node()|nodes()], DateTime).

%% @doc
%% Set the current date time of the erlcron system running on the
%% specified nodes
-spec multi_set_datetime([node()], calendar:datetime()) -> {Replies, BadNodes} when
    Replies :: [{node(), ok | {error, term()}}],
    BadNodes :: [node()].
multi_set_datetime(Nodes, DateTime) when is_list(Nodes), tuple_size(DateTime)==2 ->
    ecrn_control:multi_set_datetime(Nodes, DateTime).
