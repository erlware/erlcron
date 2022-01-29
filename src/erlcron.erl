%%% @copyright Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the BSD License; you may not use
%%% this file except in compliance with the License.
-module(erlcron).

-export([validate/1,
         cron/1,   cron/2,
         at/2,     at/3,
         daily/2,  daily/3,
         weekly/3, weekly/4,
         monthly/3,monthly/4,
         cancel/1,
         epoch/0,
         epoch_seconds/0,
         datetime/0,
         ref_datetime/0,
         set_datetime/1,
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
                      fun(() -> ok) |
                      fun((JobRef::atom()|reference(), calendar:datetime()) -> ok).
-type run_when()   :: {once, cron_time()}
                    | {once, seconds()}
                    | {daily, period()}
                    | {weekly, dow(), period()}
                    | {monthly, dom()|[dom()], period()}.

-type  job()      :: {run_when(), callable()}.

%% should be opaque but dialyzer does not allow it
-type job_ref()   :: reference() | atom().


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

-spec cron(job()) -> job_ref().
cron(Job) ->
    cron(make_ref(), Job).

-spec cron(job_ref(), job()) -> job_ref().
cron(JobRef, Job) when (is_atom(JobRef) orelse is_reference(JobRef))
                     , is_tuple(Job) ->
    ecrn_cron_sup:add_job(JobRef, Job).

%% @doc
%% Run the specified job once after the amount of time specified.
-spec at(cron_time() | seconds(), function()) -> job_ref().
at(When, Fun) ->
    at(make_ref(), When, Fun).

-spec at(job_ref(), cron_time() | seconds(), function()) -> job_ref().
at(JobRef, When, Fun) ->
    cron(JobRef, {{once, When}, Fun}).

%% @doc
%% Convenience method to specify a job run to run on a daily basis
%% at a specific time.
-spec daily(cron_time() | seconds(), function()) ->  job_ref().
daily(When, Fun) ->
    daily(make_ref(), When, Fun).

-spec daily(job_ref(), cron_time() | seconds(), function()) ->  job_ref().
daily(JobRef, When, Fun) ->
    cron(JobRef, {{daily, When}, Fun}).

%% @doc
%% Convenience method to specify a job run to run on a weekly basis
%% at a specific time.
-spec weekly(dow(), cron_time() | seconds(), function()) ->  job_ref().
weekly(DOW, When, Fun) ->
    weekly(make_ref(), DOW, When, Fun).

-spec weekly(job_ref(), dow(), cron_time() | seconds(), function()) -> job_ref().
weekly(JobRef, DOW, When, Fun) ->
    cron(JobRef, {{weekly, DOW, When}, Fun}).

%% @doc
%% Convenience method to specify a job run to run on a weekly basis
%% at a specific time.
-spec monthly(dom(), cron_time() | seconds(), function()) -> job_ref().
monthly(DOM, When, Fun) ->
    monthly(make_ref(), DOM, When, Fun).

-spec monthly(job_ref(), dom(), cron_time() | seconds(), function()) -> job_ref().
monthly(JobRef, DOM, When, Fun) ->
    cron(JobRef, {{monthly, DOM, When}, Fun}).

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
%% Set the current date time of the running erlcron system.
-spec set_datetime(calendar:datetime()) -> ok.
set_datetime({D,T} = DateTime) when tuple_size(D)==3, tuple_size(T)==3 ->
    ecrn_control:set_datetime(DateTime).

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
-spec multi_set_datetime(calendar:datetime()) -> ok.
multi_set_datetime({D,T} = DateTime) when tuple_size(D)==3, tuple_size(T)==3 ->
    ecrn_control:multi_set_datetime([node()|nodes()], DateTime).

%% @doc
%% Set the current date time of the erlcron system running on the
%% specified nodes
-spec multi_set_datetime([node()], calendar:datetime()) -> ok.
multi_set_datetime(Nodes, DateTime) when is_list(Nodes), tuple_size(DateTime)==2 ->
    ecrn_control:multi_set_datetime(Nodes, DateTime).
