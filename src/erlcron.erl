%%% @copyright Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the BSD License; you may not use
%%% this file except in compliance with the License.
-module(erlcron).

-export([validate/1,
         cron/1,
         at/2,
         once/2,
         cancel/1,
         datetime/0,
         set_datetime/1,
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
              seconds/0]).


%%%===================================================================
%%% Types
%%%===================================================================

-type seconds()    :: integer().

-type cron_time()   :: {integer(), am | pm}
                     | {integer(), integer(), am | pm}
                     | calendar:time().
-type constraint() :: {between, cron_time(), cron_time()}.
-type duration()   :: {integer(), hr | min | sec}.
-type period()     :: cron_time() | {every, duration(), constraint()}.
-type dom()        :: integer().
-type dow()        :: mon | tue | wed | thu | fri | sat | sun.
-type callable()   :: {M :: module(), F :: atom(), A :: [term()]} | function().
-type run_when()   :: {once, cron_time()}
                    | {once, seconds()}
                    | {daily, period()}
                    | {weekly, dow(), period()}
                    | {monthly, dom(), period()}.

-type  job()      :: {run_when(), callable()}.

%% should be opaque but dialyzer does not allow it
-type job_ref()   :: reference().


%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%%  Check that the spec specified is valid or invalid
-spec validate(run_when()) -> valid | invalid.
validate(Spec) ->
    ecrn_agent:validate(Spec).

%% @doc
%%  Adds a new job to the cron system. Jobs are described in the job()
%%  spec. It returns the JobRef that can be used to manipulate the job
%%  after it is created.

-spec cron(job()) -> job_ref().
cron(Job) ->
    JobRef = make_ref(),
    ecrn_cron_sup:add_job(JobRef, Job).
%% @doc
%%  Convienience method to specify a job run to run on a daily basis
%%  at a specific time.
-spec at(cron_time() | seconds(), function()) -> job_ref().
at(When, Fun) ->
    Job = {{daily, When}, Fun},
    cron(Job).

%% @doc
%%   Run the specified job once after the amount of time specifed.
-spec once(cron_time() | seconds(), function()) ->  job_ref().
once(When, Fun) ->
    Job = {{once, When}, Fun},
    cron(Job).

%% @doc
%% Cancel the job specified by the jobref.
-spec cancel(job_ref()) -> ok | undefined.
cancel(JobRef) ->
    ecrn_control:cancel(JobRef).

%% @doc
%%  Get the current date time of the running erlcron system.
-spec datetime() -> {calendar:datetime(), seconds()}.
datetime() ->
    ecrn_control:datetime().

%% @doc
%%  Set the current date time of the running erlcron system.
-spec set_datetime(calendar:datetime()) -> ok.
set_datetime(DateTime) ->
    ecrn_control:set_datetime(DateTime).

%% @doc
%%  Set the current date time of the erlcron system running on different nodes.
-spec multi_set_datetime(calendar:datetime()) -> ok.
multi_set_datetime(DateTime) ->
    ecrn_control:multi_set_datetime([node()|nodes()], DateTime).

%% @doc
%%  Set the current date time of the erlcron system running on the
%%  specified nodes
-spec multi_set_datetime([node()], calendar:datetime()) -> ok.
multi_set_datetime(Nodes, DateTime) when is_list(Nodes) ->
    ecrn_control:multi_set_datetime(Nodes, DateTime).
