%%%----------------------------------------------------------------
%%% @author Eric Newhuis <enewhuis@ecdmarket.com>
%%% @copyright 2009-2010 eCD Market
%%%----------------------------------------------------------------,
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

-include_lib("erlcron/include/erlcron.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%  Check that the spec specified is valid or invalid
%% @end
%%--------------------------------------------------------------------
-spec validate(run_when()) -> valid | invalid.
validate(Spec) ->
    ecrn_agent:validate(Spec).

%%--------------------------------------------------------------------
%% @doc
%%  Adds a new job to the cron system. Jobs are described in the job()
%% spec. It returns the JobRef that can be used to manipulate the job
%% after it is created.
%% @end
%%--------------------------------------------------------------------
-spec cron(job()) -> job_ref().
cron(Job) ->
    JobRef = make_ref(),
    ecrn_cron_sup:add_job(JobRef, Job),
    JobRef.

%%--------------------------------------------------------------------
%% @doc
%% Convienience method to specify a job run to run on a daily basis
%% at a specific time.
%% @end
%%--------------------------------------------------------------------
-spec at(time() | seconds(), function()) -> job_ref().
at(When, Fun) ->
    Job = {{daily, When}, Fun},
    cron(Job).

%%--------------------------------------------------------------------
%% @doc
%%   Run the specified job once after the amount of time specifed.
%% @end
%%--------------------------------------------------------------------
-spec once(time() | seconds(), function()) ->  job_ref().
once(When, Fun) ->
    Job = {{once, When}, Fun},
    cron(Job).

%%--------------------------------------------------------------------
%% @doc
%% Cancel the job specified by the jobref.
%% @end
%%--------------------------------------------------------------------
-spec cancel(job_ref()) -> ok.
cancel(JobRef) ->
    ecrn_control:cancel(JobRef).

%%--------------------------------------------------------------------
%% @doc
%%  Get the current date time of the running erlcron system.
%% @end
%%--------------------------------------------------------------------
-spec datetime() -> datetime().
datetime() ->
    ecrn_control:datetime().

%%--------------------------------------------------------------------
%% @doc
%%  Set the current date time of the running erlcron system.
%% @end
%%--------------------------------------------------------------------
-spec set_datetime(datetime()) -> ok.
set_datetime(DateTime) ->
    ecrn_control:set_datetime(DateTime).


%%--------------------------------------------------------------------
%% @doc
%%  Set the current date time of the erlcron system running on different nodes.
%% @end
%%--------------------------------------------------------------------
-spec multi_set_datetime(datetime()) -> ok.
multi_set_datetime(DateTime) ->
    ecrn_control:multi_set_datetime([node()|nodes()], DateTime).


%%--------------------------------------------------------------------
%% @doc
%%  Set the current date time of the erlcron system running on the
%%  specified nodes
%% @end
%%--------------------------------------------------------------------
-spec multi_set_datetime([node()], datetime()) -> ok.
multi_set_datetime(Nodes, DateTime) when is_list(Nodes) ->
    ecrn_control:multi_set_datetime(Nodes, DateTime).

