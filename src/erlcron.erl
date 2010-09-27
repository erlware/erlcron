%%%----------------------------------------------------------------
%%% @author Eric Newhuis <enewhuis@ecdmarket.com>
%%% @copyright 2009-2010 eCD Market
%%%----------------------------------------------------------------,
-module(erlcron).

-export([cron/1,
	 at/2,
	 once/2,
	 cancel/1,
	 datetime/0,
	 set_datetime/1,
	 set_datetime/2,
	 multi_set_datetime/1,
	 multi_set_datetime/2,
	 multi_set_datetime/3]).

-include_lib("erlcron/include/erlcron.hrl").

-spec cron(job()) -> job_ref().
cron(Job) ->
    JobRef = make_ref(),
    ecrn_cron_sup:add_job(JobRef, Job),
    JobRef.

-spec at(time() | seconds(), function()) -> job_ref().
at(When, Fun) ->
    Job = {{daily, When}, Fun},
    cron(Job).

-spec once(time() | seconds(), function()) ->  job_ref().
once(When, Fun) ->
    Job = {{once, When}, Fun},
    cron(Job).

-spec cancel(job_ref()) -> ok.
cancel(JobRef) ->
    ecrn_control:cancel(JobRef).


-spec datetime() -> datetime().
datetime() ->
    ecrn_control:datetime().

-spec set_datetime(datetime()) -> ok.
set_datetime(DateTime) ->
    ecrn_control:set_datetime(DateTime).


-spec multi_set_datetime(datetime()) -> ok.
multi_set_datetime(DateTime) ->
    ecrn_control:multi_set_datetime([node()|nodes()], DateTime).

-spec multi_set_datetime([node()], datetime()) -> ok.
multi_set_datetime(Nodes, DateTime) when is_list(Nodes) ->
    ecrn_control:multi_set_datetime(Nodes, DateTime).

