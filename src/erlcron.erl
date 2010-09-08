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

%% @doc Simple job scheduler for Erlang/OTP
%%
%% This is a simple utility which schedules jobs to be
%% run at a given time or periodically
%%
%% The synax of a job description is quite different from
%% <code>crontab</code>.  It is (in this author's opinion) easier to read
%% and is much more in keeping with the Erlang tradition.  It is not
%% quite as expressive as <code>cron</code> but this can be compensated
%% for by adding multiple jobs.
%%
%% No output is logged or mailed to anyone.  If you want output to
%% be logged or mailed, you must explicitly specify that as part of the
%% job.
%%
%% This does not poll the system on a minute-by-minute
%% basis like <code>cron</code> does.  Each job is assigned to a single
%% (Erlang) process.  The time until it is to run next is calculated,
%% and the process sleeps for exactly that long.
%%
%% Unlike <code>cron</code>'s one-minute resolution, <code>crone</code>
%% has a 2-second resolution (actually 1 second, but after running the
%% job, the process waits an extra second to avoid accidentally running it
%% more than once.)
%%
%% Because it does not wake up every minute, and because it does not
%% have a fixed configuration file, <code>crone</code> must be stopped and
%% restarted if the user wishes to change the scheduled jobs.
%%
%% It does not handle Daylight Savings Time (or other
%% cases when the system clock is altered) gracefully, and it is recommended
%% that it be stopped and restarted on such occasions.
%%
%% Cron Job Examples
%%
%% <code>
%%
%%   {{once, {3, 30, pm}},
%%     {io, fwrite, ["Hello, world!~n"]}}
%%
%%   {{once, {12, 23, 32}},
%%     {io, fwrite, ["Hello, world!~n"]}}
%%
%%   {{once, 3600},
%%     {io, fwrite, ["Hello, world!~n"]}}
%%
%%   {{daily, {every, {23, sec}, {between, {3, pm}, {3, 30, pm}}}},
%%     {io, fwrite, ["Hello, world!~n"]}}
%%
%%   {{daily, {3, 30, pm}},
%%     fun() -> io:fwrite("It's three thirty~n") end}
%%
%%   {{daily, [{1, 10, am}, {1, 07, 30, am}]},
%%     {io, fwrite, ["Bing~n"]}}
%%
%%   {{weekly, thu, {2, am}},
%%     {io, fwrite, ["It's 2 Thursday morning~n"]}}
%%
%%   {{weekly, wed, {2, am}},
%%     {fuxbn() -> io:fwrite("It's 2 Wednesday morning~n") end}
%%
%%   {{weekly, fri, {2, am}},
%%     {io, fwrite, ["It's 2 Friday morning~n"]}}
%%
%%   {{monthly, 1, {2, am}},
%%     {io, fwrite, ["First of the month!~n"]}}
%%
%%   {{monthly, 4, {2, am}},
%%     {io, fwrite, ["Fourth of the month!~n"]}}
%%  </code>
%%
%% @end

-spec cron(job()) -> ok.
cron(Job) ->
    AlarmRef = make_ref(),
    ecrn_cron_sup:add_job(AlarmRef, Job),
    AlarmRef.

-spec at(time() | seconds(), function()) -> ok.
at(When, Fun) ->
    Job = {{daily, When}, Fun},
    cron(Job).

-spec once(time() | seconds(), function()) -> ok.
once(When, Fun) ->
    Job = {{once, When}, Fun},
    cron(Job).

-spec cancel(job_ref()) -> ok.
cancel(AlarmRef) ->
    ecrn_control:cancel(AlarmRef).


-spec datetime() -> datetime().
datetime() ->
    ecrn_control:datetime().

-spec set_datetime(datetime()) -> ok.
set_datetime(DateTime) ->
    ecrn_control:set_datetime(DateTime).

-spec set_datetime(datetime(), testing | production) -> ok.
set_datetime(DateTime, Atom) when is_atom(Atom) ->
    ecrn_control:set_datetime(DateTime, Atom).

-spec multi_set_datetime(datetime()) -> ok.
multi_set_datetime(DateTime) ->
    ecrn_control:multi_set_datetime([node()|nodes()], DateTime).

-spec multi_set_datetime([node()], datetime()) -> ok.
multi_set_datetime(Nodes, DateTime) when is_list(Nodes) ->
    ecrn_control:multi_set_datetime(Nodes, DateTime).

multi_set_datetime(Nodes, DateTime, testing) when is_list(Nodes) ->
    ecrn_control:multi_set_datetime(Nodes, DateTime, testing).
