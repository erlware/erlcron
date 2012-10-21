Erlcron provides testable cronish like functionality for Erlang
systems, with the ability to arbitrarily set the time and place along
with fastforwarding through tests. See erlcron.erl for more
documentation.

The synax of a job description is quite different from crontab.  It is
(in this author's opinion) easier to read and is much more in keeping
with the Erlang tradition.  It is not quite as expressive as cron but
this can be compensated for by adding multiple jobs.

No output is logged or mailed to anyone.  If you want output to be
logged or mailed, you must explicitly specify that as part of the
job.

This does not poll the system on a minute-by-minute basis like cron
does.  Each job is assigned to a single (Erlang) process.  The time
until it is to run next is calculated, and the process sleeps for
exactly that long.

Unlike cron's one-minute resolution, crone
has a 2-second resolution (actually 1 second, but after running the
job, the process waits an extra second to avoid accidentally running it
more than once.)

Because it does not wake up every minute, and because it does not
have a fixed configuration file, crone must be stopped and
restarted if the user wishes to change the scheduled jobs.

It does not handle Daylight Savings Time (or other
cases when the system clock is altered) gracefully, and it is recommended
that it be stopped and restarted on such occasions.

Cron Job Description Examples:

   {{once, {3, 30, pm}},
   {io, fwrite, ["Hello, world!~n"]}}

   {{once, {12, 23, 32}},
     {io, fwrite, ["Hello, world!~n"]}}

   {{once, 3600},
     {io, fwrite, ["Hello, world!~n"]}}

   {{daily, {every, {23, sec}, {between, {3, pm}, {3, 30, pm}}}},
     {io, fwrite, ["Hello, world!~n"]}}

   {{daily, {3, 30, pm}},
     fun() -> io:fwrite("It's three thirty~n") end}

   {{daily, [{1, 10, am}, {1, 07, 30, am}]},
     {io, fwrite, ["Bing~n"]}}

   {{weekly, thu, {2, am}},
     {io, fwrite, ["It's 2 Thursday morning~n"]}}

   {{weekly, wed, {2, am}},
     {fuxbn() -> io:fwrite("It's 2 Wednesday morning~n") end}

   {{weekly, fri, {2, am}},
     {io, fwrite, ["It's 2 Friday morning~n"]}}

   {{monthly, 1, {2, am}},
     {io, fwrite, ["First of the month!~n"]}}

   {{monthly, 4, {2, am}},
     {io, fwrite, ["Fourth of the month!~n"]}}


Adding a cron to the system:

Job = {{weekly, thu, {2, am}},
     {io, fwrite, ["It's 2 Thursday morning~n"]}}.

erlcron:cron(Job).

A simple way to add a daily cron:

erlcron:at({3, 30, pm}, Fun).

A simple way to add a job that will be run once in the future. Where
'once' is the number of seconds until it runs.

erlcron:once(300, Fun).


Cancel a running job.

erlcron:cancel(JobRef).


Get the current date time of the system.

erlcron:datetime().


Set the current date time of the system. Any tests that need to be run
in the interim will be run as the time rolls forward.

erlcron:set_datetime(DateTime).


Set the current date time of the system on all nodes in the
cluster. Any tests that need to be run in the interim will be run as
the time rolls forward.

erlcron:multi_set_datetime(DateTime).

Set the current date time of the system on a specific set of nodes in
the cluster. Any tests that need to be run in the interim will be run
as the time rolls forward.

erlcron:multi_set_datetime(Nodes, DateTime).

