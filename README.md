Erlcron
=======

[![Build Status](https://github.com/erlware/erlcron/actions/workflows/ci.yml/badge.svg)](https://github.com/erlware/erlcron/actions/workflows/ci.yml)
[![Hex pm](https://img.shields.io/hexpm/v/erlcron.svg)](https://hex.pm/packages/erlcron)
[![Docs](https://img.shields.io/badge/hex-docs-green.svg?style=flat)](https://hexdocs.pm/erlcron)

Erlcron provides testable cron like functionality for Erlang
systems, with the ability to arbitrarily set the time and place along
with fastforwarding through tests. See erlcron.erl for more
documentation.

The syntax of a job description is quite different from crontab.  It is
(in this author's opinion) easier to read and is much more in keeping
with the Erlang tradition.  It is not quite as expressive as cron but
this can be compensated for by adding multiple jobs.

No output is logged or mailed to anyone.  If you want output to be
logged or mailed, you must explicitly specify that as part of the job.

This does not poll the system on a minute-by-minute basis like cron
does.  Each job is assigned to a single (Erlang) process.  The time
until it is to run next is calculated, and the process sleeps for
exactly that long.

Unlike cron's one-minute resolution, erlcron has a millisecond resolution.

It does handle Daylight Savings Time changes (but not the cases when the
system clock is altered by small increments, in which case the next
execution of a scheduled task may be imprecise).

Cron Job Description Examples:

```erlang
{{once, {3, 30, pm}, fun() -> io:fwrite("Hello world!~n") end}

{{once, {3, 30, pm}, fun(JobRef, CurrDateTime) -> io:fwrite("Hello world!~n") end}

{{once, {3, 30, pm}},
    {io, fwrite, ["Hello, world!~n"]}}

{{once, {12, 23, 32}},
    {io, fwrite, ["Hello, world!~n"]}}

{{once, 3600},
    {io, fwrite, ["Hello, world!~n"]}}

{{daily, {every, {23, sec}, {between, {3, pm}, {3, 30, pm}}}},
    {io, fwrite, ["Hello, world!~n"]}}

{{daily, {3, 30, pm}},
    fun(_JobRef, _DateTime) -> io:fwrite("It's three thirty~n") end}

{{daily, [{1, 10, am}, {1, 07, 30, am}]},
    {io, fwrite, ["Bing~n"]}}

{{weekly, thu, {2, am}},
    {io, fwrite, ["It's 2 Thursday morning~n"]}}

{{weekly, wed, {2, am}},
    {fun(_JobRef, _DateTime) -> io:fwrite("It's 2 Wednesday morning~n") end}

{{weekly, [tue,wed], {2, am}},
    {fun(_, Now) -> io:format("Now is ~p~n", [Now]) end}

{{weekly, fri, {2, am}},
    {io, fwrite, ["It's 2 Friday morning~n"]}}

{{monthly, 1, {2, am}},
    {io, fwrite, ["First of the month!~n"]}}

{{monthly, [1, 7, 14], {2, am}},
    {io, fwrite, ["Every 1st, 7th, 14th of the month!~n"]}}

{{monthly, 0, {2, am}},
    {io, fwrite, ["Last day of the month!~n"]}}

{{monthly, [0, -1], {2, am}},
    {io, fwrite, ["Last two days of the month!~n"]}}

{{monthly, 4, {2, am}},
    {io, fwrite, ["Fourth of the month!~n"]}}

%% Days of month less or equal to zero are subtracted from the end of the month
{{monthly, 0, {2, am}},
    {io, fwrite, ["Last day of the month!~n"]}}
```

Adding a cron to the system:

```erlang
Job = {{weekly, thu, {2, am}},
    {io, fwrite, ["It's 2 Thursday morning~n"]}}.

erlcron:cron(Job).
```

Cron jobs can be given named atom references:

```erlang
erlcron:cron(test_job1, {{once, {3,pm}}, {io, fwrite, "It's 3pm"}}).
```

A simple way to add a daily cron:

    erlcron:daily({3, 30, pm}, Fun).

A simple way to add a job that will be run once in the future. Where
'once' is the number of seconds until it runs.

```erlang
erlcron:at(300, Fun).
```

Cancel a running job.

```erlang
erlcron:cancel(JobRef).
```

Get the current reference (universal) date time of the system.

```erlang
erlcron:datetime().
```

Set the current date time of the system. Any tests that need to be run
in the interim will be run as the time rolls forward.

```erlang
erlcron:set_datetime(DateTime).
```

Set the current date time of the system on all nodes in the
cluster. Any tests that need to be run in the interim will be run as
the time rolls forward.

```erlang
erlcron:multi_set_datetime(DateTime).
```

Set the current date time of the system on a specific set of nodes in
the cluster. Any tests that need to be run in the interim will be run
as the time rolls forward.

```erlang
erlcron:multi_set_datetime(Nodes, DateTime).
```

The application cron can be pre-configured through environment variables
in the config file that all applications can load in the Erlang VM start.
The app.config file can be as follow:

```erlang
[
    {erlcron, [
        {crontab, [
            {{once, {3, 30, pm}}, {io, fwrite, ["Hello, world!~n"]}},

            %% A job may be specified to be run only if the current host
            %% is in the list of given hostnames.  If default crontab
            %% options are provided in the `defaults` setting,
            %% the job-specific options will override corresponding
            %% default options.  See erlcron:job_opts() for available
            %% options.
            {{once, {12, 23, 32}}, {io, fwrite, ["Hello, world!~n"]},
                #{hostnames => ["somehost"]},

            {{daily, {every, {23, sec}, {between, {3, pm}, {3, 30, pm}}}},
             {io, fwrite, ["Hello, world!~n"]}},

            %% A job spec can be defined as a map, where the `interval' and
            %% `execute' keys are mandatory:
            #{id => test_job,    interval  => {daily, {1, 0, pm}},
                                 execute   => {io, fwrite, ["Hello, world!~n"]}},

            %% If defined as a map, the map can contain any `erlcron:job_opts()'
            %% options:
            #{id => another_job, interval  => {daily, {1, 0, pm}},
                                 execute   => {io, fwrite, ["Hello, world!~n"]},
                                 hostnames => ["myhost"]}
        ]},

        %% Instead of specifying individual options for each job, you can
        %% define default options here.
        {defaults, #{
            %% Limit jobs to run only on the given list of hosts
            hostnames    => ["myhost"],

            %% Function `fun((Ref) -> ignore | any())` to call before a job is started.
            %% If the function returns `ignore`, the job will not be executed, and the
            %% `on_job_end` callback will not be executed.
            on_job_start => {some_module, function},

            %% Function `fun((Ref, Status :: {ok, Result}|{error, {Reason, StackTrace}}) -> ok)`
            %% to call after a job has ended
            on_job_end   => {some_module, function}
        }}
    ]}
].
```

So, when the app will be started, all configurations will be loaded.

Note that the limitation is that in the config file is impossible load an
anonymous function (or lambda function) so, you only can use {M,F,A} format.

If an `on_job_start` or `on_job_end` functions are provided for any job or as
default settings, it's the responsibility of a developer to make sure that
those functions handle exceptions, since the failure in those functions will
cause the supervisor to restart the job up until the supervisor reaches its
maximum restart intensity.
