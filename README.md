Erlcron
=======

[![Build Status](https://github.com/erlware/erlcron/actions/workflows/ci.yml/badge.svg)](https://github.com/erlware/erlcron/actions/workflows/ci.yml)
[![Hex pm](https://img.shields.io/hexpm/v/erlcron.svg)](https://hex.pm/packages/erlcron)
[![Docs](https://img.shields.io/badge/hex-docs-green.svg?style=flat)](https://hexdocs.pm/erlcron)

Erlcron is a cron-like job scheduler for Erlang applications. It is designed to
be testable: the system clock can be arbitrarily set and fast-forwarded, making
it easy to verify time-based behaviour in automated tests.

- **Erlang-native syntax** — job schedules are expressed as Erlang terms, not
  crontab strings.
- **Millisecond resolution** — unlike Unix cron's one-minute polling interval,
  each job sleeps for the exact duration until its next execution.
- **Per-job processes** — every scheduled job runs in its own lightweight
  Erlang process.
- **Daylight Saving Time aware** — schedule recalculation is capped at 30
  minutes so DST transitions do not cause jobs to be skipped or duplicated.
- **Cluster support** — the reference clock can be synchronised across a set of
  Erlang nodes with a single call.

> **Note:** No output is logged or mailed automatically. Any logging must be
> done inside the job function itself.

---

## Table of Contents

1. [Installation](#installation)
2. [Quick Start](#quick-start)
3. [Schedule Syntax](#schedule-syntax)
   - [once](#once)
   - [Period](#period)
   - [daily](#daily)
   - [weekly](#weekly)
   - [monthly](#monthly)
4. [Callable Forms](#callable-forms)
5. [Job Options](#job-options)
6. [API Reference](#api-reference)
7. [Application Configuration](#application-configuration)
8. [Testing with Time Manipulation](#testing-with-time-manipulation)

---

## Installation

For Elixir, add the following to `mix.exs` dependencies: 
```elixir
{:erlcron, "~> 1.0"}
```

For Erlng, add `erlcron` to your `rebar.config`:

```erlang
{deps, [
    {erlcron, "~> 1.0"}
]}.
```

Or with the latest stable version from Hex:

```erlang
{deps, [erlcron]}.
```

Then start the application before scheduling any jobs:

```erlang
application:ensure_all_started(erlcron).
```

---

## Quick Start

```erlang
%% Run a function once at 3:30 PM today
erlcron:cron({{once, {3, 30, pm}}, fun() -> io:fwrite("Hello!~n") end}).

%% Run a function every day at 3:30 PM
erlcron:daily({3, 30, pm}, fun() -> io:fwrite("Daily reminder~n") end).

%% Run a function once after 5 minutes (300 seconds)
erlcron:at(300, fun() -> io:fwrite("Five minutes later~n") end).

%% Cancel a job
JobRef = erlcron:cron({{daily, {noon}}, fun() -> ok end}),
erlcron:cancel(JobRef).
```

---

## Schedule Syntax

All schedules are expressed as `{Type, ...}` tuples.

### Time Literals (Duration)

Times can be expressed in several equivalent forms:

| Form | Meaning |
|------|---------|
| `{H, am\|pm}` | Hour in 12-hour clock, e.g. `{3, pm}` = 15:00:00 |
| `{H, M, am\|pm}` | Hour and minute, e.g. `{3, 30, pm}` = 15:30:00 |
| `{H, M, S}` | 24-hour clock, e.g. `{15, 30, 0}` |
| `{H, M, S, Ms}` | Same as `{H, M, S}` but includes milliseconds |

### `once`

Run a job exactly one time.

```erlang
%% At a specific time of day
{once, {3, 30, pm}}

%% At an exact 24-hour time
{once, {12, 23, 32}}

%% After N seconds from now
{once, 3600}
```

### Period

`Period` is used wherever a repeating or multi-time schedule is needed
(`daily`, `weekly`, `monthly`). It can take any of the following forms:

| Form | Example | Meaning |
|------|---------|----------|
| `When` | `{3, 30, pm}` | Run once per day at the given time |
| `[When, ...]` | `[{9, am}, {5, pm}]` | Run at each listed time per day |
| `{every, Duration}` | `{every, {30, min}}` | Repeat every *Duration* all day |
| `{every, Duration, {between, From, To}}` | `{every, {5, min}, {between, {9, am}, {5, pm}}}` | Repeat every *Duration* within the time window |

Duration units: `hr` / `h`, `min` / `m`, `sec` / `s`.

---

### `daily`

Run a job every day. The time can be a single time or a list of times.
It also accepts an `every` tuple to repeat within a time window.

```erlang
%% Every day at 3:30 PM
{daily, {3, 30, pm}}

%% Every day at multiple times
{daily, [{1, 10, am}, {1, 07, 30, am}]}

%% Every 23 seconds between 3 PM and 3:30 PM, every day
{daily, {every, {23, sec}, {between, {3, pm}, {3, 30, pm}}}}
```

### `weekly`

Run a job on one or more specific days of the week. Day-of-week atoms are:
`mon`, `tue`, `wed`, `thu`, `fri`, `sat`, `sun`.

```erlang
%% Every Thursday at 2 AM
{weekly, thu, {2, am}}

%% Every Tuesday and Wednesday at 2 AM
{weekly, [tue, wed], {2, am}}

%% Every Friday at 2 AM
{weekly, fri, {2, am}}
```

### `monthly`

Run a job on one or more days of the month. Day values of `0` and negative
integers are counted backwards from the last day of the month: `0` means the
last day, `-1` means the second-to-last day, and so on.

```erlang
%% On the 1st of every month at 2 AM
{monthly, 1, {2, am}}

%% On the 1st, 7th, and 14th at 2 AM
{monthly, [1, 7, 14], {2, am}}

%% On the last day of every month at 2 AM
{monthly, 0, {2, am}}

%% On the last two days of every month at 2 AM
{monthly, [0, -1], {2, am}}
```

---

## Callable Forms

The action to execute can be specified in any of the following forms:

```erlang
%% Zero-arity fun
fun() -> io:fwrite("Hello!~n") end

%% Two-arity fun receiving the job reference and the scheduled run time
fun(JobRef, DateTime) -> io:format("Running ~p at ~p~n", [JobRef, DateTime]) end

%% MFA tuple: the function is called as M:F(Args...)
{io, fwrite, ["Hello!~n"]}

%% MF tuple (no args): the function is called as M:F() or M:F(JobRef, DateTime)
%% depending on which arity is exported
{my_module, my_function}
```

---

## Job Options

Jobs accept an optional `job_opts()` map to control behaviour:

| Key | Type | Description |
|-----|------|-------------|
| `id` | `term()` | A stable identifier passed to lifecycle callbacks. |
| `hostnames` | `[string() \| binary()]` | Restrict the job to run only on the listed hostnames. |
| `on_job_start` | `fun((JobRef) -> ignore \| any()) \| {Mod, Fun}` | Called before executing the job. Return `ignore` to skip this execution. |
| `on_job_end` | `fun((JobRef, Result) -> any()) \| {Mod, Fun}` | Called after the job finishes. `Result` is `{ok, Value}` or `{error, {Reason, Stack}}`. |

> **Important:** If `on_job_start` or `on_job_end` raise an exception, the
> supervisor will restart the job process. Make sure these callbacks handle
> errors internally to avoid hitting the supervisor's restart intensity limit.

---

## API Reference

### Scheduling jobs

```erlang
%% Schedule a job from a full job() spec
erlcron:cron(Job) -> JobRef | ignored | already_started | {error, Reason}
erlcron:cron(JobRef, Job) -> ...
erlcron:cron(JobRef, Job, Opts) -> ...

%% Convenience: run once
erlcron:at(When, Fun) -> JobRef | ...
erlcron:at(JobRef, When, Fun) -> ...
erlcron:at(JobRef, When, Fun, Opts) -> ...

%% Convenience: run daily
erlcron:daily(When, Fun) -> JobRef | ...
erlcron:daily(JobRef, When, Fun) -> ...
erlcron:daily(JobRef, When, Fun, Opts) -> ...

%% Convenience: run weekly
erlcron:weekly(DOW, When, Fun) -> JobRef | ...
erlcron:weekly(JobRef, DOW, When, Fun) -> ...
erlcron:weekly(JobRef, DOW, When, Fun, Opts) -> ...

%% Convenience: run monthly
erlcron:monthly(DOM, When, Fun) -> JobRef | ...
erlcron:monthly(JobRef, DOM, When, Fun) -> ...
erlcron:monthly(JobRef, DOM, When, Fun, Opts) -> ...
```

### Managing jobs

```erlang
%% Cancel a running job (returns true if found, false otherwise)
erlcron:cancel(JobRef) -> boolean()

%% Get the job references of all running jobs
erlcron:get_all_jobs() -> [JobRef]

%% Validate a schedule spec without scheduling it
erlcron:validate(RunWhen) -> ok | {error, Reason}
```

### Time functions

```erlang
%% Current erlcron datetime (universal) with millisecond offset
erlcron:datetime() -> {calendar:datetime(), Milliseconds}

%% Reference datetime used as the clock base
erlcron:ref_datetime() -> {calendar:datetime(), Milliseconds}

%% Current time as milliseconds since Unix epoch
erlcron:epoch() -> Milliseconds

%% Current time as seconds since Unix epoch
erlcron:epoch_seconds() -> Seconds
```

### Time manipulation (for testing)

```erlang
%% Set the clock on the local node (local time)
erlcron:set_datetime(DateTime) -> ok

%% Set the clock on the local node with explicit timezone
erlcron:set_datetime(DateTime, local | universal) -> ok

%% Reset the clock to the real system time
erlcron:reset_datetime() -> ok

%% Synchronise the clock across all connected nodes
erlcron:multi_set_datetime(DateTime) -> {Replies, BadNodes}

%% Synchronise the clock across a specific list of nodes
erlcron:multi_set_datetime(Nodes, DateTime) -> {Replies, BadNodes}
```

### Named job references

Jobs can be given a named atom reference that can be used in other calls:

```erlang
erlcron:cron(my_job, {{once, {3, pm}}, {io, fwrite, ["It's 3 PM~n"]}}).

%% Later:
erlcron:cancel(my_job).
```

---

## Application Configuration

Jobs can be pre-configured via the standard Erlang application environment so
they start automatically when the application starts. Because anonymous
functions cannot be stored in config files, only `{M, F, A}` callables are
supported here.

```erlang
[
    {erlcron, [
        {crontab, [
            %% Simple job
            {{once, {3, 30, pm}}, {io, fwrite, ["Hello, world!~n"]}},

            %% Job restricted to a specific host
            {{once, {12, 23, 32}}, {io, fwrite, ["Hello, world!~n"]},
                #{hostnames => ["somehost"]}},

            %% Recurring daily job
            {{daily, {every, {23, sec}, {between, {3, pm}, {3, 30, pm}}}},
             {io, fwrite, ["Hello, world!~n"]}},

            %% Map-based job spec (id, interval, and execute are required)
            #{id       => test_job,
              interval => {daily, {1, 0, pm}},
              execute  => {io, fwrite, ["Hello, world!~n"]}},

            %% Map-based spec with additional options
            #{id        => another_job,
              interval  => {daily, {1, 0, pm}},
              execute   => {io, fwrite, ["Hello, world!~n"]},
              hostnames => ["myhost"]}
        ]},

        %% Default options applied to every job unless overridden per-job
        {defaults, #{
            %% Only run jobs on these hosts
            hostnames    => ["myhost"],

            %% Called before each job execution; return `ignore` to skip
            on_job_start => {some_module, on_start},

            %% Called after each job execution with the result
            on_job_end   => {some_module, on_end}
        }}
    ]}
].
```

All jobs listed in `crontab` are started when the `erlcron` application starts.
Per-job options override the corresponding `defaults` options.

---

## Testing with Time Manipulation

One of erlcron's primary design goals is testability. You can advance the
scheduler clock to any point in the future and all jobs whose scheduled time
falls within the elapsed interval will be executed in order:

```erlang
%% Schedule a job for tomorrow
erlcron:cron({{once, Tomorrow}, fun() -> do_something() end}),

%% Fast-forward to tomorrow — the job will execute
erlcron:set_datetime(Tomorrow).
```

To restore the real system clock after a test:

```erlang
erlcron:reset_datetime().
```
