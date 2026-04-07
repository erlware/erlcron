%%% @copyright Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the BSD License; you may not use
%%% this file except in compliance with the License.
-module(erlcron).

-moduledoc """
Erlcron is a testable cron-like job scheduler for Erlang applications.

Each scheduled job runs in its own lightweight process that sleeps for
exactly the computed duration until its next execution time, giving
millisecond precision instead of the one-minute polling interval used
by Unix cron.

The scheduler clock can be manipulated at runtime with
`set_datetime/1` and `multi_set_datetime/1`. When the clock is
advanced, all jobs whose scheduled time falls within the elapsed
interval are executed in order, which makes it straightforward to test
time-based behaviour in automated test suites.

## Schedule types

- **`{once, When}`** — Run a job exactly once at the given time or
  after the given number of seconds.
- **`{daily, Period}`** — Run a job every day at the given time, list
  of times, or on a recurring schedule optionally bounded by a time
  window.
- **`{weekly, DOW, Period}`** — Run a job on the specified day(s) of
  the week. `DOW` is a day atom (`mon`..`sun`) or a list of day atoms.
- **`{monthly, DOM, Period}`** — Run a job on the specified day(s) of
  the month. Positive integers count from the start of the month; `0`
  and negative integers count backwards from the last day (`0` = last
  day, `-1` = second-to-last, and so on).
- **`CronExpr`** — A standard 5-field Unix cron expression as a string
  or binary (e.g. `"30 9 * * 1"`, `<<"*/5 * * * *">>`). The expression
  is parsed via `ecrn_util:from_cron/1` into an equivalent erlcron
  tuple schedule at job-submission time. See `ecrn_util:from_cron/1`
  for supported syntax and the mapping to erlcron schedule forms.

## When

`When` specifies a point in time. It is used as the argument to
`{once, When}` and to the convenience functions `at/2`, `daily/2`,
`weekly/3`, and `monthly/3`.

A `When` value can be any of:

| Form | Example | Meaning |
|------|---------|---------|
| `{H, am\|pm}` | `{3, pm}` | 3:00:00 PM (15:00:00) |
| `{H, M, am\|pm}` | `{3, 30, pm}` | 3:30:00 PM (15:30:00) |
| `{H, M, S, am\|pm}` | `{3, 30, 15, pm}` | 3:30:15 PM (15:30:15) |
| `{H, M, S}` | `{15, 30, 0}` | 24-hour clock time |
| `Seconds` | `3600` | Number of seconds from now (for `once` only) |

## Period

`Period` is used wherever a repeating or multi-time schedule is
needed (daily, weekly, monthly). It can take any of these forms:

| Form | Example | Meaning |
|------|---------|---------|
| `When` | `{3, 30, pm}` | Run once per day at the given time |
| `[When]` | `[{9, am}, {5, pm}]` | Run at each listed time per day |
| `{every, Duration}` | `{every, {30, min}}` | Repeat every duration, all day |
| `{every, Duration, {between, From, To}}` | `{every, {5, min}, {between, {9, am}, {5, pm}}}` | Repeat every duration within the time window |

Duration units: `hr` / `h`, `min` / `m`, `sec` / `s`.

## Quick start

```erlang
%% Start the application first
application:ensure_all_started(erlcron),

%% Run a fun once at 3:30 PM
erlcron:cron({{once, {3, 30, pm}}, fun() -> io:fwrite("Hello!~n") end}),

%% Run a fun every 5 minutes using a Unix cron expression
erlcron:cron({"*/5 * * * *", fun() -> poll() end}),

%% Run a fun every day at 9 AM, identified by a named reference
erlcron:daily(my_daily_job, {9, am}, fun() -> do_work() end),

%% Cancel the daily job
erlcron:cancel(my_daily_job).
```
""".

-export([validate/1,
         cron/1,   cron/2,    cron/3,    cron/4,
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
              job_opts/0,
              cron_opts/0,
              job_start/0,
              job_end/0,
              schedule/0,
              cron_expr/0,
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
-type constraint()  :: {between, cron_time(), cron_time()}.
-type duration()    :: {integer(), hr | h | min | m | sec | s}.
-type period()      :: cron_time() | {every, duration()}
                                   | {every, duration(), constraint()}.
-type dom()         :: integer().
-type dow_day()     :: mon | tue | wed | thu | fri | sat | sun.
-type dow()         :: dow_day() | [dow_day()].
-type callable()    :: fun(() -> term()) |
                       fun((JobRef::job_ref(), calendar:datetime()) -> term()).
-type task()        :: {M :: module(), F :: atom()} |
                       {M :: module(), F :: atom(), Args :: list()} |
                       callable().
-doc """
A 5-field Unix cron expression string or binary.

Examples: `\"*/5 * * * *\"`, `\"30 9 * * 1\"`, `~\"0 18 * * fri\"`.  
Parsed at job-submission time by `ecrn_util:from_cron/1`.
""".
-type cron_expr()   :: string() | binary().

-type schedule()    :: {once, cron_time()}
                     | {once, seconds()}
                     | {daily, period()}
                     | {weekly, dow(), period()}
                     | {monthly, dom()|[dom()], period()}
                     | cron_expr().

-type job()         :: {schedule(), task()}
                     | {schedule(), task(), job_opts()}
                     | #{id => job_ref(), schedule => schedule(), task => task(), _ => any()}.

%% should be opaque but dialyzer does not allow it
-doc """
A job reference. Can be a `reference()`, atom, or binary.

When the reference is an atom, the job process is locally registered
under that name, making it easy to cancel it by name later.
""".
-type job_ref()   :: reference() | atom() | binary().

-doc """
Called before a job is executed.

If the function returns `ignore`, the job will not be executed and
`on_job_end` will not be called.
""".
-type job_start() :: fun((JobRef::job_ref()) -> ignore | any()).

-doc """
Called after a job has finished executing.

`Res` is `{ok, Result}` on success, or `{error, {Reason, StackTrace}}`
if the job raised an exception.
""".
-type job_end()   :: fun((JobRef::job_ref(),
                         Res :: {ok, term()} | {error, {Reason::term(), Stack::list()}})
                       -> term()).

-doc """
Per-job options map.

- **`hostnames`** — List of hostnames on which the job is allowed to run.
  The job is silently ignored on any other host.
- **`id`** — An arbitrary identifier passed to `on_job_start` and
  `on_job_end` callbacks.
- **`on_job_start`** — `{Mod, Fun}` or a `fun/1` called before the job
  executes. Return `ignore` to skip this execution.
- **`on_job_end`** — `{Mod, Fun}` or a `fun/2` called after the job
  finishes. Receives `(JobRef, {ok, Result} | {error, {Reason, Stack}})`.
""".
-type job_opts()  ::
    #{hostnames    => [binary()|string()],
      id           => term(),
      on_job_start => {Mod::atom(), Fun::atom()} | job_start(),
      on_job_end   => {Mod::atom(), Fun::atom()} | job_end()
    }.

-doc "Alias for `t:job_opts/0`. Default options applied to all jobs in the crontab.".
-type cron_opts() :: job_opts().

%%%===================================================================
%%% API
%%%===================================================================

-doc "Validate a schedule spec without scheduling the job.".
-spec validate(schedule()) -> ok | {error, term()}.
validate(Spec) ->
    ecrn_agent:validate(Spec).

-doc """
Add a new job to the scheduler.

The job is described using a `t:job/0` spec. Returns the `t:job_ref/0`
that can be used to cancel the job later.
""".
-spec cron(job()) ->
        job_ref() | ignored | already_started | {error, term()}.
cron(Job) ->
    cron(make_ref(Job), Job, #{}).

-spec cron(job()|job_ref(), job()|cron_opts()|task()) ->
        job_ref() | ignored | already_started | {error, term()}.
cron(Job, DefOpts) when is_map(DefOpts) ->
    cron(make_ref(Job), Job, DefOpts);

cron(JobRef, Job) when (is_atom(JobRef) orelse is_reference(JobRef) orelse is_binary(JobRef)) ->
    cron(JobRef, Job, #{});

%% cron(Sched, Task) — schedule without an explicit job reference
cron(Sched, Task) ->
    cron(make_ref(), {Sched, Task}, #{}).

-doc """
Schedule a job identified by `JobRef`.

The reference can be a `reference()`, atom, or binary. When it is an
atom the job process is locally registered under that name.
Returns `ignored` when the job is not permitted to run on the current host.

When called as `cron(Sched, Task, Opts)` a fresh reference is generated
automatically and `Opts` is a `t:job_opts/0` map.
""".
-spec cron(job_ref(), job(), job_opts()) ->
        job_ref() | ignored | already_started | {error, term()}.
cron(JobRef, Job, Opts) when (is_atom(JobRef) orelse is_reference(JobRef) orelse is_binary(JobRef))
                           , is_map(Opts) ->
    ecrn_cron_sup:add_job(JobRef, Job, Opts);

%% cron(Sched, Task, Opts) — schedule without an explicit job reference
cron(Sched, Task, Opts) when is_map(Opts) ->
    cron(make_ref(), {Sched, Task}, Opts).

-doc """
Schedule a job with an explicit reference, schedule, task, and options.

```erlang
erlcron:cron(my_job, {daily, {9, am}}, fun() -> ok end, #{}).
erlcron:cron(my_job, \"30 9 * * 1-5\", fun() -> standup() end, #{}).
```
""".
-spec cron(job_ref(), schedule(), task(), job_opts()) ->
        job_ref() | ignored | already_started | {error, term()}.
cron(JobRef, Sched, Task, Opts) when (is_atom(JobRef) orelse is_reference(JobRef) orelse is_binary(JobRef))
                                   , is_map(Opts) ->
    ecrn_cron_sup:add_job(JobRef, {Sched, Task}, Opts).

-doc """
Schedule a job to run once at `When`.

`When` can be a `t:cron_time/0` (e.g. `{3, 30, pm}`) or an integer
number of seconds from now.
""".
-spec at(cron_time() | seconds(), task()) ->
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

-doc "Schedule a job to run every day at `When`.".
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

-doc "Schedule a job to run every week on `DOW` (day-of-week) at `When`.".
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

-doc """
Schedule a job to run every month on day `DOM` at `When`.

Positive `DOM` values count from the start of the month; `0` means the
last day of the month, and negative values count backwards from the last
day (e.g. `-1` is the second-to-last day).
""".
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

-doc "Cancel the job identified by `JobRef`. Returns `true` if found, `false` otherwise.".
-spec cancel(job_ref()) -> boolean().
cancel(JobRef) ->
    ecrn_control:cancel(JobRef).

-doc "Return the current erlcron time as milliseconds since the Unix epoch.".
-spec epoch() -> milliseconds().
epoch() ->
    ecrn_util:epoch_milliseconds().

-doc "Return the current erlcron time as seconds since the Unix epoch.".
-spec epoch_seconds() -> seconds().
epoch_seconds() ->
    ecrn_util:epoch_seconds().

-doc """
Return the current erlcron datetime (universal) together with the
corresponding millisecond epoch value.
""".
-spec datetime() -> {calendar:datetime(), milliseconds()}.
datetime() ->
    ecrn_control:datetime().

-doc "Return the reference datetime used as the scheduler's clock base.".
-spec ref_datetime() -> {calendar:datetime(), milliseconds()}.
ref_datetime() ->
    ecrn_control:ref_datetime().

-doc "Set the scheduler clock to `DateTime` (local time).".
-spec set_datetime(calendar:datetime()) -> ok.
set_datetime(DateTime) ->
    set_datetime(DateTime, local).

-doc """
Set the scheduler clock to `DateTime`.

`TZ` controls how `DateTime` is interpreted: `local` (default) or
`universal`. When the clock is advanced, all jobs scheduled between the
old and new time are executed immediately.
""".
-spec set_datetime(calendar:datetime(), local|universal) -> ok.
set_datetime({D,T} = DateTime, TZ) when tuple_size(D)==3, tuple_size(T)==3 ->
    ecrn_control:set_datetime(DateTime, TZ).

-doc "Reset the scheduler clock to the real system time.".
-spec reset_datetime() -> ok.
reset_datetime() ->
    ecrn_control:reset_datetime().

-doc "Return the references of all currently running jobs.".
-spec get_all_jobs() -> [job_ref()].
get_all_jobs() ->
    ecrn_reg:get_all_refs().

-doc "Set the scheduler clock on all connected nodes. See `multi_set_datetime/2`.".
-spec multi_set_datetime(calendar:datetime()) -> {Replies, BadNodes} when
    Replies :: [{node(), ok | {error, term()}}],
    BadNodes :: [node()].
multi_set_datetime({D,T} = DateTime) when tuple_size(D)==3, tuple_size(T)==3 ->
    ecrn_control:multi_set_datetime([node()|nodes()], DateTime).

-doc """
Set the scheduler clock to `DateTime` on the given `Nodes`.

Any jobs scheduled between the old and new time on each node are
executed immediately. Returns `{Replies, BadNodes}` in the same format
as `gen_server:multi_call/3`.
""".
-spec multi_set_datetime([node()], calendar:datetime()) -> {Replies, BadNodes} when
    Replies :: [{node(), ok | {error, term()}}],
    BadNodes :: [node()].
multi_set_datetime(Nodes, DateTime) when is_list(Nodes), tuple_size(DateTime)==2 ->
    ecrn_control:multi_set_datetime(Nodes, DateTime).

make_ref(#{id := ID}) when is_atom(ID); is_binary(ID); is_reference(ID) ->
    ID;
make_ref({_When, _Callable, #{id := ID}}) when is_atom(ID); is_binary(ID); is_reference(ID) ->
    ID;
make_ref({_When, _Callable, #{}}) ->
    make_ref();
make_ref({_When, _Callable}) ->
    make_ref().
