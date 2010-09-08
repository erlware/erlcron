
-type seconds()    :: integer().
-type date()       :: {integer(), integer(), integer()}.
-type time()       :: {integer(), integer(), integer()}.
-type datetime()   :: {date(), time()}.

-type cron_time()   :: {integer(), am | pm} | {integer(), integer(), am | pm}.
-type constraint() :: {between, cron_time(), cron_time()}.
-type duration()   :: {integer(), hr | min | sec}.
-type period()     :: cron_time() | [cron_time()] | {every, duration(), constraint()}.
-type dom()        :: integer().
-type dow()        :: mon | tue | wed | thu | fri | sat | sun.
-type callable()   :: mfa() | function().
-type run_when()   :: {daily, period()}
                          | {weekly, dow(), period()}
                          | {monthly, dom(), period()}.

-type  job()      :: {run_when(), callable()}.
-opaque job_ref()   :: {integer(), reference()}.
