%%% @copyright Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the BSD License; you may not use
%%% this file except in compliance with the License.
%%%-------------------------------------------------------------------
-module(ecrn_util).

-moduledoc """
Utility functions for epoch time conversion used internally by erlcron.

All epoch values are in milliseconds unless the function name contains
`_seconds`.
""".

-export([epoch_seconds/0, epoch_milliseconds/0]).
-export([epoch_to_time_string/1, epoch_to_datetime_string/1]).
-export([universaltime_to_epoch/1, localtime_to_epoch/1]).
-export([parse_schedule/1, from_cron/1]).

%%%===================================================================
%%% API
%%%===================================================================

-doc "Return the current time as seconds since the Unix epoch.".
-spec epoch_seconds() -> erlcron:seconds().
epoch_seconds() ->
    erlang:system_time(seconds).

-doc "Return the current time as milliseconds since the Unix epoch.".
-spec epoch_milliseconds() -> erlcron:milliseconds().
epoch_milliseconds() ->
    erlang:system_time(millisecond).

-doc """
Format an epoch millisecond timestamp as a `"HH:MM:SS.mmm"` string in local time.
""".
-spec epoch_to_time_string(erlcron:milliseconds()) -> string().
epoch_to_time_string(Epoch) when is_integer(Epoch) ->
    DT = erlang:posixtime_to_universaltime(Epoch div 1000),
    {_, {H,M,S}} = erlang:universaltime_to_localtime(DT),
    fmt("~.2.0w:~.2.0w:~.2.0w.~.3.0w", [H,M,S, Epoch rem 1000]).

-doc """
Format an epoch millisecond timestamp as a `"YYYY-MM-DD HH:MM:SS.mmm"` string in local time.
""".
-spec epoch_to_datetime_string(erlcron:milliseconds()) -> string().
epoch_to_datetime_string(Epoch) when is_integer(Epoch) ->
    DT = erlang:posixtime_to_universaltime(Epoch div 1000),
    {{Y,Mo,D}, {H,M,S}} = erlang:universaltime_to_localtime(DT),
    fmt("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w.~.3.0w",
        [Y,Mo,D,H,M,S, Epoch rem 1000]).

-doc "Convert a universal `calendar:datetime()` to milliseconds since the Unix epoch.".
-spec universaltime_to_epoch(calendar:datetime()) -> erlcron:milliseconds().
universaltime_to_epoch(DT) ->
    erlang:universaltime_to_posixtime(DT)*1000.

-doc "Convert a local `calendar:datetime()` to milliseconds since the Unix epoch.".
-spec localtime_to_epoch(calendar:datetime()) -> erlcron:milliseconds().
localtime_to_epoch(DT) ->
    erlang:universaltime_to_posixtime(erlang:localtime_to_universaltime(DT))*1000.

fmt(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).

-doc """
Parse an erlcron schedule specification from a cron expression string
or a tuple representing an erlcron schedule.
Returns `{ok, Schedule}` on success or `{error, Reason}` on failure.
The supported cron syntax is a subset of standard Unix cron expressions,
with some extensions and restrictions. See `from_cron/1` for details.
""".
-spec parse_schedule(erlcron:schedule()) ->
    {ok, erlcron:schedule()} | {error, string()}.
parse_schedule(Schedule) when is_tuple(Schedule) ->
    case ecrn_agent:validate(Schedule) of
        ok -> {ok, Schedule};
        {error, Reason} -> {error, {invalid_schedule, Reason}}
    end;
parse_schedule(Expr) when is_binary(Expr) ->
    from_cron(Expr);
parse_schedule(Expr) when is_list(Expr) ->
    from_cron(Expr).

%%%===================================================================
%%% Cron-expression → erlcron schedule conversion
%%%===================================================================

-doc """
Converts standard 5-field Unix cron expressions to erlcron schedule specs.

Returns `{ok, Schedule}` on success or `{error, Reason}` on failure.

## Cron field order

    ┌───────── minute      (0–59)
    │ ┌─────── hour        (0–23)
    │ │ ┌───── day-of-month (1–31)
    │ │ │ ┌─── month       (1–12, ignored – erlcron has no month scope)
    │ │ │ │ ┌─ day-of-week  (0–7, 0/7=Sun; or mon/tue/wed/thu/fri/sat/sun)
    │ │ │ │ │
    * * * * *

## Supported field syntax

| Syntax   | Meaning               |
|----------|-----------------------|
| `*`      | every value           |
| `N`      | specific value        |
| `*/N`    | every N steps         |
| `N-M`    | inclusive range       |
| `a,b,c`  | list of values        |

## Supported schedule shapes and the erlcron spec they produce

| Cron            | erlcron                            |
|-----------------|------------------------------------|
| `* * * * *`     | `{:daily, {:every, {1, :min}}}`     |
| `*/5 * * * *`   | `{:daily, {:every, {5, :min}}}`     |
| `0 * * * *`     | `{:daily, {:every, {1, :hr}}}`      |
| `0 */2 * * *`   | `{:daily, {:every, {2, :hr}}}`      |
| `30 9 * * *`    | `{:daily, {9, 30, 0}}`              |
| `0,30 9 * * *`  | `{:daily, [{9, 0, 0}, {9, 30, 0}]}`|
| `0 9 * * 1`     | `{:weekly, :mon, {9, 0, 0}}`        |
| `0 9 * * 1,3`   | `{:weekly, [:mon, :wed], {9, 0, 0}}`|
| `0 9 1 * *`     | `{:monthly, 1, {9, 0, 0}}`          |
| `0 9 1,15 * *`  | `{:monthly, [1, 15], {9, 0, 0}}`   |

Note: the month field is accepted but ignored (erlcron has no month-level scope).
Expressions specifying both DOM and DOW simultaneously are rejected.

The month field is accepted but ignored (erlcron has no month-level scope).
Expressions specifying both DOM and DOW simultaneously are rejected.

## Examples

```
{ok,{daily,{every,{5,min}}}} = ecrn_util:from_cron("*/5 * * * *")
{ok,{daily,{9,30,0}}}        = ecrn_util:from_cron("30 9 * * *")
{ok,{weekly,mon,{9,0,0}}}    = ecrn_util:from_cron("0 9 * * 1")
{ok,{monthly,1,{9,0,0}}}     = ecrn_util:from_cron("0 9 1 * *")
```
""".
-spec from_cron(string() | binary()) -> {ok, tuple()} | {error, string()}.
from_cron(Expr) when is_binary(Expr) ->
    from_cron(binary_to_list(Expr));
from_cron(Expr) when is_list(Expr) ->
    case string:tokens(Expr, " \t\n\r") of
        [MinS, HourS, DomS, _MonthS, DowS] ->
            case cron_build_period(MinS, HourS) of
                {ok, Period} -> cron_build_scope(DomS, DowS, Period);
                Err -> Err
            end;
        _ ->
            {error, "Invalid cron expression: expected 5 fields, got: " ++ Expr}
    end.

%% --- Period (sub-daily repetition) -------------------------------------

%% * * → every minute
cron_build_period("*", "*") ->
    {ok, {every, {1, min}}};
%% */N * → every N minutes
cron_build_period("*/" ++ N, "*") ->
    case cron_parse_pos_int(N) of
        {ok, V} -> {ok, {every, {V, min}}};
        Err -> Err
    end;
%% 0 * → every hour on the hour
cron_build_period(Zero, "*") when Zero =:= "0"; Zero =:= "00" ->
    {ok, {every, {1, hr}}};
%% 0 */N → every N hours
cron_build_period(Zero, "*/" ++ N) when Zero =:= "0"; Zero =:= "00" ->
    case cron_parse_pos_int(N) of
        {ok, V} -> {ok, {every, {V, hr}}};
        Err -> Err
    end;
%% M H → one or more specific times; expand both fields and produce time tuples
cron_build_period(MinS, HourS) ->
    case cron_parse_field(HourS, 0, 23) of
        {ok, Hours} ->
            case cron_parse_field(MinS, 0, 59) of
                {ok, Mins} ->
                    Times = [{H, M, 0} || H <- Hours, M <- Mins],
                    case Times of
                        [T]  -> {ok, T};
                        List -> {ok, List}
                    end;
                Err -> Err
            end;
        Err -> Err
    end.

%% --- Scope (daily / weekly / monthly) ----------------------------------

cron_build_scope("*", "*", Period) ->
    {ok, {daily, Period}};
cron_build_scope("*", DowS, Period) ->
    case cron_parse_dow(DowS) of
        {ok, Dow} -> {ok, {weekly, Dow, Period}};
        Err -> Err
    end;
cron_build_scope(DomS, "*", Period) ->
    case cron_parse_field(DomS, 1, 31) of
        {ok, Doms} ->
            Dom = case Doms of [D] -> D; Ds -> Ds end,
            {ok, {monthly, Dom, Period}};
        Err -> Err
    end;
cron_build_scope(_, _, _) ->
    {error, "Cron expressions specifying both DOM and DOW are not supported"}.

%% --- Field parsing -----------------------------------------------------

cron_parse_field("*", Lo, Hi) ->
    {ok, lists:seq(Lo, Hi)};
cron_parse_field("*/" ++ Step, Lo, Hi) ->
    case cron_parse_pos_int(Step) of
        {ok, S} -> {ok, lists:seq(Lo, Hi, S)};
        Err -> Err
    end;
cron_parse_field(Expr, Lo, Hi) ->
    Parts = string:split(Expr, ",", all),
    cron_parse_field_parts(Parts, Lo, Hi, []).

cron_parse_field_parts([], _Lo, _Hi, Acc) ->
    {ok, lists:usort(Acc)};
cron_parse_field_parts([Part | Rest], Lo, Hi, Acc) ->
    case cron_parse_field_part(Part, Lo, Hi) of
        {ok, Vals} -> cron_parse_field_parts(Rest, Lo, Hi, Acc ++ Vals);
        Err -> Err
    end.

cron_parse_field_part(Expr, Lo, Hi) ->
    case string:split(Expr, "-", leading) of
        [A, B] ->
            case {cron_parse_non_neg_int(A), cron_parse_non_neg_int(B)} of
                {{ok, RLo}, {ok, RHi}} when RLo >= Lo, RHi =< Hi, RLo =< RHi ->
                    {ok, lists:seq(RLo, RHi)};
                _ ->
                    {error, "Range " ++ Expr ++ " is out of bounds " ++
                            integer_to_list(Lo) ++ ".." ++ integer_to_list(Hi)}
            end;
        [N] ->
            case cron_parse_non_neg_int(N) of
                {ok, V} when V >= Lo, V =< Hi ->
                    {ok, [V]};
                _ ->
                    {error, "Value " ++ N ++ " is out of range " ++
                            integer_to_list(Lo) ++ ".." ++ integer_to_list(Hi)}
            end
    end.

%% --- Day-of-week parsing -----------------------------------------------

cron_parse_dow(Expr) ->
    Parts = string:split(Expr, ",", all),
    cron_parse_dow_parts(Parts, []).

cron_parse_dow_parts([], []) ->
    {error, "Empty day-of-week field"};
cron_parse_dow_parts([], [Day]) ->
    {ok, Day};
cron_parse_dow_parts([], Days) ->
    {ok, lists:reverse(Days)};
cron_parse_dow_parts([Part | Rest], Acc) ->
    case cron_day_of_week(string:lowercase(Part)) of
        {ok, Day} -> cron_parse_dow_parts(Rest, [Day | Acc]);
        error     -> {error, "Unknown day of week: " ++ Part}
    end.

cron_day_of_week("0")   -> {ok, sun};
cron_day_of_week("1")   -> {ok, mon};
cron_day_of_week("2")   -> {ok, tue};
cron_day_of_week("3")   -> {ok, wed};
cron_day_of_week("4")   -> {ok, thu};
cron_day_of_week("5")   -> {ok, fri};
cron_day_of_week("6")   -> {ok, sat};
cron_day_of_week("7")   -> {ok, sun};
cron_day_of_week("sun") -> {ok, sun};
cron_day_of_week("mon") -> {ok, mon};
cron_day_of_week("tue") -> {ok, tue};
cron_day_of_week("wed") -> {ok, wed};
cron_day_of_week("thu") -> {ok, thu};
cron_day_of_week("fri") -> {ok, fri};
cron_day_of_week("sat") -> {ok, sat};
cron_day_of_week(_)     -> error.

%% --- Integer helpers ---------------------------------------------------

cron_parse_pos_int(S) ->
    try list_to_integer(S) of
        N when N > 0 -> {ok, N};
        _            -> {error, "Expected a positive integer, got: " ++ S}
    catch
        _:_ -> {error, "Expected a positive integer, got: " ++ S}
    end.

cron_parse_non_neg_int(S) ->
    try list_to_integer(S) of
        N when N >= 0 -> {ok, N};
        _             -> {error, "Expected a non-negative integer, got: " ++ S}
    catch
        _:_ -> {error, "Expected a non-negative integer, got: " ++ S}
    end.
