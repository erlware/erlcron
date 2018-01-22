-module(ecrn_time).

-export([until_next_time/2]).

%% @doc Calculates the duration in seconds until the next time
%% a job is to be run.
-spec until_next_time(calendar:datetime(), erlcron:run_when()) ->
                             erlcron:seconds().
until_next_time(_Date, {once, Seconds}) when is_integer(Seconds) ->
    Seconds;
until_next_time(Date, {once, {H, M, S}})
  when is_integer(H), is_integer(M), is_integer(S) ->
    normalize_seconds(Date, S + (M + (H * 60)) * 60);
until_next_time(Date, {once, Period})  ->
    normalize_seconds(Date, resolve_time(Period));
until_next_time(Date, {daily, Period}) ->
    until_next_daytime(Date, Period);
until_next_time(DateTime = {Date, _}, {weekly, DoW, Period}) ->
    OnDay = resolve_dow(DoW),
    Today = calendar:day_of_the_week(Date),
    case Today of
        OnDay ->
            until_next_daytime_or_days_from_now(DateTime, Period, 7);
        Today when Today < OnDay ->
            until_days_from_now(DateTime, Period, OnDay - Today);
        Today when Today > OnDay  ->
            until_days_from_now(DateTime, Period, (OnDay+7) - Today)
    end;
until_next_time(Date, {monthly, DoM, Period}) ->
    {{ThisYear, ThisMonth, Today}, _} = Date,
    {NextYear, NextMonth} =
        case ThisMonth of
            12 ->
                {ThisYear + 1, 1};
            _  ->
                {ThisYear, ThisMonth + 1}
        end,
    D1 = calendar:date_to_gregorian_days(ThisYear, ThisMonth, Today),
    D2 = calendar:date_to_gregorian_days(NextYear, NextMonth, DoM),
    Days = D2 - D1,
    case Today of
        DoM ->
            until_next_daytime_or_days_from_now(Date, Period, Days);
        _ ->
            until_days_from_now(Date, Period, Days)
    end.

%% @doc Calculates the duration in seconds until the next time this
%% period is to occur during the day.
-spec until_next_daytime(calendar:datetime(), erlcron:period()) -> erlcron:seconds().
until_next_daytime(Date, Period) ->
    StartTime = first_time(Period),
    EndTime = last_time(Period),
    case current_time(Date) of
        T when T > EndTime ->
            until_tomorrow(Date, StartTime);
        T ->
            next_time(Period, T) - T
    end.

%% @doc Calculates the last time in a given period.
-spec last_time(erlcron:period()) -> erlcron:seconds().
last_time(Period) ->
    hd(lists:reverse(lists:sort(resolve_period(Period)))).


%% @doc Calculates the first time in a given period.
-spec first_time(erlcron:period()) -> erlcron:seconds().
first_time(Period) ->
    hd(lists:sort(resolve_period(Period))).

%% @doc Calculates the first time in the given period after the given time.
-spec next_time(erlcron:period(), erlcron:seconds()) -> erlcron:seconds().
next_time(Period, Time) ->
    R = lists:sort(resolve_period(Period)),
    lists:foldl(fun(X, A) ->
                        case X of
                            T when T >= Time, T < A ->
                                T;
                            _ ->
                                A
                        end
                end, 24*3600, R).

%% @doc Returns a list of times given a periodic specification.
-spec resolve_period([erlcron:period()] | erlcron:period()) -> [erlcron:seconds()].
resolve_period([]) ->
    [];
resolve_period([H | T]) ->
    resolve_period(H) ++ resolve_period(T);
resolve_period({every, Duration, {between, TimeA, TimeB}}) ->
    Period = resolve_dur(Duration),
    StartTime = resolve_time(TimeA),
    EndTime = resolve_time(TimeB),
    resolve_period0(Period, StartTime, EndTime, []);
resolve_period(Time) ->
    [resolve_time(Time)].

resolve_period0(_, Time, EndTime, Acc) when Time >= EndTime ->
    Acc;
resolve_period0(Period, Time, EndTime, Acc) ->
    resolve_period0(Period, Time + Period, EndTime, [Time | Acc]).

%% @doc Returns seconds past midnight for a given time.
-spec resolve_time(erlcron:cron_time()) -> erlcron:seconds().
resolve_time({H, M, S}) when H < 24, M < 60, S < 60  ->
    S + M * 60 + H * 3600;
resolve_time({H, M, S, X}) when  H =< 12, M < 60, S < 60, is_atom(X) ->
    resolve_time({H, X}) + M * 60 + S;
resolve_time({H, M, X}) when  H =< 12, M < 60, is_atom(X) ->
    resolve_time({H, X}) + M * 60;
resolve_time({12, am}) ->
    0;
resolve_time({H,  am}) when H < 12 ->
    H * 3600;
resolve_time({12, pm}) ->
    12 * 3600;
resolve_time({H,  pm}) when H < 12->
    (H + 12) * 3600.

%% @doc Returns seconds for a given duration.
-spec resolve_dur(erlcron:duration()) -> erlcron:seconds().
resolve_dur({Hour, hr}) ->
    Hour * 3600;
resolve_dur({Min, min}) ->
    Min * 60;
resolve_dur({Sec, sec}) ->
    Sec.

%% @doc Returns the number of the given day of the week. See the calendar
%% module for day numbers.
-spec resolve_dow(erlcron:dow()) -> integer().
resolve_dow(mon) ->
    1;
resolve_dow(tue) ->
    2;
resolve_dow(wed) ->
    3;
resolve_dow(thu) ->
    4;
resolve_dow(fri) ->
    5;
resolve_dow(sat) ->
    6;
resolve_dow(sun) ->
    7.

%% @doc Calculates the duration in seconds until the given time occurs
%% tomorrow.
-spec until_tomorrow(calendar:datetime(), erlcron:seconds()) -> erlcron:seconds().
until_tomorrow(Date, StartTime) ->
    (StartTime + 24*3600) - current_time(Date).

%% @doc Calculates the duration in seconds until the given period
%% occurs several days from now.
-spec until_days_from_now(calendar:datetime(), erlcron:period(), integer()) ->
                                 erlcron:seconds().
until_days_from_now(Date, Period, Days) ->
    CurrentTime = current_time(Date),
    case last_time(Period) of
        T when T < CurrentTime ->
            (Days - 1) * 24 * 3600 + until_next_daytime(Date, Period);
        _ ->
            Days * 24 * 3600 + until_next_daytime(Date, Period)
    end.

%% @doc Calculates the duration in seconds until the given period
%% occurs, which may be today or several days from now.
-spec until_next_daytime_or_days_from_now(calendar:datetime(), erlcron:period(), integer()) ->
                                                 erlcron:seconds().
until_next_daytime_or_days_from_now(Date, Period, Days) ->
    CurrentTime = current_time(Date),
    case last_time(Period) of
        T when T < CurrentTime ->
            until_days_from_now(Date, Period, Days);
        _ ->
            until_next_daytime(Date, Period)
    end.

normalize_seconds(Date, Seconds) ->
    case Seconds - current_time(Date) of
        Value when Value >= 0 ->
            Value;
        _ ->
            erlang:display(erlang:get_stacktrace()),
            throw(invalid_once_exception)
    end.

-spec current_time(calendar:datetime()) -> erlcron:seconds().
current_time({_, {H,M,S}}) ->
    S + M * 60 + H * 3600.

