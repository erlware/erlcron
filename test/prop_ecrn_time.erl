-module(prop_ecrn_time).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

%% A weekly job should always run on the provided day of the week
prop_same_day_of_week() ->
    ?FORALL({RefDate, When}, {ref_date(), ecrn_weekly_when()},
            begin
                {PlannedDate, _} = next_time(RefDate, When),
                resolve_day_of_week(When) =:= calendar:day_of_the_week(PlannedDate)
            end).

%% A daily job should run within 24 hours of the reference
prop_daily_within_24h() ->
    ?FORALL({RefDate, When}, {ref_date(), ecrn_daily_when()},
            begin
                24 * 3600 > ecrn_time:until_next_time(RefDate, When)
            end).

%% A job should always run at the given time
prop_match_given_time() ->
    ?FORALL({RefDate, When}, {ref_date(), ecrn_when()},
            begin
                {_, PlannedTime} = next_time(RefDate, When),
                TimeSpec = timespec(When),
                resolve_time_spec(TimeSpec) =:= PlannedTime
            end).

%% A job should always run at or after the "current" moment
prop_after_reference() ->
    ?FORALL({RefDate, When}, {ref_date(), ecrn_when()},
            begin
                RefDate =< next_time(RefDate, When)
            end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
-spec next_time(calendar:datetime(), erlcron:run_when()) -> calendar:datetime().
next_time(RefDate, When) ->
    O = ecrn_time:until_next_time(RefDate, When),
    S = calendar:datetime_to_gregorian_seconds(RefDate),
    calendar:gregorian_seconds_to_datetime(S + O).

-spec resolve_day_of_week({any(), erlcron:dow(), any()}) -> calendar:daynum().
resolve_day_of_week({_, mon, _}) ->
    1;
resolve_day_of_week({_, tue, _}) ->
    2;
resolve_day_of_week({_, wed, _}) ->
    3;
resolve_day_of_week({_, thu, _}) ->
    4;
resolve_day_of_week({_, fri, _}) ->
    5;
resolve_day_of_week({_, sat, _}) ->
    6;
resolve_day_of_week({_, sun, _}) ->
    7.

-spec resolve_time_spec(erlcron:cron_time()) -> calendar:time().
resolve_time_spec(T = {_, _, S}) when is_integer(S) ->
    T;
resolve_time_spec({H, M, S, AmOrPm}) ->
    {resolve_hour(H, AmOrPm), M, S};
resolve_time_spec({H, M, AmOrPm}) ->
    {resolve_hour(H, AmOrPm), M, 0};
resolve_time_spec({H, AmOrPm}) ->
    {resolve_hour(H, AmOrPm), 0, 0}.

-spec resolve_hour(integer(), am | pm) -> integer().
resolve_hour(12, am) ->
    0;
resolve_hour(X, am) ->
    X;
resolve_hour(12, pm) ->
    12;
resolve_hour(X, pm) ->
    X + 12.

-spec timespec({daily, erlcron:cron_time()}) -> erlcron:cron_time();
              ({weekly, erlcron:dow(), erlang:cron_time()}) -> erlcron:cron_time().
timespec({daily, T}) ->
    T;
timespec({weekly, _, T}) ->
    T.


%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
ref_date() ->
    { oneof([ exactly({2018, 1, 15})
            , exactly({2018, 1, 18})
            , exactly({2018, 1, 21})
            ])
    , oneof([ exactly({0, 0, 0})
            , exactly({12, 30, 30})
            , exactly({23, 59, 59})
            ]
           )
    }.

ecrn_when() ->
    oneof([ ecrn_daily_when()
          , ecrn_weekly_when()
          ]).

ecrn_daily_when() ->
    { exactly(daily)
    , ecrn_time()
    }.

ecrn_weekly_when() ->
    { exactly(weekly)
    , day_of_week()
    , ecrn_time()
    }.

ecrn_time() ->
    oneof([ {hour24(), minute(), second()}
          , {hour12(), minute(), second(), am_or_pm()}
          , {hour12(), minute(), am_or_pm()}
          , {hour12(), am_or_pm()}
          ]).

second() ->
    integer(0, 59).

minute() ->
    integer(0, 59).

hour24() ->
    integer(0, 23).

hour12() ->
    integer(0, 12).

am_or_pm() ->
    oneof([exactly(am), exactly(pm)]).

day_of_week() ->
    oneof([ exactly(mon)
          , exactly(tue)
          , exactly(wed)
          , exactly(thu)
          , exactly(fri)
          , exactly(sat)
          , exactly(sun)
          ]).
