%%% @copyright Erlware, LLC. All Rights Reserved.
%%% vim:ts=4:ts=4:et
%%%
%%% This file is provided to you under the BSD License; you may not use
%%% this file except in compliance with the License.
-module(ecrn_test).
-compile(export_all).
-compile(nowarn_export_all).

-import(ecrn_startup_test, [disable_sasl_logger/0, enable_sasl_logger/0]).

-include_lib("eunit/include/eunit.hrl").
-include("internal.hrl").

-define(FuncTest(A), {atom_to_list(A), fun A/0}).

%%%===================================================================
%%% Types
%%%===================================================================
cron_test_() ->
    {setup,
     fun() ->
        application:load(erlcron),
        application:set_env(erlcron, sup_intensity, 0),
        application:set_env(erlcron, sup_period,    1),
        application:unset_env(erlcron, crontab),
        disable_sasl_logger(),
        application:start(erlcron)
     end,
     fun(_) ->
        application:stop(erlcron),
        enable_sasl_logger()
     end,
     [{timeout, 30, [
       ?FuncTest(set_alarm),
       ?FuncTest(start_stop_fun1),
       ?FuncTest(start_stop_fun2),
       ?FuncTest(start_stop_fun3),
       ?FuncTest(travel_back_in_time),
       ?FuncTest(cancel_alarm),
       ?FuncTest(big_time_jump),
       ?FuncTest(cron),
       ?FuncTest(cron_run_job_on_host),
       ?FuncTest(cron_skip_job_on_host),
       ?FuncTest(validation),
       ?FuncTest(info_job),
       ?FuncTest(start_link4),
       ?FuncTest(map_job_schedule_task)
      ]},
      {timeout, 30, [
       ?FuncTest(weekly)
      ]},
      {timeout, 30, [
       ?FuncTest(weekly_every)
      ]}
     ]}.

set_alarm() ->
    erlcron:set_datetime({{2000,1,1},{8,0,0}}),

    Self = self(),

    erlcron:at(test1, {9,0,0}, fun() -> Self ! ack1 end),
    erlcron:at(test2, {9,0,1}, fun() -> Self ! ack2 end),
    erlcron:daily(test3, {every, {1,s}, {between, {9,0,2}, {9,0,4}}},
                         fun() -> Self ! ack3 end),

    erlcron:set_datetime({{2000,1,1},{8,59,59}}),

    %% The alarm should trigger this nearly immediately.
    ?assertMatch(1, collect(ack1, 1500, 1)),

    %% The alarm should trigger this 1 second later.
    ?assertMatch(1, collect(ack2, 2500, 1)),

    %% The alarm should trigger this 1 second later.
    ?assertMatch(3, collect(ack3, 3000, 3)),

    erlcron:cancel(test3).

cancel_alarm() ->
    Day = {2000,1,1},
    erlcron:set_datetime({Day,{8,0,0}}),
    AlarmTimeOfDay = {9,0,0},

    Self = self(),

    Ref = erlcron:at(AlarmTimeOfDay, fun(_, _) -> Self ! ack end),
    erlcron:cancel(Ref),
    erlcron:set_datetime({Day, AlarmTimeOfDay}),
    ?assertMatch(0, collect(ack, 125, 1)).

start_stop_fun1() ->
    Day = {2000,1,1},
    erlcron:set_datetime({Day,{8,0,0}}),
    AlarmTimeOfDay = {8,0,1},

    Self = self(),
    Opts = #{on_job_start => fun(Ref)      -> Self ! {start,  Ref} end,
             on_job_end   => fun(Ref, Res) -> Self ! {finish, Ref, Res} end},
    Ref1 = erlcron:at(test1, AlarmTimeOfDay, fun(_, _) -> Self ! ack, 1234 end, Opts),
    ?assertEqual(test1, Ref1),
    ?assertMatch(1, collect({start, test1}, 1500, 1)),
    ?assertMatch(1, collect(ack, 125, 1)),
    ?assertMatch(1, collect({finish, test1, {ok, 1234}}, 1500, 1)).

start_stop_fun2() ->
    Day = {2000,1,1},
    erlcron:set_datetime({Day,{8,0,0}}),
    AlarmTimeOfDay = {8,0,1},

    Self = self(),
    Opts2 = #{on_job_start => fun(Ref)      -> Self ! {ignored, Ref}, ignore end,
              on_job_end   => fun(Ref, Res) -> Self ! {finish,  Ref, Res}    end},
    Ref2  = erlcron:at(test2, AlarmTimeOfDay, fun(_, _) -> timer:sleep(60000), Self ! ack, 1000 end, Opts2),

    ?assertEqual(test2, Ref2),
    ?assertMatch(1, collect({ignored, test2}, 1500, 1)),
    ?assertMatch(0, collect({finish, test2, {ok, 1000}}, 125, 1)),
    ?assertEqual(undefined, ecrn_reg:get(test2)).

start_stop_fun3() ->
    Day = {2000,1,1},
    erlcron:set_datetime({Day,{8,0,0}}),
    AlarmTimeOfDay = {8,0,1},

    Self = self(),
    Opts = #{on_job_start => fun(Ref)      -> Self ! {start,  Ref} end,
             on_job_end   => fun(Ref, Res) -> Self ! {finish, Ref, Res} end},
    Len3 = length(lists:seq(1, rand:uniform(10)+2)),
    Ref3 = erlcron:at(test3, AlarmTimeOfDay, fun(_, _) -> 1 = Len3 end, Opts),

    ?assertEqual(test3, Ref3),
    ?assertMatch(1, collect({start, test3}, 1500, 1)),
    ?assertMatch(0, collect(1, 125, 1)),
    ?assertMatch({finish, test3, {error,{{badmatch, _}, [_|_]}}}, receive _M -> _M after 1500 -> timeout end).

%% Time jumps ahead one day so we should see the alarms from both days.
big_time_jump() ->
    Day1 = {2000,1,1},
    Day2 = {2000,1,2},
    EpochDateTime = {Day1,{8,0,0}},
    erlcron:set_datetime(EpochDateTime),
    Alarm1TimeOfDay = {9,0,0},
    Alarm2TimeOfDay = {9,0,1},

    Self = self(),

    erlcron:daily(Alarm1TimeOfDay, fun(_, _) -> Self ! ack1 end),
    erlcron:daily(Alarm2TimeOfDay, fun(_, _) -> Self ! ack2 end),
    erlcron:set_datetime({Day2, {9, 10, 0}}),
    ?assertMatch(1, collect(ack1, 1500, 1)),
    ?assertMatch(1, collect(ack2, 1500, 1)),
    ?assertMatch(1, collect(ack1, 1500, 1)),
    ?assertMatch(1, collect(ack2, 1500, 1)).

travel_back_in_time() ->
    Seconds = seconds_now(),
    Past = {{2000,1,1},{12,0,0}},
    erlcron:set_datetime(Past, universal),
    {ExpectedDateTime, _} = erlcron:datetime(),
    ExpectedSeconds = calendar:datetime_to_gregorian_seconds(ExpectedDateTime),
    ?assertMatch(true, ExpectedSeconds =< calendar:datetime_to_gregorian_seconds(Past)),
    ?assertMatch(true, ExpectedSeconds < Seconds).

cron() ->
    Day1 = {2000,1,1},
    AlarmTimeOfDay = {15,29,59},
    erlcron:set_datetime({Day1, AlarmTimeOfDay}),

    Self = self(),

    Ref1 = make_ref(),
    Job1 = {{daily, {3, 30, pm}}, fun(Ref, _) -> Self ! {ack, Ref} end},
    Ref1 = erlcron:cron(Ref1, Job1),

    Job2  = {{daily, {3, 30, pm}}, fun(Ref, _) -> Self ! {ack, Ref} end},
    test2 = erlcron:cron(test2, Job2),

    Job3  = {{daily, {3, 30, pm}}, fun() -> Self ! {ack, test3} end},
    test3 = erlcron:cron(test3, Job3),

    Job4 = {{daily, {3, 30, pm}}, {?MODULE, sample_arity0},
              #{on_job_end => fun(Ref, {ok, undefined}) -> Self ! {ack, Ref} end}},
    test4 = erlcron:cron(test4, Job4),

    Job5 = {{daily, {3, 30, pm}}, {?MODULE, sample_arity2},
              #{on_job_end => fun(_Ref, {ok, Res}) -> Self ! Res end}},
    test5 = erlcron:cron(test5, Job5),
    Job6 = {{daily, {3, 30, pm}}, fun(Ref, _) -> sample_arityN(Ref, Self) end},
    test6 = erlcron:cron(test6, Job6),

    ?assertMatch(1, collect({ack, Ref1},  1000, 1)),
    ?assertMatch(1, collect({ack, test2}, 1000, 1)),
    ?assertMatch(1, collect({ack, test3}, 1000, 1)),
    ?assertMatch(1, collect({ack, test4}, 1000, 1)),
    ?assertMatch(1, collect({ack, test5}, 1000, 1)),
    ?assertMatch(1, collect({ack, test6}, 1000, 1)).

sample_arity0() ->
    undefined.
sample_arity2(Ref, _) ->
    {ack, Ref}.
sample_arityN(Ref, Pid) ->
    Pid ! {ack, Ref}.

%% Run job on this host
cron_run_job_on_host() ->
    {ok, Host} = inet:gethostname(),
    erlcron:set_datetime({{2000, 1, 1}, {12, 59, 59}}),
    Self = self(),
    Ref  = make_ref(),
    Job  = {{once, {1, 00, pm}}, fun(_,_) -> Self ! Ref end, #{hostnames => [Host]}},

    ?assert(is_reference(erlcron:cron(Job))),
    ?assertMatch(1, collect(Ref, 2500, 1)).

%% Don't add job when executed on a disallowed host
cron_skip_job_on_host() ->
    {ok, Host} = inet:gethostname(),
    Self = self(),
    Ref  = make_ref(),
    Job  = {{once, {1, 00, pm}}, fun(_,_) -> Self ! Ref end, #{hostnames => [Host ++ "123"]}},
    ?assertEqual(ignored, erlcron:cron(Job)).

validation() ->
    erlcron:set_datetime({{2000,1,1}, {15,0,0}}),
    ?assertMatch(ok, ecrn_agent:validate({once, {3, 30, pm}})),
    erlcron:set_datetime({{2000,1,1}, {15,31,0}}),
    ?assertMatch({error,{specified_time_past_seconds_ago, -60}},
                 ecrn_agent:validate({once, {3, 30, pm}})),

    ?assertMatch(ok, ecrn_agent:validate({once, 3600})),
    ?assertMatch(ok, ecrn_agent:validate({daily, {every, {0,s}}})),
    ?assertMatch(ok, ecrn_agent:validate({daily, {every, {23,s}}})),
    ?assertMatch(ok, ecrn_agent:validate({daily, {every, {23,sec},
                                                     {between, {3, pm}, {3, 30, pm}}}})),
    ?assertMatch(ok, ecrn_agent:validate({daily, {3, 30, pm}})),
    ?assertMatch(ok, ecrn_agent:validate({weekly, thu, {2, am}})),
    ?assertMatch(ok, ecrn_agent:validate({weekly, wed, {2, am}})),
    ?assertMatch(ok, ecrn_agent:validate({weekly, fri, {every, {5,sec}}})),
    ?assertMatch(ok, ecrn_agent:validate({monthly, 1, {2, am}})),
    ?assertMatch(ok, ecrn_agent:validate({monthly, 4, {2, am}})),
    ?assertMatch({error,{invalid_time,{55,22,am}}},
                    ecrn_agent:validate({daily, {55, 22, am}})),
    ?assertMatch({error,{invalid_days_in_schedule,{monthly,"A",{55,am}}}},
                    ecrn_agent:validate({monthly, 65, {55, am}})).

%% info/1 returns the internal #job{} record for a running job.
%% The record is a 4-tuple {job, Schedule, Task, Lambda}.
info_job() ->
    erlcron:set_datetime({{2000,1,1},{8,0,0}}),
    Ref = erlcron:at(info_job_ref, {9,0,0}, fun() -> ok end),
    Pid = ecrn_reg:get(Ref),
    Info = ecrn_agent:info(Pid),
    ?assert(is_record(Info, job)),
    ?assertEqual({once, {absolute,32400000}}, Info#job.schedule),
    ?assert(is_function(Info#job.lambda, 0)),
    erlcron:cancel(Ref).

%% start_link/4 accepts an explicit callable fun/0 or fun/2.
start_link4() ->
    erlcron:set_datetime({{2000,1,1},{8,0,0}}),
    Self = self(),
    %% fun/0
    Ref0 = make_ref(),
    Fun0 = fun() -> Self ! {ack0, Ref0} end,
    {ok, _} = ecrn_agent:start_link(Ref0, {{once, {8,0,1}}, task0}, Fun0, #{}),
    ?assertMatch(1, collect({ack0, Ref0}, 2000, 1)),
    %% fun/2 - receives (JobRef, DateTime) as arguments
    Ref2 = make_ref(),
    Fun2 = fun(R, _DT) -> Self ! {ack2, R} end,
    {ok, _} = ecrn_agent:start_link(Ref2, {{once, {8,0,1}}, task2}, Fun2, #{}),
    ?assertMatch(1, collect({ack2, Ref2}, 2000, 1)).

%% Map-based jobs support 'schedule' and 'task' keys (new names).
%% The old 'interval'/'execute' keys remain supported for backward compat.
map_job_schedule_task() ->
    erlcron:set_datetime({{2000,1,1},{12,59,59}}),
    Self = self(),
    Ref  = make_ref(),
    %% New key names
    Job  = #{id => map_job_new, schedule => {once, {1, 0, pm}},
             task => fun() -> Self ! {Ref, new} end},
    map_job_new = erlcron:cron(Job),
    ?assertMatch(1, collect({Ref, new}, 2000, 1)),
    %% Old key names still work
    Job2 = #{id => map_job_old, interval => {once, {1, 0, pm}},
             execute => fun() -> Self ! {Ref, old} end},
    erlcron:set_datetime({{2000,1,1},{12,59,59}}),
    map_job_old = erlcron:cron(Job2),
    ?assertMatch(1, collect({Ref, old}, 2000, 1)).

weekly() ->
    DateF = fun (Offset) -> {2000, 1, 1 + Offset} end,
    erlcron:set_datetime({DateF(0), {7,0,0}}),
    Self = self(),
    erlcron:cron(weekly, {{weekly, [sat, sun], {9,0,0}}, fun() -> Self ! weekly end}),
    Pattern = [1, 1, 0, 0, 0, 0, 0, 1],
    collect_weekly(DateF, {8, 0, 0}, {10, 0, 0}, Pattern),
    erlcron:cancel(weekly).

weekly_every() ->
    DateF = fun (Offset) -> {2000, 1, 1 + Offset} end,
    erlcron:set_datetime({DateF(0), {7,0,0}}),
    Self = self(),
    erlcron:cron(weekly, {{weekly, [sat, mon],
                           {every, {29, sec}, {between, {9, 0, 0}, {9, 1, 0}}}},
                          fun() -> Self ! weekly end}),
    Pattern = [3, 0, 3, 0, 0, 0, 0, 3],
    collect_weekly(DateF, {8, 0, 0}, {10, 0, 0}, Pattern),
    erlcron:cancel(weekly).

%%%===================================================================
%%% Standalone unit tests (no application required)
%%%===================================================================

%% check_task/2 now returns the resolved fun instead of just validating.
check_task_test_() ->
    [?FuncTest(check_task_mfa_returns_fun),
     ?FuncTest(check_task_mfa_empty_returns_fun),
     ?FuncTest(check_task_fun0_passthrough),
     ?FuncTest(check_task_fun2_passthrough),
     ?FuncTest(check_task_rejects_mfa_with_args)].

%% {M, F} - checks arities [0, 2], returns the matching fun.
check_task_mfa_returns_fun() ->
    F = ecrn_cron_sup:check_task(r, {erlang, self}),
    ?assert(is_function(F, 0)).

%% {M, F, []} - restricts to arity 0 only.
check_task_mfa_empty_returns_fun() ->
    F = ecrn_cron_sup:check_task(r, {erlang, garbage_collect, []}),
    ?assert(is_function(F, 0)).

%% fun/0 is returned as-is.
check_task_fun0_passthrough() ->
    Fun = fun() -> ok end,
    ?assertEqual(Fun, ecrn_cron_sup:check_task(r, Fun)).

%% fun/2 is returned as-is.
check_task_fun2_passthrough() ->
    Fun = fun(_, _) -> ok end,
    ?assertEqual(Fun, ecrn_cron_sup:check_task(r, Fun)).

%% {M, F, [arg, ...]} with non-empty args is no longer supported.
check_task_rejects_mfa_with_args() ->
    ?assertError({invalid_job_task, r, _},
                 ecrn_cron_sup:check_task(r, {io, format, ["~p~n", []]})).

%%%===================================================================
%%% from_cron/1 unit tests (no application required)
%%%===================================================================

from_cron_test_() ->
    [?FuncTest(from_cron_every_minute),
     ?FuncTest(from_cron_every_n_minutes),
     ?FuncTest(from_cron_every_hour),
     ?FuncTest(from_cron_every_n_hours),
     ?FuncTest(from_cron_specific_time),
     ?FuncTest(from_cron_multiple_minutes),
     ?FuncTest(from_cron_weekly_single_day),
     ?FuncTest(from_cron_weekly_multiple_days),
     ?FuncTest(from_cron_weekly_named_day),
     ?FuncTest(from_cron_weekly_named_days),
     ?FuncTest(from_cron_monthly_single_dom),
     ?FuncTest(from_cron_monthly_multiple_doms),
     ?FuncTest(from_cron_month_field_ignored),
     ?FuncTest(from_cron_minute_range),
     ?FuncTest(from_cron_dom_range),
     ?FuncTest(from_cron_binary_input),
     ?FuncTest(from_cron_error_five_fields),
     ?FuncTest(from_cron_error_dom_and_dow),
     ?FuncTest(from_cron_error_out_of_range_minute),
     ?FuncTest(from_cron_error_bad_dow)].

%% * * * * *  →  {daily, {every, {1, min}}}
from_cron_every_minute() ->
    ?assertEqual({ok, {daily, {every, {1, min}}}},
                 ecrn_util:from_cron("* * * * *")).

%% */5 * * * *  →  {daily, {every, {5, min}}}
from_cron_every_n_minutes() ->
    ?assertEqual({ok, {daily, {every, {5, min}}}},
                 ecrn_util:from_cron("*/5 * * * *")).

%% 0 * * * *  →  {daily, {every, {1, hr}}}
from_cron_every_hour() ->
    ?assertEqual({ok, {daily, {every, {1, hr}}}},
                 ecrn_util:from_cron("0 * * * *")).

%% 0 */2 * * *  →  {daily, {every, {2, hr}}}
from_cron_every_n_hours() ->
    ?assertEqual({ok, {daily, {every, {2, hr}}}},
                 ecrn_util:from_cron("0 */2 * * *")).

%% 30 9 * * *  →  {daily, {9, 30, 0}}
from_cron_specific_time() ->
    ?assertEqual({ok, {daily, {9, 30, 0}}},
                 ecrn_util:from_cron("30 9 * * *")).

%% 0,30 9 * * *  →  {daily, [{9,0,0}, {9,30,0}]}
from_cron_multiple_minutes() ->
    ?assertEqual({ok, {daily, [{9, 0, 0}, {9, 30, 0}]}},
                 ecrn_util:from_cron("0,30 9 * * *")).

%% 0 9 * * 1  →  {weekly, mon, {9, 0, 0}}
from_cron_weekly_single_day() ->
    ?assertEqual({ok, {weekly, mon, {9, 0, 0}}},
                 ecrn_util:from_cron("0 9 * * 1")).

%% 0 9 * * 1,3  →  {weekly, [mon, wed], {9, 0, 0}}
from_cron_weekly_multiple_days() ->
    ?assertEqual({ok, {weekly, [mon, wed], {9, 0, 0}}},
                 ecrn_util:from_cron("0 9 * * 1,3")).

%% 0 9 * * mon  →  {weekly, mon, {9, 0, 0}}
from_cron_weekly_named_day() ->
    ?assertEqual({ok, {weekly, mon, {9, 0, 0}}},
                 ecrn_util:from_cron("0 9 * * mon")).

%% 0 9 * * mon,wed,fri  →  {weekly, [mon,wed,fri], {9, 0, 0}}
from_cron_weekly_named_days() ->
    ?assertEqual({ok, {weekly, [mon, wed, fri], {9, 0, 0}}},
                 ecrn_util:from_cron("0 9 * * mon,wed,fri")).

%% 0 9 1 * *  →  {monthly, 1, {9, 0, 0}}
from_cron_monthly_single_dom() ->
    ?assertEqual({ok, {monthly, 1, {9, 0, 0}}},
                 ecrn_util:from_cron("0 9 1 * *")).

%% 0 9 1,15 * *  →  {monthly, [1, 15], {9, 0, 0}}
from_cron_monthly_multiple_doms() ->
    ?assertEqual({ok, {monthly, [1, 15], {9, 0, 0}}},
                 ecrn_util:from_cron("0 9 1,15 * *")).

%% 0 9 1 6 *  →  month field is ignored; same as "0 9 1 * *"
from_cron_month_field_ignored() ->
    ?assertEqual({ok, {monthly, 1, {9, 0, 0}}},
                 ecrn_util:from_cron("0 9 1 6 *")).

%% Minute range: 0-5 expands to [0,1,2,3,4,5] crossed with hour 9 → list
from_cron_minute_range() ->
    {ok, {daily, Times}} = ecrn_util:from_cron("0-5 9 * * *"),
    ?assertEqual([{9,0,0},{9,1,0},{9,2,0},{9,3,0},{9,4,0},{9,5,0}], Times).

%% DOM range: 1-3 expands to [1,2,3]
from_cron_dom_range() ->
    ?assertEqual({ok, {monthly, [1, 2, 3], {9, 0, 0}}},
                 ecrn_util:from_cron("0 9 1-3 * *")).

%% Binary input works the same as string input
from_cron_binary_input() ->
    ?assertEqual({ok, {daily, {9, 30, 0}}},
                 ecrn_util:from_cron(<<"30 9 * * *">>)).

%% Too few / too many fields returns an error
from_cron_error_five_fields() ->
    ?assertMatch({error, _}, ecrn_util:from_cron("* * * *")),
    ?assertMatch({error, _}, ecrn_util:from_cron("* * * * * *")).

%% Both DOM and DOW specified is unsupported
from_cron_error_dom_and_dow() ->
    ?assertMatch({error, _}, ecrn_util:from_cron("0 9 1 * 1")).

%% Minute value out of range (0–59)
from_cron_error_out_of_range_minute() ->
    ?assertMatch({error, _}, ecrn_util:from_cron("60 9 * * *")).

%% Unrecognised day-of-week token
from_cron_error_bad_dow() ->
    ?assertMatch({error, _}, ecrn_util:from_cron("0 9 * * funday")).

%%%===================================================================
%%% parse_schedule/1 unit tests (no application required)
%%%===================================================================

parse_schedule_test_() ->
    {setup,
     fun() ->
        application:load(erlcron),
        application:set_env(erlcron, sup_intensity, 0),
        application:set_env(erlcron, sup_period,    1),
        application:unset_env(erlcron, crontab),
        disable_sasl_logger(),
        application:start(erlcron)
     end,
     fun(_) ->
        application:stop(erlcron),
        enable_sasl_logger()
     end,
     [{timeout, 10, [
        ?FuncTest(parse_schedule_valid_tuple_daily),
        ?FuncTest(parse_schedule_valid_tuple_weekly),
        ?FuncTest(parse_schedule_valid_tuple_monthly),
        ?FuncTest(parse_schedule_valid_tuple_once),
        ?FuncTest(parse_schedule_valid_tuple_every),
        ?FuncTest(parse_schedule_invalid_tuple),
        ?FuncTest(parse_schedule_string_cron),
        ?FuncTest(parse_schedule_binary_cron),
        ?FuncTest(parse_schedule_string_error)
       ]}
     ]}.

%% Valid daily tuple is returned unchanged
parse_schedule_valid_tuple_daily() ->
    ?assertEqual({ok, {daily, {9, 30, 0}}},
                 ecrn_util:parse_schedule({daily, {9, 30, 0}})).

%% Valid weekly tuple is returned unchanged
parse_schedule_valid_tuple_weekly() ->
    ?assertEqual({ok, {weekly, mon, {9, 0, 0}}},
                 ecrn_util:parse_schedule({weekly, mon, {9, 0, 0}})).

%% Valid monthly tuple is returned unchanged
parse_schedule_valid_tuple_monthly() ->
    ?assertEqual({ok, {monthly, 1, {9, 0, 0}}},
                 ecrn_util:parse_schedule({monthly, 1, {9, 0, 0}})).

%% Valid once tuple is returned unchanged; set_datetime ensures {11,0,0} is in the future
parse_schedule_valid_tuple_once() ->
    erlcron:set_datetime({{2000,1,1},{8,0,0}}),
    ?assertEqual({ok, {once, {11, 0, 0}}},
                 ecrn_util:parse_schedule({once, {11, 0, 0}})).

%% Valid every tuple is returned unchanged
parse_schedule_valid_tuple_every() ->
    ?assertEqual({ok, {daily, {every, {5, min}}}},
                 ecrn_util:parse_schedule({daily, {every, {5, min}}})).

%% Invalid tuple yields {error, {invalid_schedule, _}}
parse_schedule_invalid_tuple() ->
    ?assertMatch({error, {invalid_schedule, _}},
                 ecrn_util:parse_schedule({monthly, 65, {55, am}})).

%% String cron expression is delegated to from_cron/1
parse_schedule_string_cron() ->
    ?assertEqual({ok, {daily, {every, {5, min}}}},
                 ecrn_util:parse_schedule("*/5 * * * *")).

%% Binary cron expression is delegated to from_cron/1
parse_schedule_binary_cron() ->
    ?assertEqual({ok, {weekly, fri, {18, 0, 0}}},
                 ecrn_util:parse_schedule(<<"0 18 * * 5">>)).

%% Invalid cron string returns {error, _}
parse_schedule_string_error() ->
    ?assertMatch({error, _},
                 ecrn_util:parse_schedule("not a cron expression")).

%%%===================================================================
%%% ecrn_agent:run/1 unit tests
%%%===================================================================

run_test_() ->
    {setup,
     fun() ->
        application:load(erlcron),
        application:set_env(erlcron, sup_intensity, 0),
        application:set_env(erlcron, sup_period,    1),
        application:unset_env(erlcron, crontab),
        disable_sasl_logger(),
        application:start(erlcron)
     end,
     fun(_) ->
        application:stop(erlcron),
        enable_sasl_logger()
     end,
     [{timeout, 10, [
        ?FuncTest(run_executes_fun0),
        ?FuncTest(run_executes_fun2),
        ?FuncTest(run_does_not_cancel_daily_job),
        ?FuncTest(run_stops_once_job),
        ?FuncTest(run_fires_on_job_end_callback),
        ?FuncTest(run_skipped_when_on_job_start_returns_ignore),
        ?FuncTest(run_returns_ok)
      ]}
     ]}.

%% run/1 executes a fun/0 immediately
run_executes_fun0() ->
    erlcron:set_datetime({{2000,1,1},{8,0,0}}),
    Self = self(),
    Ref  = erlcron:daily(run_fun0, {9, 0, 0}, fun() -> Self ! ran end),
    Pid  = ecrn_reg:get(Ref),
    ecrn_agent:run(Pid),
    ?assertMatch(1, collect(ran, 500, 1)),
    erlcron:cancel(Ref).

%% run/1 executes a fun/2, passing (JobRef, Time)
run_executes_fun2() ->
    erlcron:set_datetime({{2000,1,1},{8,0,0}}),
    Self = self(),
    Ref  = erlcron:daily(run_fun2, {9, 0, 0}, fun(R, _DT) -> Self ! {ran2, R} end),
    Pid  = ecrn_reg:get(Ref),
    ecrn_agent:run(Pid),
    ?assertMatch(1, collect({ran2, run_fun2}, 500, 1)),
    erlcron:cancel(Ref).

%% run/1 on a daily job does not cancel it; the job remains registered
run_does_not_cancel_daily_job() ->
    erlcron:set_datetime({{2000,1,1},{8,0,0}}),
    Ref = erlcron:daily(run_persist, {9, 0, 0}, fun() -> ok end),
    Pid = ecrn_reg:get(Ref),
    ecrn_agent:run(Pid),
    timer:sleep(50),
    ?assertNotEqual(undefined, ecrn_reg:get(Ref)),
    erlcron:cancel(Ref).

%% run/1 on a once job causes it to stop (process exits normally);
%% gen_server:call exits with {normal,...} when the server stops without replying
run_stops_once_job() ->
    erlcron:set_datetime({{2000,1,1},{8,0,0}}),
    Ref = erlcron:at(run_once, {9, 0, 0}, fun() -> ok end),
    Pid = ecrn_reg:get(Ref),
    ?assertExit({normal, _}, ecrn_agent:run(Pid)),
    timer:sleep(50),
    ?assertEqual(undefined, ecrn_reg:get(Ref)).

%% run/1 fires the on_job_end callback with {ok, Result}
run_fires_on_job_end_callback() ->
    erlcron:set_datetime({{2000,1,1},{8,0,0}}),
    Self = self(),
    Opts = #{on_job_end => fun(_Ref, Res) -> Self ! {ended, Res} end},
    Ref  = erlcron:daily(run_cb, {9, 0, 0},
                         fun() -> done end, Opts),
    Pid  = ecrn_reg:get(Ref),
    ecrn_agent:run(Pid),
    ?assertMatch({ended, {ok, done}}, receive M -> M after 500 -> timeout end),
    erlcron:cancel(Ref).

%% run/1 skips execution when on_job_start returns ignore
run_skipped_when_on_job_start_returns_ignore() ->
    erlcron:set_datetime({{2000,1,1},{8,0,0}}),
    Self = self(),
    Opts = #{on_job_start => fun(_Ref) -> ignore end,
             on_job_end   => fun(_Ref, _Res) -> Self ! ran_anyway end},
    Ref  = erlcron:daily(run_ignored, {9, 0, 0}, fun() -> Self ! ran_anyway end, Opts),
    Pid  = ecrn_reg:get(Ref),
    ecrn_agent:run(Pid),
    ?assertMatch(0, collect(ran_anyway, 200, 1)),
    erlcron:cancel(Ref).

%% run/1 returns ok
run_returns_ok() ->
    erlcron:set_datetime({{2000,1,1},{8,0,0}}),
    Ref = erlcron:daily(run_ret, {9, 0, 0}, fun() -> ok end),
    Pid = ecrn_reg:get(Ref),
    ?assertEqual(ok, ecrn_agent:run(Pid)),
    erlcron:cancel(Ref).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
seconds_now() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

collect(Msg, Timeout, Count) ->
    collect(Msg, Timeout, 0, Count).
collect(_Msg, _Timeout, Count, Count) ->
    Count;
collect(Msg, Timeout, I, Count) ->
    receive
        Msg     -> collect(Msg, Timeout, I+1, Count)
    after
        Timeout -> I
    end.

% check that for each day generated by DateF(I) for increasing I, Pattern[I]
% weekly messages are received
collect_weekly(DateF, TimeBefore, TimeAfter, Pattern) ->
    collect_weekly(DateF, TimeBefore, TimeAfter, Pattern, 0).

collect_weekly(DateF, TimeBefore, TimeAfter, [N | PatternTail], I) ->
    erlcron:set_datetime({DateF(I), TimeBefore}),
    ?assertMatch(0, collect(weekly, 1000, 1)),
    erlcron:set_datetime({DateF(I), TimeAfter}),
    ?assertMatch(N, collect(weekly, 1000, N)),
    collect_weekly(DateF, TimeBefore, TimeAfter, PatternTail, I+1);
collect_weekly(_DateF, _TimeBefore, _TimeAfter, [], _I) -> ok.
