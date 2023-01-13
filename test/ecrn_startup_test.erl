%%% @copyright Erlware, LLC. All Rights Reserved.
%%% vim:ts=4:ts=4:et
%%%
%%% This file is provided to you under the BSD License; you may not use
%%% this file except in compliance with the License.
-module(ecrn_startup_test).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

-define(FuncTest(A), {atom_to_list(A), fun A/0}).

disable_sasl_logger() ->
    logger:add_handler_filter(default, ?MODULE, {fun(_,_) -> stop end, nostate}).

enable_sasl_logger() ->
    logger:remove_handler_filter(default, ?MODULE).

%%%===================================================================
%%% Types
%%%===================================================================
cron_test_() ->
    {setup,
     fun() ->
        Ref = make_ref(),
        application:load(erlcron),
        application:set_env(erlcron, sup_intensity, 0),
        application:set_env(erlcron, sup_period,    1),
        application:set_env(erlcron, crontab, [
            {{daily, {1, 0, pm}}, {erlang, system_time, []}, #{id => one}},
            {{daily, {2, 0, pm}}, fun() -> erlang:system_time() end, #{id => <<"two">>}},
            {{daily, {3, 0, pm}}, fun(_JobRef, _Now) -> erlang:system_time() end, #{id => Ref}},
            #{id => four, interval => {daily, {1, 0, pm}}, execute => {erlang, system_time, []}}
        ]),
        disable_sasl_logger(),
        application:start(erlcron),
        Ref
     end,
     fun(_) ->
        application:stop(erlcron),
        enable_sasl_logger()
     end,
     {with, [fun(Ref) -> check_startup_jobs(Ref) end]}
    }.

check_startup_jobs(Ref) ->
    ?assertMatch([_, _, _, _], ecrn_cron_sup:all_jobs()),
    ?assertEqual([four, one, Ref, <<"two">>], ecrn_reg:get_all_refs()),
    ?assert(is_pid(ecrn_reg:get(one))),
    ?assert(is_pid(ecrn_reg:get(<<"two">>))),
    ?assert(is_pid(ecrn_reg:get(Ref))),
    ?assert(is_pid(ecrn_reg:get(four))).

cron_bad_job_spec_test_() ->
    {setup,
     fun() ->
         application:set_env(sasl, sasl_error_logger, false),
         application:load(erlcron),
         application:set_env(erlcron, sup_intensity, 0),
         application:set_env(erlcron, sup_period,    1),
         disable_sasl_logger()
     end,
     fun(_) ->
         enable_sasl_logger()
     end,
     [
         ?_assertMatch(
             {error,
                 {bad_return, {{ecrn_app,start,[normal,[]]},
                     {'EXIT', {{module_not_loaded,one, {'$$$bad_module',system_time,[]}, nofile}, [_|_]}}}}},
             begin
                 application:set_env(erlcron, crontab, [
                     {{daily, {1, 0, pm}}, {'$$$bad_module', system_time, []}, #{id => one}}
                 ]),
                 application:start(erlcron)
             end),
         ?_assertMatch(
            {error,
                {bad_return,
                    {{ecrn_app,start,[normal,[]]},
                     {'EXIT',
                         {{wrong_arity_of_job_task, one, "erlang:system_time1000/0"},
                          [_|_]}}}}},
             begin
                 application:set_env(erlcron, crontab, [
                     {{daily, {1, 0, pm}}, {erlang, system_time1000, []}, #{id => one}}
                 ]),
                 application:start(erlcron)
             end),
         ?_assertMatch(
            {error,
                {bad_return,
                    {{ecrn_app,start,[normal,[]]},
                     {'EXIT',
                         {{wrong_arity_of_job_task, one, "erlang:system_time/3"},
                          [_|_]}}}}},
             begin
                 application:set_env(erlcron, crontab, [
                     {{daily, {1, 0, pm}}, {erlang, system_time, [1,2,3]}, #{id => one}}
                 ]),
                 application:start(erlcron)
             end),
         ?_assertMatch(
            {error,
                {bad_return,
                    {{ecrn_app,start,[normal,[]]},
                     {'EXIT',
                         {{wrong_arity_of_job_task, one, "erlang:system_time123/[0,2]"},
                          [_|_]}}}}},
             begin
                 application:set_env(erlcron, crontab, [
                     {{daily, {1, 0, pm}}, {erlang, system_time123}, #{id => one}}
                 ]),
                 application:start(erlcron)
             end)
     ]
    }.
