%%% @copyright Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the BSD License; you may not use
%%% this file except in compliance with the License.
-module(ecrn_startup_test).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

-define(FuncTest(A), {atom_to_list(A), fun A/0}).

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
        application:start(erlcron),
        Ref
     end,
     fun(_) ->
        application:stop(erlcron)
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
