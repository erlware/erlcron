%%%-------------------------------------------------------------------
%%% @author Eric Merritt <emerritt@ecdmarket.com>
%%%-------------------------------------------------------------------
-module(ecrn_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% -- tests ---------------------------------------------------------------------
cron_test_() ->
    {setup,
     fun() ->
	     ecrn_app:manual_start()
     end,
     fun(_) ->
	     ecrn_app:manual_stop()
     end,
     {with, [fun set_alarm_test/1,
	     fun travel_back_in_time_test/1,
	     fun cancel_alarm_test/1,
	     fun big_time_jump_test/1,
	     fun cron_test/1,
	     fun validation_test/1]}}.



set_alarm_test(_) ->
    EpochDay = {2000,1,1},
    EpochDateTime = {EpochDay,{8,0,0}},
    erlcron:set_datetime(EpochDateTime),
    Alarm1TimeOfDay = {9,0,0},
    Alarm2TimeOfDay = {9,0,1},

    Self = self(),

    erlcron:at(Alarm1TimeOfDay, fun(_, _) ->
					  Self ! ack1
				  end),
    erlcron:at(Alarm2TimeOfDay, fun(_, _) ->
					  Self ! ack2
				  end),
    erlcron:set_datetime({EpochDay, {8, 59, 59}}),
%%
%% The alarm should trigger this nearly immediately.
%%
    ?assertMatch(ok, receive
			 ack1 -> ok
		     after
			 1500 -> timeout
		     end),
%%
%% The alarm should trigger this 1 second later.
%%
    ?assertMatch(ok, receive
			 ack2 -> ok
		     after
			 2500 -> timeout
		     end).

cancel_alarm_test(_) ->
    Day = {2000,1,1},
    erlcron:set_datetime({Day,{8,0,0}}),
    AlarmTimeOfDay = {9,0,0},

    Self = self(),

    Ref = erlcron:at(AlarmTimeOfDay, fun(_, _) ->
					       Self ! ack
				       end),
    erlcron:cancel(Ref),
    erlcron:set_datetime({Day, AlarmTimeOfDay}),
    ?assertMatch(ok, receive
			 ack -> ack
		     after
%% There is no event-driven way to
%% ensure we never receive an ack.
			 125 -> ok
		     end).

%% Time jumps ahead one day so we should see the alarms from both days.
big_time_jump_test(_) ->
    Day1 = {2000,1,1},
    Day2 = {2000,1,2},
    EpochDateTime = {Day1,{8,0,0}},
    erlcron:set_datetime(EpochDateTime),
    Alarm1TimeOfDay = {9,0,0},
    Alarm2TimeOfDay = {9,0,1},

    Self = self(),

    erlcron:at(Alarm1TimeOfDay, fun(_, _) ->
					  Self ! ack1
				  end),
    erlcron:at(Alarm2TimeOfDay, fun(_, _) ->
					  Self ! ack2
				  end),
    erlcron:set_datetime({Day2, {9, 10, 0}}),
    ?assertMatch(ok, receive
			 ack1 -> ok
		     after
			 1500 -> timeout
		     end),
    ?assertMatch(ok, receive
			 ack2 -> ok
		     after
			 1500 -> timeout
		     end),

    ?assertMatch(ok, receive
			 ack1 -> ok
		     after
			 1500 -> timeout
		     end),
    ?assertMatch(ok, receive
			 ack2 -> ok
		     after
			 2500 -> timeout
		     end).

travel_back_in_time_test(_) ->
    Seconds = seconds_now(),
    Past = {{2000,1,1},{12,0,0}},
    erlcron:set_datetime(Past),
    {ExpectedDateTime, _} = erlcron:datetime(),
    ExpectedSeconds = calendar:datetime_to_gregorian_seconds(ExpectedDateTime),
    ?assert(ExpectedSeconds >= calendar:datetime_to_gregorian_seconds(Past)),
    ?assert(ExpectedSeconds < Seconds).


%% Time jumps ahead one day so we should see the alarms from both days.
cron_test(_) ->
    Day1 = {2000,1,1},
    AlarmTimeOfDay = {15,29,58},
    erlcron:set_datetime({Day1, AlarmTimeOfDay}),

    Self = self(),

    Job = {{daily, {3, 30, pm}},
	    fun(_, _) ->
		    Self ! ack
	    end},

    erlcron:cron(Job),

    ?assertMatch(ok, receive
			 ack -> ok
		     after
			 2500 -> timeout
		     end).

validation_test(_) ->
    ?assertMatch(valid, ecrn_agent:validate({once, {3, 30, pm}})),
    ?assertMatch(valid, ecrn_agent:validate({once, 3600})),
    ?assertMatch(valid, ecrn_agent:validate({daily, {every, {23, sec}, {between, {3, pm}, {3, 30, pm}}}})),
    ?assertMatch(valid, ecrn_agent:validate({daily, {3, 30, pm}})),
    ?assertMatch(valid, ecrn_agent:validate({daily, [{1, 10, am}, {1, 07, 30, am}]})),
    ?assertMatch(valid, ecrn_agent:validate({weekly, thu, {2, am}})),
    ?assertMatch(valid, ecrn_agent:validate({weekly, wed, {2, am}})),
    ?assertMatch(valid, ecrn_agent:validate({monthly, 1, {2, am}})),
    ?assertMatch(valid, ecrn_agent:validate({monthly, 4, {2, am}})),
    ?assertMatch(invalid, ecrn_agent:validate({once, fubar})),
    ?assertMatch(invalid, ecrn_agent:validate({daily, {55, 22, am}})),
    ?assertMatch(invalid, ecrn_agent:validate({monthly, 65, {55, am}})).



%% -- helpers -------------------------------------------------------------------

seconds_now() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).
