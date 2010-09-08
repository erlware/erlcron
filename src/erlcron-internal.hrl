%%%----------------------------------------------------------------
%%% @author Eric Merritt <emerritt@ecdmarket.com>
%%% @copyright 2009 eCD Market
%%%----------------------------------------------------------------
-define(ONE_DAY,(24 * 60 * 60) ).

-define(EPOC_SECONDS,
	fun() ->
		{Megasecs, Secs, Microsecs} = erlang:now(),
		(Megasecs * 1000000) + Secs + (Microsecs / 1000000)
	end()).
