%%% @copyright Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the BSD License; you may not use
%%% this file except in compliance with the License.
%%%-------------------------------------------------------------------
%%% @doc
%%%  Utility functions for the erlcron system
-module(ecrn_util).

-export([epoch_seconds/0, datetime_to_epoch_seconds/1, epoch_seconds_to_datetime/1]).

-define(SINCE_1970, calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})).

%%%===================================================================
%%% API
%%%===================================================================
-spec epoch_seconds() -> erlcron:seconds().
epoch_seconds() ->
    {Megasecs, Secs, Microsecs} = os:timestamp(),
    erlang:trunc((Megasecs * 1000000) + Secs + (Microsecs / 1000000)).

-spec datetime_to_epoch_seconds(calendar:datetime()) -> erlcron:seconds().
datetime_to_epoch_seconds(DateTime) ->
  calendar:datetime_to_gregorian_seconds(DateTime) - ?SINCE_1970.

-spec epoch_seconds_to_datetime(erlcron:seconds()) -> calendar:datetime().
epoch_seconds_to_datetime(Seconds) ->
  calendar:gregorian_seconds_to_datetime(Seconds + ?SINCE_1970).
