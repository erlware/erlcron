%%% @copyright Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the BSD License; you may not use
%%% this file except in compliance with the License.
%%%-------------------------------------------------------------------
%%% @doc
%%%  Utility functions for the erlcron system
-module(ecrn_util).

-export([epoch_seconds/0, epoch_milliseconds/0]).
-export([epoch_to_time_string/1, epoch_to_datetime_string/1]).
-export([universaltime_to_epoch/1, localtime_to_epoch/1]).

%%%===================================================================
%%% API
%%%===================================================================
-spec epoch_seconds() -> erlcron:seconds().
epoch_seconds() ->
    erlang:system_time(seconds).

-spec epoch_milliseconds() -> erlcron:milliseconds().
epoch_milliseconds() ->
    erlang:system_time(millisecond).

-spec epoch_to_time_string(erlcron:milliseconds()) -> string().
epoch_to_time_string(Epoch) when is_integer(Epoch) ->
    DT = erlang:posixtime_to_universaltime(Epoch div 1000),
    {_, {H,M,S}} = erlang:universaltime_to_localtime(DT),
    fmt("~.2.0w:~.2.0w:~.2.0w.~.3.0w", [H,M,S, Epoch rem 1000]).

-spec epoch_to_datetime_string(erlcron:milliseconds()) -> string().
epoch_to_datetime_string(Epoch) when is_integer(Epoch) ->
    DT = erlang:posixtime_to_universaltime(Epoch div 1000),
    {{Y,Mo,D}, {H,M,S}} = erlang:universaltime_to_localtime(DT),
    fmt("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w.~.3.0w",
        [Y,Mo,D,H,M,S, Epoch rem 1000]).

-spec universaltime_to_epoch(calendar:datetime()) -> erlcron:milliseconds().
universaltime_to_epoch(DT) ->
    erlang:universaltime_to_posixtime(DT)*1000.

-spec localtime_to_epoch(calendar:datetime()) -> erlcron:milliseconds().
localtime_to_epoch(DT) ->
    erlang:universaltime_to_posixtime(erlang:localtime_to_universaltime(DT))*1000.


fmt(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).
