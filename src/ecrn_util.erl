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
