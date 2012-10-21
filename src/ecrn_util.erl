%%% @copyright Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the BSD License; you may not use
%%% this file except in compliance with the License.
%%%-------------------------------------------------------------------
%%% @doc
%%%  Utility functions for the erlcron system
-module(ecrn_util).

-export([epoch_seconds/0]).

%%%===================================================================
%%% API
%%%===================================================================
-spec epoch_seconds/0 :: () -> erlcron:seconds().
epoch_seconds() ->
    {Megasecs, Secs, Microsecs} = erlang:now(),
    erlang:trunc((Megasecs * 1000000) + Secs + (Microsecs / 1000000)).
