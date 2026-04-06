%%% @copyright Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the BSD License; you may not use
%%% this file except in compliance with the License.
%%%----------------------------------------------------------------
-module(ecrn_app).
-behaviour(application).

-moduledoc """
OTP application callback module for erlcron.

On startup, reads the `crontab` key from the application environment
and registers each listed job with the scheduler.  A `defaults` map
may also be provided to set options that apply to every job unless
overridden per-job.  See the README for the full configuration format.
""".

%% API
-export([manual_start/0, manual_stop/0]).

%% Application callbacks
-export([start/2, stop/1]).

-include("erlcron.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-doc "Start the erlcron application and all its dependencies".
manual_start() ->
    %application:start(crypto),
    application:start(eunit),
    %application:start(sasl),
    application:start(erlcron).

-doc "Stop the erlcron application".
manual_stop() ->
    application:stop(erlcron).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @private
start(_StartType, _StartArgs) ->
    case ecrn_sup:start_link() of
        {ok, Pid} ->
            {ok, H} = inet:gethostname(),
            Def = application:get_env(erlcron, defaults, #{}),
            is_map(Def) orelse
              erlang:error("erlcron/defaults config must be a map!"),
            ?LOG_INFO("CRON: started on host ~s using defaults: ~1024p", [H, Def]),
            setup(Def),
            {ok, Pid};
        Error ->
            Error
    end.

%% @private
stop(_State) ->
    ok.

setup(Def) ->
    case application:get_env(erlcron, crontab) of
        {ok, Crontab} ->
            lists:foreach(fun(CronJob) ->
                Res  = erlcron:cron(CronJob, Def),
                Res2 = if is_reference(Res) -> io_lib:format(": ~p", [Res]); true -> [] end,
                ?LOG_INFO("CRON: adding job ~1024p~s", [CronJob, Res2]),
                case Res of
                    already_started ->
                        erlang:error({duplicate_job_reference, CronJob});
                    ignored ->
                        ok;
                    Ref when is_reference(Ref); is_atom(Ref); is_binary(Ref) ->
                        ok;
                    {error, Reason} ->
                        erlang:error({failed_to_add_cron_job, CronJob, Reason})
                end
            end, Crontab);
        undefined ->
            ok
    end.
