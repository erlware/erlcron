%%%----------------------------------------------------------------
%%% @author Eric Newhuis <enewhuis@ecdmarket.com>
%%% @doc
%%%  erlcron app system
%%% @end
%%%----------------------------------------------------------------,
-module(ecrn_app).

-behaviour(application).

-export([manual_start/0, manual_stop/0]).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    case ecrn_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%
%% @doc
%%  start up the app and all the dependent apps.
%% @end
%%
manual_start() ->
    application:start(crypto),
    application:start(eunit),
    application:start(sasl),
    application:start(erlcron).

%%
%% @doc
%%  stop the app manually
%% @end
%%
manual_stop() ->
    application:stop(erlcron).

%%%===================================================================
%%% Internal functions
%%%===================================================================
