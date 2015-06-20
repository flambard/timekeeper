-module(timekeeper_app).
-behaviour(application).

%% Application callbacks
-export([ start/2
        , stop/1
        ]).


%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(normal, _StartArgs) ->
    game_clock_sup:start_link().

stop(_State) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
