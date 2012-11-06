-module(timekeeper).
-behaviour(application).

%% application callbacks
-export([
         start/2,
         stop/1
        ]).

%% API
-export([
         game_clock/1
        ]).


%%%
%%% application callbacks
%%%

start(normal, _Args) ->
  game_clock_sup:start_link().

stop(_State) ->
  ok.


%%%
%%% API
%%%

game_clock(TimeSystems) ->
  game_clock_sup:start_game_clock(TimeSystems).
