-module(timekeeper).

%% API
-export([
         game_clock/1
        ]).


%%%
%%% Timekeeper API
%%%

game_clock(TimeSystems) ->
  game_clock_sup:start_game_clock(TimeSystems).
