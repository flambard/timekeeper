-module(game_clock_sup).
-behaviour(supervisor).

%% supervisor callbacks
-export([init/1]).

%% API
-export([
         start_link/0,
         start_game_clock/1
        ]).


%%%
%%% supervisor callbacks
%%%

init(_Args) ->
  MaxRestart = 5,
  MaxTime = 3600,
  {ok, {{simple_one_for_one, MaxRestart, MaxTime},
        [
         {game_clock,
          {game_clock, start_link, []},
          temporary,
          1000,
          worker,
          [game_clock]}
        ]}}.


%%%
%%% API
%%%

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_game_clock(TimeSystems) ->
  supervisor:start_child(?MODULE, [TimeSystems]).
