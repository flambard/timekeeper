-module(fischer).
-behaviour(time_system).

-export([
         next_time_period/1,
         start/1,
         stop/2,
         time/1,
         time_left/1
        ]).

-record(fischer,
        {
          time,
          increment
        }).

%%%
%%% Time system callbacks
%%%

time({Time, Increment}) ->
  #fischer{
      time = Time,
      increment = Increment
     }.

start(#fischer{time = Time}) ->
  time_system:start_timer(Time).

stop(TimerRef, T = #fischer{increment = Increment}) ->
  TimeLeft = time_system:stop_timer(TimerRef),
  T#fischer{time = TimeLeft + Increment}.

next_time_period(#fischer{}) ->
  none.

time_left(#fischer{time = Time}) ->
  Time.


%%%
%%% API
%%%

