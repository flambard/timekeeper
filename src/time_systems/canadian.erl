-module(canadian).
-behaviour(time_system).

-export([
         next_time_period/1,
         start/1,
         stop/2,
         time/1,
         time_left/1
        ]).

-record(canadian,
        {
          stones,
          time,
          stones_left,
          time_left
        }).

%%%
%%% Time system callbacks
%%%

time({Stones, Time}) ->
  #canadian{
      stones = Stones,
      time = Time,
      stones_left = Stones,
      time_left = Time
     }.

start(#canadian{time = Time}) ->
  time_system:start_timer(Time).

stop(TimerRef, T = #canadian{stones_left = 1}) ->
  _TimeLeft = time_system:stop_timer(TimerRef),
  T#canadian{
    stones_left = T#canadian.stones,
    time_left = T#canadian.time
   };
stop(TimerRef, T = #canadian{stones_left = SL}) ->
  TimeLeft = time_system:stop_timer(TimerRef),
  T#canadian{
    stones_left = SL - 1,
    time_left = TimeLeft
   }.

next_time_period({canadian, _Stones, _Time}) ->
  none.

time_left(#canadian{time_left = Time}) ->
  Time.


%%%
%%% API
%%%

