-module(japanese).
-behaviour(time_system).

-export([
         next_time_period/1,
         start/1,
         stop/2,
         time/1,
         time_left/1
        ]).

-record(japanese,
        {
          periods,
          time
        }).

%%%
%%% Time system callbacks
%%%

time({Periods, Time}) ->
  #japanese{
      periods = Periods,
      time = Time
     }.

start(#japanese{time = Time}) ->
  time_system:start_timer(Time).

stop(TimerRef, T = #japanese{}) ->
  _TimeLeft = time_system:stop_timer(TimerRef),
  T.

next_time_period(#japanese{periods = 1}) ->
  none;
next_time_period(T = #japanese{periods = Periods}) ->
  T#japanese{periods = Periods - 1}.

time_left(#japanese{time = Time}) ->
  Time.


%%%
%%% API
%%%

