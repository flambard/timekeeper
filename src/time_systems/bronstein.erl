-module(bronstein).
-behaviour(time_system).

-export([
         next_time_period/1,
         start/1,
         stop/2,
         time/1,
         time_left/1
        ]).

-record(bronstein,
        {
          time,
          bonus
        }).

%%%
%%% Time system callbacks
%%%

time({Time, Bonus}) ->
  #bronstein{
      time = Time,
      bonus = Bonus
     }.

start(#bronstein{time = Time}) ->
  time_system:start_timer(Time).

stop(TimerRef, T = #bronstein{time = OldTimeLeft, bonus = Bonus}) ->
  TimeLeft = time_system:stop_timer(TimerRef),
  TimeUsed = OldTimeLeft - TimeLeft,
  if Bonus > TimeUsed -> T#bronstein{time = OldTimeLeft};
     true             -> T#bronstein{time = TimeLeft + Bonus}
  end.

next_time_period(#bronstein{}) ->
  none.

time_left(#bronstein{time = Time}) ->
  Time.


%%%
%%% API
%%%

