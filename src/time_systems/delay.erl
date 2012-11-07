-module(delay).
-behaviour(time_system).

-export([
         next_time_period/1,
         start/1,
         stop/2,
         time/1,
         time_left/1
        ]).

-record(delay,
        {
          time,
          delay
        }).

%%%
%%% Time system callbacks
%%%

time({Time, Delay}) ->
  #delay{time = Time, delay = Delay}.

start(#delay{time = Time, delay = Delay}) ->
  time_system:start_timer(Time + Delay).

stop(TimerRef, T = #delay{time = OldTimeLeft}) ->
  TimeLeft = time_system:stop_timer(TimerRef),
  if TimeLeft > OldTimeLeft -> T#delay{time = OldTimeLeft};
     true                   -> T#delay{time = TimeLeft}
  end.

next_time_period(#delay{}) ->
  none.

time_left(#delay{time = Time}) ->
  Time.


%%%
%%% API
%%%

