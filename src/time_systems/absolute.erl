-module(absolute).
-behaviour(time_system).

-export([
         next_time_period/1,
         start/1,
         stop/2,
         time/1,
         time_left/1
        ]).

-record(absolute,
        {
          time
        }).

%%%
%%% Time system callbacks
%%%

time(Time) ->
  #absolute{time = Time}.

start(#absolute{time = Time}) ->
  time_system:start_timer(Time).

stop(TimerRef, T = #absolute{}) ->
  TimeLeft = time_system:stop_timer(TimerRef),
  T#absolute{time = TimeLeft}.

next_time_period(#absolute{}) ->
  none.

time_left(#absolute{time = Time}) ->
  Time.


%%%
%%% API
%%%

