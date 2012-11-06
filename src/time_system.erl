-module(time_system).

%% Behaviour functions
-export([
         behaviour_info/1,
         module/1,
         start_timer/1,
         stop_timer/1
        ]).

%% Polymorphic time system functions
-export([
         start/1,
         stop/2,
         time_left/1
        ]).


behaviour_info(callbacks) ->
  [
   {next_time_period, 1},
   {start, 1},
   {stop, 2},
   {time, 1},
   {time_left, 1}
  ];
behaviour_info(_Other) ->
  undefined.


%%%
%%% Behaviour functions
%%%

module(TimeSystem) ->
  element(1, TimeSystem).

start_timer(Seconds) ->
  erlang:send_after(Seconds * 1000, self(), time_period_ended).

stop_timer(TimerRef) ->
  case erlang:cancel_timer(TimerRef) of
    false    -> false;
    MSeconds -> (MSeconds div 1000) + 1
  end.


%%%
%%% Polymorphic time system functions
%%%

start(TimeSystem) ->
  Module = module(TimeSystem),
  Module:start(TimeSystem).

stop(TimerRef, TimeSystem) ->
  Module = module(TimeSystem),
  Module:stop(TimerRef, TimeSystem).

time_left(TimeSystem) ->
  Module = module(TimeSystem),
  Module:time_left().
