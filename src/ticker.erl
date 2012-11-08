-module(ticker).
-behaviour(gen_fsm).

%% API
-export([
         start_link/0,
         start_ticking/1,
         stop_ticking/1
        ]).

%% gen_fsm callbacks
-export([
         init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4
        ]).

%% State functions
-export([
         ticking/2,
         stopped/2
        ]).

-define(ONE_SECOND, 1000).


%%%
%%% API
%%%

start_link() ->
  gen_fsm:start_link(?MODULE, self(), []).

start_ticking(Ticker) ->
  gen_fsm:send_event(Ticker, start).

stop_ticking(Ticker) ->
  gen_fsm:send_event(Ticker, stop).


%%%
%%% gen_fsm callbacks
%%%

init(Pid) ->
  {ok, ticking, Pid, ?ONE_SECOND}.

handle_event(_Event, StateName, StateData) ->
  {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
  {next_state, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
  {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
  ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.


%%%
%%% State functions
%%%

ticking(timeout, Pid) ->
  Pid ! tick,
  {next_state, ticking, Pid, ?ONE_SECOND};
ticking(start, Pid) ->
  {next_state, ticking, Pid, ?ONE_SECOND};
ticking(stop, Pid) ->
  {next_state, stopped, Pid};
ticking(_Event, StateData) ->
  {next_state, stopped, StateData}.

stopped(start, Pid) ->
  {next_state, ticking, Pid, ?ONE_SECOND};
stopped(stop, Pid) ->
  {next_state, stopped, Pid};
stopped(_Event, StateData) ->
  {next_state, stopped, StateData}.
