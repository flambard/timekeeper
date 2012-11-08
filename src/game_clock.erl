-module(game_clock).
-behaviour(gen_server).

%% API
-export([
         start_link/1,
         start_link/2,
         hit/2,
         stop/1
        ]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state,
        {
          timer,
          pid,
          whose_turn,
          black,
          white
        }).


%%%
%%% API
%%%

start_link(TimeSystems) ->
  start_link(TimeSystems, self()).

start_link(TimeSystems, Pid) ->
  gen_server:start_link(?MODULE, {TimeSystems, Pid}, []).

hit(GameClock, Player) ->
  gen_server:call(GameClock, {hit, Player}).

stop(GameClock) ->
  gen_server:call(GameClock, stop).


%%%
%%% gen_server callbacks
%%%

init({TimeSystems, Pid}) ->
  State = #state{black = TimeSystems,
                 white = TimeSystems,
                 pid = Pid},
  {ok, State}.

handle_call({hit, Player}, _From, S = #state{whose_turn = undefined}) ->
  %%
  %% Clock started from stopped position
  %%
  TimeLeft = player_time_left(S, Player),
  S1 = set_player_turn(S, other_player(Player)),
  {reply, TimeLeft, start_timer(S1)};
handle_call({hit, Player}, _From, S = #state{whose_turn = Player}) ->
  %%
  %% Clock swapped
  %%
  S1 = stop_timer(S),
  TimeLeft = player_time_left(S1, Player),
  S2 = swap_player_turn(S1),
  {reply, TimeLeft, start_timer(S2)};
handle_call(stop, _From, S = #state{whose_turn = Player}) ->
  %%
  %% Clock stopped
  %%
  S1 = stop_timer(S),
  TimeLeft = player_time_left(S1, Player),
  {reply, TimeLeft, S1#state{whose_turn = undefined}};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(time_period_ended, S = #state{}) ->
  %%
  %% Clock time period ended
  %%
  signal_time_period_ended(S),
  [T | Ts] = get_current_player_time_systems(S),
  Module = time_system:module(T),
  NewState =
    case Module:next_time_period(T) of
      none ->
        Timer = case Ts of
                  []          -> none;
                  [NextT | _] -> time_system:start(NextT)
                end,
        set_current_player_time_systems(S#state{timer = Timer}, Ts);
      NewT ->
        Timer = Module:start(NewT),
        set_current_player_time_systems(S#state{timer = Timer}, [NewT | Ts])
    end,
  {noreply, NewState};
handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%
%%% Internal functions
%%%

other_player(black) -> white;
other_player(white) -> black.

get_current_player_time_systems(#state{whose_turn = black, black = T}) -> T;
get_current_player_time_systems(#state{whose_turn = white, white = T}) -> T.

set_current_player_time_systems(S = #state{whose_turn = black}, T) ->
  S#state{black = T};
set_current_player_time_systems(S = #state{whose_turn = white}, T) ->
  S#state{white = T}.

start_timer(S) ->
  [T | _] = get_current_player_time_systems(S),
  S#state{timer = time_system:start(T)}.

stop_timer(S = #state{whose_turn = black, black = [T | Ts]}) ->
  NewT = time_system:stop(S#state.timer, T),
  S#state{timer = undefined, black = [NewT | Ts]};
stop_timer(S = #state{whose_turn = white, white = [T | Ts]}) ->
  NewT = time_system:stop(S#state.timer, T),
  S#state{timer = undefined, white = [NewT | Ts]}.


set_player_turn(S = #state{}, Player) ->
  S#state{whose_turn = Player}.

swap_player_turn(S = #state{whose_turn = Player}) ->
  S#state{whose_turn = other_player(Player)}.

player_time_left(#state{black = [T | _]}, black) ->
  time_system:time_left(T);
player_time_left(#state{white = [T | _]}, white) ->
  time_system:time_left(T).


signal_time_period_ended(S) ->
  S#state.pid ! {time_period_ended, S#state.whose_turn}.
