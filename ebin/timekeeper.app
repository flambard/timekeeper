{application, timekeeper,
 [
  {vsn, "0.0.1"},
  {registered, [game_clock_sup]},
  {mod, {timekeeper, []}},
  {applications,
   [
    kernel,
    stdlib
   ]},
  {modules,
   [
    timekeeper,
    game_clock,
    game_clock_sup,
    time_system,
    ticker,

    %% Time systems
    absolute,
    bronstein,
    canadian,
    delay,
    fischer,
    japanese
    %%
   ]}
 ]}.
