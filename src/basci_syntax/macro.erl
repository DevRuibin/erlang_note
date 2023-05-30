%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. May 2023 9:54 PM
%%%-------------------------------------------------------------------
-module(macro).
-author("ruibin").

%% API
-export([start/0]).
-define(a, 1).
-define(macro1(X, Y), {X + Y}).

start() ->
  io:fwrite("~w~n", [?a]),
  io:fwrite("~p~n", [?macro1(1, 3)]).