%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. May 2023 7:51 PM
%%%-------------------------------------------------------------------
-module(number).
-author("ruibin").

%% API
-export([start/0]).

start() ->
  io:fwrite("The sum of 1 + 1 is ~w~n", [1 +1]).