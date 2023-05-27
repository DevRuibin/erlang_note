%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. May 2023 8:08 PM
%%%-------------------------------------------------------------------
-module(list).
-author("ruibin").

%% API
-export([start/0]).

start() ->
  L = ([1, 2, 3]),
  io:fwrite("~p~n", [length(L)]).