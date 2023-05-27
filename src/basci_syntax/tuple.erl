%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. May 2023 8:03 PM
%%%-------------------------------------------------------------------
-module(tuple).
-author("ruibin").

%% API
-export([start/0]).

start() ->
  P = {john, 24, {june, 25}},
  io:fwrite("~w~n", [tuple_size(P)]).