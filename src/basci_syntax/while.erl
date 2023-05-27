%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. May 2023 8:52 PM
%%%-------------------------------------------------------------------
-module(while).
-author("ruibin").

%% API
-export([start/0]).

while(L) -> while(L, 0).

while([], Acc) -> Acc;
while([_|T], Acc) ->
  io:fwrite("The accmulator is ~w~n", [Acc]),
  while(T, Acc+1).

start() ->
  L = ([1, 2, 3]),
  io:fwrite("~p~n", [while(L)]).