%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. May 2023 10:57 PM
%%%-------------------------------------------------------------------
-module(funs).
-author("ruibin").

%% API
-export([]).

start() ->
  B = 6,
  A = fun(X) ->
    io:fwrite("~p~n", [X]),
    io:fwrite("~p~n", [B])
  end,
  A(5),
  Adder = fun(X) -> fun(Y) -> io:fwrite("~p~n", [X + Y]) end end,
  C = Adder(6),
  A(10).