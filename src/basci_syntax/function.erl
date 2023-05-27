%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. May 2023 9:46 PM
%%%-------------------------------------------------------------------
-module(function).
-author("ruibin").

%% API
-export([start/0]).

% A normal function
add(X, Y) ->
  Z = X + Y,
  io:fwrite("~p + ~p = ~p~n", [X, Y, Z]).

% An anonymous function
add1(X, Y) ->
  F = fun(A, B) ->
    A + B
  end,
  Z = F(X, Y),
  io:fwrite("~p + ~p = ~p~n", [X, Y, Z]).

% A function with guard
add2(X, Y) when X > 0, Y > 0 ->
  Z = X + Y,
  io:fwrite("~p + ~p = ~p~n", [X, Y, Z]).

start() ->
  add(1, 2),
  add1(1, 2),
  add2(1, -1).