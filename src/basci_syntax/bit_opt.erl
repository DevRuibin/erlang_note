%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. May 2023 8:29 PM
%%%-------------------------------------------------------------------
-module(bit_opt).
-author("ruibin").

%% API
-export([start/0]).

start() ->
  io:fwrite("~p~n", [2#1010 band 2#1100]),
  io:fwrite("~p~n", [2#1010 bor 2#1100]),
  io:fwrite("~p~n", [2#1010 bxor 2#1100]),
  io:fwrite("~p~n", [2#1010 bsl 2]), % 2#101000 = 40
  io:fwrite("~p~n", [2#1010 bsr 2]), % 2#10 = 2
  io:fwrite("~p~n", [bnot 2#1010 ]), % 2#11110101 => 0000 1011 => 11
  io:fwrite("~p~n",[00111100 band 00001101]), % They are interpreted as decimal numbers
  io:fwrite("~w~n",[00111100 bxor 00111100]),
  io:fwrite("~w~n",[bnot 00111100]),
  io:fwrite("~w~n",[00111100 bor 00111100]).