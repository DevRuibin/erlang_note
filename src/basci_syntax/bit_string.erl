%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. May 2023 7:57 PM
%%%-------------------------------------------------------------------
-module(bit_string).
-author("ruibin").

%% API
-export([start/0]).

start() ->
  Bin1 = <<1,2,3>>,
  X = binary_to_list(Bin1),
  io:fwrite("~p~n", [X]).