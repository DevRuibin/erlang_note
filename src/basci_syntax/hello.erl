%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. May 2023 7:25 PM
%%%-------------------------------------------------------------------
-module(hello).
-author("ruibin").

%% API
-export([start/0]).

start() ->
  io:fwrite("Hello World!~n").
