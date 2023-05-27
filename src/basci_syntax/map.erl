%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. May 2023 8:06 PM
%%%-------------------------------------------------------------------
-module(map).
-author("ruibin").

%% API
-export([start/0]).

start() ->
  Map1 = #{name=>"ruibin", age=>23},
  io:fwrite("Map1: ~p~n", [Map1]).


