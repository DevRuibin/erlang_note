%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. May 2023 10:28 PM
%%%-------------------------------------------------------------------
-module(preProcessors).
-author("ruibin").

%% API
-export([start/0]).
-include("user.hrl").

start() ->
  P = #user{name="Rueibin Chang", age=19, address = "LLN"},
  io:fwrite("~w~n", [P#user.name]).