%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. May 2023 11:09 PM
%%%-------------------------------------------------------------------
-module(process).
-author("ruibin").

%% API
-export([start/0, call/2]).

call(Arg1, Arg2) ->
  io:format("~p ~p~n", [Arg1, Arg2]).

start() ->
  Pid = spawn(?MODULE, call, ["hello", "process"]),
  io:fwrite("~p", [Pid]).