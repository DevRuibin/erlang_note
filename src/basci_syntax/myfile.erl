%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. May 2023 10:16 PM
%%%-------------------------------------------------------------------
-module(myfile).
-author("ruibin").

%% API
-export([start/0]).

start() ->
  {ok, File} = file:open("test.txt", [read, write]),
  file:write(File, "Hello world!~n"),
  file:close(File),
  {ok, File1} = file:open("test.txt", [read]),
  Result = file:read(File1, 10),
  io:fwrite("~p~n", [Result]),
  file:close(File1).
