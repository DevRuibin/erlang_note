%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. May 2023 7:33 PM
%%%-------------------------------------------------------------------
-module(import).
-author("ruibin").
-import(io, [fwrite/1]).

%% API
-export([start/0]).

start() ->
  fwrite("Hello World!~n").
