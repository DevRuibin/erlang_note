%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. May 2023 7:56 PM
%%%-------------------------------------------------------------------
-module(boolean).
-author("ruibin").

%% API
-export([start/0]).

start() ->
  io:fwrite(2=<3).
