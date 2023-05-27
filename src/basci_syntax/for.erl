%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. May 2023 8:58 PM
%%%-------------------------------------------------------------------
-module(for).
-author("ruibin").

%% API
-export([start/0]).

for(0, _) -> [];
for(N, F) when N > 0 ->
  [F | for(N-1, F)].

start() ->
  for(5, "Hello").