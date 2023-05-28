%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. May 2023 9:21 PM
%%%-------------------------------------------------------------------
-module(exception).
-author("ruibin").

%% API
-compile(export_all). % export all functions

generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> exit(a);
generate_exception(4) -> erlang:error(a);
generate_exception(5) -> {'EXIT', a}.

demo1() ->
  [catcher(I) || I <- lists:seq(1, 5)].

catcher(N) ->
  try generate_exception(N) of
    Val -> {N, normal, Val}
  catch
    throw:Val -> {N, throw, Val};
    exit:Val -> {N, exit, Val};
    error:Val -> {N, error, Val}
  end.

demo2() ->
  [{I, (catch generate_exception(I))} || I <- lists:seq(1, 5)].

demo3() ->
  try generate_exception(5)
  catch
    error:X -> {X, erlang:get_stacktrace()}
  end.

loopUp(N) ->
  case(N) of
    1 -> {'EXIT', a};
    2 -> exit(a)
  end.
