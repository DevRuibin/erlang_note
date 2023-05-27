%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. May 2023 9:58 PM
%%%-------------------------------------------------------------------
-module(recursion).
-author("ruibin").

%% API
-export([factorial/1, len/1, tail_len/1]).

% A function to calculate the factorial of a number
factorial(0) -> 1;
factorial(N) when N > 0 ->
  N * factorial(N-1).

% Calculate the length of a list
len([]) -> 0;
len([_|T]) ->
  1 + len(T).

% Another way to calculate the length of a list
tail_len(L) -> tail_len(L, 0).
tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) ->
  tail_len(T, Acc+1).

% It takes an integer N and any other term Z as arguments.
% It returns a list of N elements, each of which is Z.
% For example, replicate(3, 5) returns [5, 5, 5].
replicate(0, _) -> [];
replicate(N, Z) when N > 0 ->
  [Z | replicate(N-1, Z)].

% Reverse a list
% For example, [a, b, c] -> [c, b, a]
% Below is the deduction process.
% reverse([a, b, c]) -> reverse([a|b, c], [])
%                    -> reverse([b, c], [a | []])
%                    -> reverse([c], [b | [a | []]])
%                    -> reverse([], [c | [b | [a | []]]])
%                    -> [c, b, a]
tail_reverse(L) -> tail_reverse(L, []).
tail_reverse([], Acc) -> Acc;
tail_reverse([H|T], Acc) ->
  tail_reverse(T, [H|Acc]).