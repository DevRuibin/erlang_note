%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. May 2023 9:38 PM
%%%-------------------------------------------------------------------
-module( if).
-author("ruibin").

%% API
-export([]).

if1() ->
  A = 5,
  B = 6,
  if
    A == B ->
      io:fwrite("A is equal to B~n");
    true -> % it is a default branch
      io:fwrite("A is not equal to B~n")
  end.

if2() ->
  A = 5,
  B = 6,
  if
    A == B ->
      io:fwrite("A is equal to B~n");
    A < B ->
      io:fwrite("A is less than B~n");
    true -> % it is a default branch
      io:fwrite("A is not equal to B~n")
  end.

% An example of nested if

if3() ->
  A = 5,
  B = 6,
  if
    A == B ->
      io:fwrite("A is equal to B~n");
    A < B ->
      if
        A>=5 -> io:fwrite("A is less than B and greater than 5~n");
        true -> io:fwrite("A is less than B and less than 5~n")
      end;
      true -> % it is a default branch
      io:fwrite("A is not equal to B~n")
  end.

% An example of case

case1() ->
  A = 5,
  B = 6,
  case A == B of
    true -> io:fwrite("A is equal to B~n");
    false -> io:fwrite("A is not equal to B~n")
  end.