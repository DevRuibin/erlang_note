%%%-------------------------------------------------------------------
%%% @author ruibin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. May 2023 8:43 PM
%%%-------------------------------------------------------------------
-module(myRecord).
-author("ruibin").


%% API
-export([start/0]).
-record(person, {name, id}).
-record(employee, {person, salary}).

start() ->
  P = #person{name="ruibin", id=1},
  io:fwrite("~p~n", [P#person.name]), % Access the field name of the record P
  P#person{id=2}, % Update the field id of the record P
  E = #employee{person=P, salary=10000}, % Nested record
  io:fwrite("~p~n", [E#employee.person#person.name]). % Access the field name of the record E