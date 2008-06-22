-module(single_pass).

-behavior(ast_transformer).

-export([transform/3, start/0, is_complete/1]).

transform(Node, _Opts, State) ->
  {State, Node}.

start() ->
  1.

is_complete(State) ->
  if
    State == 1 ->
      true;
    true ->
      exit("Unexpected pass")
  end.
