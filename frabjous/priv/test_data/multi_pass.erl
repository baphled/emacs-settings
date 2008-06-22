-module(multi_pass).

-behavior(ast_transformer).

-export([transform/3, start/0, is_complete/1]).

transform(Node, _Opts, State) ->
  {State, Node}.

start() ->
  {1, 3}.

is_complete(State) ->
  {Count, Max} = State,
  case Count == Max of
    true ->
      true;
    false ->
      {false, {Count + 1, Max}}
  end.
