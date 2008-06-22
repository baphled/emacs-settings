-module(showast).

-behavior(ast_transformer).

-export([transform/3, start/1, is_complete/1]).

transform(Node, _Opts, State) ->
  {Node, State}.

start(State) ->
  case State of
    nil ->
      io:format("First time!~n");
    _ ->
      ok
  end.

is_complete(_State) ->
  true.
