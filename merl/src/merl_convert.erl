-module(merl_convert).

-export([get_converter/1]).

get_converter(string) ->
  fun(X) ->
      X end;
get_converter(integer) ->
  fun(X) ->
      list_to_integer(X) end;
get_converter(atom) ->
  fun(X) ->
      list_to_atom(X) end.
