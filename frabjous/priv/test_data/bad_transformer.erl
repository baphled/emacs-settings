-module(bad_transformer).

-export([transform/3, start/0, is_complete/1]).

transform(_Foo, _Bar, _Baz) ->
  ok.

start() ->
  ok.

is_complete(_Foo) ->
  ok.
