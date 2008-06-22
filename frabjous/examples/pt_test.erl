-module(pt_test).

-compile([{parse_transform, frabjous}]).
-frabjous([{showast, [{debug, true}]}]).

add(X, Y) ->
  X + Y.
