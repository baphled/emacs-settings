-module(ast_transformer).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [{start, 0},
   {is_complete, 1},
   {transform, 3}].
