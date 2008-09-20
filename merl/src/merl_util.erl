-module(merl_util).

-author("kevin@hypotheticalabs.com").

-export([formatb/2]).

formatb(Template, Values) when is_list(Values) ->
  list_to_binary(lists:flatten(io_lib:format(Template, Values))).
