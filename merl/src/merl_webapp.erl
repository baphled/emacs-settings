-module(merl_webapp).

-author("kevin@hypotheticalabs.com").

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [{build_app_definition, 0},
   {get_mode, 1}].
