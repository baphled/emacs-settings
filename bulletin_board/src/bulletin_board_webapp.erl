-module(bulletin_board_webapp).

-behaviour(merl_webapp).

-include_lib("merl/include/merl.hrl").

-export([get_mode/1, build_app_definition/0]).

get_mode(_) ->
  development.

build_app_definition() ->
  #webapp{name="bb",
	  routes=[#route{url="/bulletin_board:login",
			 params=[{"user", string},
				 {"password", string}]},

		  #route{url="/bulletin_board:register",
			 params=[{"user", string},
				 {"real_name", string},
				 {"password", string}]}]}.
