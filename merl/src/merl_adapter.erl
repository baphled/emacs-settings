-module(merl_adapter).

-author("kevin@hypotheticalabs.com").

-define(DEFAULT_ROUTER_CONFIG, "./config/router.es").

-export([start/1, start/2, stop/0]).

start(Port) ->
  start(Port, ?DEFAULT_ROUTER_CONFIG).

start(Port, ConfigFilePath) ->
  merl_router:start_link(ConfigFilePath),
  mochiweb_http:start([{port, Port},
		       {loop, fun(Req) -> merl_router:dispatch(Req) end}]).
stop() ->
  mochiweb_http:stop().
