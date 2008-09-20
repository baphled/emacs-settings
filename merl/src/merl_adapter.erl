-module(merl_adapter).

-author("kevin@hypotheticalabs.com").

-define(DEFAULT_ROUTER_CONFIG, "./config/router.merl").

-export([start/1, start/2, stop/0, service/1]).

start(Port) ->
  start(Port, ?DEFAULT_ROUTER_CONFIG).

start(Port, ConfigFilePath) ->
  merl_router:start_link(ConfigFilePath),
  mochiweb_http:start([{port, Port},
		       {loop, fun service/1}]),
  io:format("Merl is ready...~n").
stop() ->
  mochiweb_http:stop(),
  merl_router:stop(),
  io:format("Merl has stopped...~n").

service(Req) ->
  Headers = Req:get(headers),
  QueryString = Req:parse_qs(),
  Path = Req:get(path),
  case merl_router:dispatch(Path, QueryString, Headers, []) of
    {ok, MimeType, Data} ->
      RespHeaders = mochiweb_headers:from_list([{"Content-Type", MimeType}]),
      Resp = Req:respond({200, RespHeaders, chunked}),
      Resp:write_chunk(Data),
      Resp:write_chunk([]);
    {error, MimeType, Data} ->
      RespHeaders = mochiweb_headers:from_list([{"Content-Type", MimeType}]),
      Resp = Req:respond({500, RespHeaders, chunked}),
      Resp:write_chunk(Data),
      Resp:write_chunk([])
  end.
