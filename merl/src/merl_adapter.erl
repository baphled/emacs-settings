-module(merl_adapter).

-author("kevin@hypotheticalabs.com").

-define(DEFAULT_ROUTER_CONFIG, "./config/router.merl").

-export([start/1, start/2, stop/0, service/1]).

-define(EMPTY_BODY, <<"">>).

start(Port) ->
  start(Port, ?DEFAULT_ROUTER_CONFIG).

start(Port, ConfigFilePath) ->
  merl_route_table:start_link(),
  merl_router:start_link(ConfigFilePath),
  mochiweb_http:start([{port, Port},
		       {loop, fun service/1}]),
  io:format("Merl is ready...~n").
stop() ->
  mochiweb_http:stop(),
  merl_router:stop(),
  io:format("Merl has stopped...~n").

service(Req) ->
  service(Req, Req:get(path), []).

service(Req, Path, Accum) ->
  Headers = Req:get(headers),
  QueryString = Req:parse_qs(),
  case merl_router:dispatch(Path, QueryString, Headers, Accum) of
    {ok, {redirect, NewPath}} ->
      send_response(Req, {302, [{"Location", NewPath}], ?EMPTY_BODY});
    {ok, {forward, NewPath}} ->
      service(Req, NewPath, Accum);
    {ok, {forward, NewPath, Data}} ->
      service(Req, NewPath, [{Path, Data}|Accum]);
    {ok, {output, MimeType, Data}} ->
      send_response(Req, {200, [{"Content-Type", MimeType}], assemble_outputs(Accum, Data)});
    {error, MimeType, Data} ->
      send_response(Req, {500, [{"Content-Type", MimeType}], Data})
  end.

%% Internal functions
send_response(Req, {StatusCode, Headers, Body}) ->
  RespHeaders = mochiweb_headers:from_list(Headers),
  case Body =:= ?EMPTY_BODY of
    true ->
      Req:respond({StatusCode, RespHeaders, Body});
    false ->
      Resp = Req:respond({500, RespHeaders, chunked}),
      Resp:write_chunk(Body),
      Resp:write_chunk([])
  end.

assemble_outputs([], Data) ->
  Data;
assemble_outputs(Outputs, Data) ->
  accumulate_outputs(Outputs, [Data]).

accumulate_outputs([{_Path, Output}|T], Accum) ->
  accumulate_outputs(T, [Output|Accum]);
accumulate_outputs([], Accum) ->
  list_to_binary(Accum).
