-module(twoply_http_dispatcher).

-author("kevin@hypotheticalabs.com").

-export([start_link/1, handle_request/1]).

start_link([Config]) ->
  mochiweb_http:start(Config).

handle_request(Req) ->
  ok.
