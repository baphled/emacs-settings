-module(tokenizer_token_resource).

-author("kevin@hypotheticalabs.com").

-export([init/1, post_is_create/2, process_post/2, allowed_methods/2, moved_temporarily/2]).

-include_lib("webmachine/include/webmachine.hrl").

-define(OUTPUT_MIMETYPES, ["text/plain", "application/json", "text/xml"]).

init([]) -> {ok, undefined}.

allowed_methods(_ReqProps, Context) ->
  {['POST'], Context}.

post_is_create(_ReqProps, Context) ->
  {false, Context}.

process_post(ReqProps, Context) ->
  Req = ?REQ(ReqProps),
  Params = parse_params(binary_to_list(Req:recv_body())),
  case find_url(Params) of
    undefined ->
      {false, Context};
    URL ->
      Token = tokenizer_store:associate_token(URL),
      write(Req, Token),
      {true, Context}
  end.

moved_temporarily(_ReqProps, Token) ->
  {true, lists:flatten(io_lib:format("/g?token=~s", [Token]))}.

%% Internal functions
write(Req, Token) ->
  {MimeType, Output} = generate_output(determine_mimetype(Req:get_header_value("Accept")), Token),
  Req:add_response_header("Content-Type", MimeType),
  Req:append_to_response_body(Output).

generate_output(MimeType, Token) when MimeType =:= "text/plain" ->
  {MimeType, build_redirect_url(Token)};
generate_output(MimeType, Token) when MimeType =:= "text/xml" ->
  {MimeType, io_lib:format("<redirect>~s</redirect>", [build_redirect_url(Token)])};
generate_output(MimeType, Token) when MimeType =:= "application/json" ->
  {MimeType, mochijson:encode({struct, {"redirect", build_redirect_url(Token)}})}.

build_redirect_url(Token) ->
  io_lib:format("/r/~s", [Token]).

determine_mimetype(AcceptHeader) ->
  Types = string:tokens(AcceptHeader, ","),
  scan_types(Types, ?OUTPUT_MIMETYPES, "text/plain").

scan_types([H|T], OutputMimeTypes, Default) ->
  case lists:member(string:strip(H), OutputMimeTypes) of
    true ->
      H;
    false ->
      scan_types(T, OutputMimeTypes, Default)
  end;
scan_types([], _, Default) ->
  Default.

find_url(Params) ->
  URL = proplists:get_value("URL", Params,
                            proplists:get_value("url", Params)),
  case URL of
    undefined ->
      undefined;
    V ->
      tokenizer_util:url_decode(V)
  end.

parse_params(Body) when is_list(Body) ->
  Pairs = string:tokens(Body, "&"),
  lists:foldr(fun(NV, Acc) ->
                  [Name, Value] = string:tokens(NV, "="),
                  [{Name, Value}|Acc] end, [], Pairs).
