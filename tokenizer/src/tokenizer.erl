%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(tokenizer).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.
%% @spec start() -> ok
%% @doc Start the tokenizer server.
start() ->
  tokenizer_deps:ensure(),
  ensure_started(crypto),
  ensure_started(webmachine),
  mnesia:create_schema([node()]),
  ensure_started(mnesia),
  application:start(tokenizer).

%% @spec stop() -> ok
%% @doc Stop the tokenizer server.
stop() ->
  Res = application:stop(tokenizer),
  application:stop(webmachine),
  application:stop(crypto),
  Res.
