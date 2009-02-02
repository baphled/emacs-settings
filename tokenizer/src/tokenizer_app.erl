%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the tokenizer application.

-module(tokenizer_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for tokenizer.
start(_Type, _StartArgs) ->
    tokenizer_deps:ensure(),
    tokenizer_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for tokenizer.
stop(_State) ->
    ok.
