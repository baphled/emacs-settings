-module(twoply_supervisor).

-behaviour(supervisor).

-include("twoply.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  application:load(?TWOPLY_APP_NAME),
  HttpDispatcher = {twoply_http_dispatcher, {twoply_http_dispatcher, start_link, [[build_http_config()]]},
		    permanent, 2000, worker, [twoply_http_dispatcher]},
  {ok, {{one_for_one, 5, 30}, [HttpDispatcher]}}.

%% Internal functions
build_http_config() ->
  Config = lists:foldl(fun(ConfigName, Acc) ->
			   case application:get_env(?TWOPLY_APP_NAME, ConfigName) of
			     undefined ->
			       Acc;
			     {ok, Value} ->
			       [{ConfigName, Value}|Acc] end end, [], ?TWOPLY_HTTP_CONFIG_PARAMS),
  [{loop, fun twoply_http_dispatcher:handle_request/1}|Config].
