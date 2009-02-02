%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the tokenizer application.

-module(tokenizer_sup).
-author('kevin@hypotheticalabs.com').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
	    [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
		      supervisor:terminate_child(?MODULE, Id),
		      supervisor:delete_child(?MODULE, Id),
		      ok
	      end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    Dispatch = [{["s"], tokenizer_token_resource, []},
                {["r", '*'], tokenizer_redirect_resource, []}],
    WebConfig = [
		 {ip, Ip},
		 {port, 9000},
                 {log_dir, "priv/log"},
		 {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
	   {webmachine_mochiweb, start, [WebConfig]},
	   permanent, 5000, worker, dynamic},
    Store = {tokenizer_store,
             {tokenizer_store, start_link, []},
             permanent, 5000, worker, dynamic},
    Processes = [Web, Store],
    {ok, {{one_for_one, 10, 10}, Processes}}.
