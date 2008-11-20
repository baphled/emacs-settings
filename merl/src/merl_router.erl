-module(merl_router).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% Public API
-export([start_link/1, reload_config/0, dispatch/4, find_match/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state,
       {config_mod,
	config_checksum,
	mode=development,
	monitor_pid}).

%% Public API

start_link(ConfigModule) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [ConfigModule], []).

reload_config() ->
  gen_server:call(?SERVER, reload_config).

find_match(Path) ->
  gen_server:call(?SERVER, {find_match, Path, "", []}).

dispatch(Path, _QueryString, _Headers, _Data) ->
  case merl_router:find_match(Path) of
    {ok, Route} ->
      Route:invoke(Path);
    {error, nomatch} ->
      {error, "text/html", merl_util:formatb("No match for ~p", [Path])}
  end.

stop() ->
  gen_server:call(?SERVER, stop).

%% gen_server callbacks

init([ConfigModule]) ->
  State = load_configuration(#state{config_mod=ConfigModule}),
  {ok, State}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call({find_match, Path, _QueryString, _Headers}, _From, State) ->
  case merl_route_table:find_match(Path) of
    {ok, Route} ->
      {reply, {ok, Route}, State};
    Error ->
      {reply, Error, State}
  end;

handle_call(reload_config, _From, State) ->
  merl_route_table:clear(),
  NewState = load_configuration(State),
  {reply, {ok, NewState#state.config_checksum}, NewState};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  case State#state.monitor_pid of
    undefined ->
      ok;
    Pid ->
      Pid ! shutdown
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
start_monitor(State) ->
  #state{config_mod=ConfigModule, config_checksum=ConfigChecksum} = State,
  {ok, Pid} = proc_lib:spawn(fun() ->
				 monitor_config(ConfigModule, ConfigChecksum) end),
  State#state{monitor_pid=Pid}.

load_configuration(State) ->
  case is_config_needed(State) of
    false ->
      State;
    true ->
      evaluate_config(State)
  end.

evaluate_config(State) ->
  code:soft_purge(State#state.config_mod),
  code:purge(State#state.config_mod),
  code:delete(State#state.config_mod),
  S1 = State#state{config_checksum=read_config_checksum(State#state.config_mod)},
  S2 = persist_settings(S1),
  case S2#state.mode =:= production andalso is_pid(S1#state.monitor_pid) of
    true ->
      S1#state.monitor_pid ! shutdown;
    false ->
      ok
  end,
  case S2#state.mode =:= development andalso S1#state.monitor_pid =:= undefined of
    true ->
      start_monitor(State);
    false ->
      S2
  end.

persist_settings(State) ->
  #state{config_mod=ConfigMod} = State,
  Mode = ConfigMod:get_mode(),
  AppDef = ConfigMod:build_app_definitions(),
  merl_router:add_app(AppDef),
  State#state{mode=Mode}.

is_config_needed(State) ->
  case State#state.config_checksum of
    undefined ->
      true;
    Value ->
      case Value =:= read_config_checksum(State#state.config_mod) of
	true ->
	  false;
	false ->
	  true
      end
  end.

read_config_checksum(ConfigModule) ->
  {ok, FI} = file:read_file_info(code:which(ConfigModule)),
  erlang:md5(term_to_binary(FI)).

monitor_config(ConfigModule, ConfigChecksum) ->
  NewChecksum = case read_config_checksum(ConfigModule) =:= ConfigChecksum of
		  false ->
		    {ok, CS} = merl_router:reload_config(),
		    CS;
		  true ->
		    ConfigChecksum
		end,
  receive
    shutdown ->
      ok
  after 5000 ->
      ok
  end,
  monitor_config(ConfigModule, NewChecksum).
