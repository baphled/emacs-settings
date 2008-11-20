-module(merl_route_table).

-author("kevin@hypotheticalabs.com").

-include("merl.hrl").
-include("merl_internal.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, add_app/1, find_match/1, clear/0]).

-define(SERVER, ?MODULE).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state,
	{sites=dict:new(),
	 controllers = dict:new()}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_app(App) ->
  gen_server:call(?SERVER, {add_app, App}).

find_match(URL) ->
  gen_server:call(?SERVER, {find_match, URL}).

clear() ->
  gen_server:call(?SERVER, clear).

init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{}}.

handle_call(clear, _From, _State) ->
  {reply, ok, #state{}};

handle_call({find_url, FunRef}, _From, State) ->
  FunName = merl_util:fun_name(FunRef),
  URL = case dict:find(FunName, State#state.controllers) of
	  {ok, Route} ->
	    Route:attr(mapping);
	  error->
	    none
	end,
  {reply, {ok, URL}, State};

handle_call({find_match, URL}, _From, State) ->
  Reply = case find_best_route(URL, State) of
	    nomatch ->
	      {error, no_route};
	    Route ->
	      {ok, Route}
	  end,
  {reply, Reply, State};

handle_call({add_app, App}, _From, State) ->
  CompiledRoutes = [compile_route(R) || R <- App#webapp.routes],
  NewState = State#state{sites=dict:store(Name, CompiledRoutes, State#state.sites),
			 controllers=update_controllers(CompiledRoutes, State#state.controllers)},
  {reply, ok, NewState};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
find_best_route(URL, State) ->
  case dict:fold(fun(_Site, Routes, Acc) ->
		     lists:foldl(fun(Route, {Size, Candidate}) ->
				     case Route:matches(URL) of
				       nomatch ->
					 {Size, Candidate};
				       {match, Captured} ->
					 if
					   Captured > Size ->
					     {Captured, Route};
					   true ->
					     {Size, Candidate}
					 end
				     end
				 end, Acc, Routes) end, {-1, nomatch}, State#state.sites) of
    {-1, nomatch} ->
      nomatch;
    {_, Target} ->
      Target
  end.

update_controllers(Routes, Controllers) ->
  lists:foldl(fun(R, C) ->
		  case R:attr(name) of
		    "anonymous" ->
		      C;
		    Name ->
		      dict:store(Name, R, C)
		  end end,
	      Controllers, Routes).

compile_route(Route) ->
  #route{url=URL, params=Params} = Route,
  [Handler|Parts] = string:tokens(URL, "/"),
  CR = resolve_handler(Handler, Route),
  CR1 = compile_parts(CR, Parts),
  compile_params(CR1, Params).

resolve_handler(Handler, Route) ->
  CR = #compiled_route{},
  case Route#route.target =:= undefined of
    true ->
      case string:tokens(Handler, ":") of
	[M, F] ->
	  CR#compiled_route{target={M, F},
			    root_url=F};
	[_] ->
	  throw({illegal_route_spec, merl_util:format("Missing target fun: ~s", [Handler])})
      end;
    false ->
      CR#compiled_route{target=Route#route.target,
			root_url=Route#route.url}
  end.

compile_parts(CR, Parts) ->
  compile_parts(CR, Parts, []);
compile_parts(CR, []) ->
  CR.

compile_parts(CR, [{Regex, DataType}|T], Accum) ->
  {ok, RE} = re:compile(Regex),
  compile_parts(CR, T, [{RE, merl_convert:get_converter(DataType)}|Accum]);
compile_parts(CR, [], Accum) ->
  CR#compiled_route{url_regexen=lists:reverse(Accum)}.

compile_params(CR, []) ->
  CR;
compile_params(CR, Params) ->
  compile_params(CR, Params, []).

compile_params(CR, [{Name, DataType}|T], Accum) ->
  compile_params(CR, T, [{Name, merl_convert:get_converter(DataType)}|Accum]);
compile_params(CR, [], Accum) ->
  CR#compiled_route{params=lists:reverse(Accum)}.
