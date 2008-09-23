-module(merl_route_table).

-author("kevin@hypotheticalabs.com").

-behaviour(gen_server).

%% API
-export([start_link/0, add_site/2, find_match/1, clear/0]).

-define(SERVER, ?MODULE).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state,
	{sites=dict:new(),
	 controllers = dict:new()}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_site(Name, Routes) ->
  gen_server:call(?SERVER, {add_site, Name, Routes}).

find_match(URL) ->
  gen_server:call(?SERVER, {find_match, URL}).

clear() ->
  gen_server:call(?SERVER, clear).

init([]) ->
  process_flag(trap_exit, true),
  {ok, dict:new()}.

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

handle_call({add_site, Name, Routes}, _From, State) ->
  CompiledRoutes = [compile_route(R) || R <- Routes],
  NewState = State#{sites=dict:store(Name, CompiledRoutes, State#state.sites),
		    controllers=update_controllers(CompiledRoutes, State#state.controllers)},
  {reply, ok, dict:store(Name, CompiledRoutes, NewState)};

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
				 end, Acc, Routes) end, {-1, nomatch}, State) of
    {-1, nomatch} ->
      nomatch;
    {_, Target} ->
      Target
  end.

update_controllers(Routes, Controllers) ->
  lists:foldl(fun(R, C) ->
		  case R:attr(name)
		    "anonymous" ->
		      C;
		    Name ->
		      dict:store(Name, R, C)
		  end end,
	      Controllers, Routes).

compile_route({URL, FunRef}) when is_function(FunRef) ->
  {ok, RegEx} = re:compile(URL),
  merl_route:new(RegEx, URL, [], merl_util:fun_name(FunRef), FunRef);

compile_route({URL, Specs, FunRef}) when is_function(FunRef) ->
  {ok, RegEx} = re:compile(URL),
  merl_route:new(RegEx, URL, Specs, merl_util:fun_name(FunRef), FunRef).
