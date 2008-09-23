-module(merl_route_table).

-author("kevin@hypotheticalabs.com").

-behaviour(gen_server).

%% API
-export([start_link/0, add_site/2, find_match/1, clear/0]).

-define(SERVER, ?MODULE).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

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

handle_call(clear, _From, _Sites) ->
  {reply, ok, dict:new()};

handle_call({find_match, URL}, _From, Sites) ->
  Reply = case find_best_route(URL, Sites) of
	    nomatch ->
	      {error, no_route};
	    Route ->
	      {ok, Route}
	  end,
  {reply, Reply, Sites};

handle_call({add_site, Name, Routes}, _From, Sites) ->
  CompiledRoutes = [compile_route(R) || R <- Routes],
  {reply, ok, dict:store(Name, CompiledRoutes, Sites)};

handle_call(_Request, _From, Sites) ->
  {reply, ignored, Sites}.

handle_cast(_Msg, Sites) ->
  {noreply, Sites}.

handle_info(_Info, Sites) ->
  {noreply, Sites}.

terminate(_Reason, _Sites) ->
  ok.

code_change(_OldVsn, Sites, _Extra) ->
  {ok, Sites}.

%% Internal functions
find_best_route(URL, Sites) ->
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
				 end, Acc, Routes) end, {-1, nomatch}, Sites) of
    {-1, nomatch} ->
      nomatch;
    {_, Target} ->
      Target
  end.

compile_route({URL, FunRef}) when is_function(FunRef) orelse
				  is_tuple(FunRef) ->
  {ok, RegEx} = re:compile(URL),
  merl_route:new(RegEx, [], FunRef);

compile_route({URL, Specs, FunRef}) when is_function(FunRef) orelse
					 is_tuple(FunRef)->
  {ok, RegEx} = re:compile(URL),
  merl_route:new(RegEx, Specs, FunRef).
