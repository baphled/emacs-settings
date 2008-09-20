-module(merl_route_table).

-author("kevin@hypotheticalabs.com").

-export([new/0, new_route/3, add_route/2, del_route/2, route/2]).
-export([find_matching_route/4]).

new() ->
  [].

new_route(Name, RoutePred, Handler) when is_function(Handler),
					 is_function(RoutePred) ->
  case erlang:fun_info(Handler, arity) of
    {arity, 4} ->
      [{name, Name},
       {route_pred, RoutePred},
       {handler, Handler}];
    {arity, _} ->
      throw({wrong_handler_arity, erlang:fun_info(Handler, name)})
  end;

new_route(Name, RoutePred, Handler) when is_function(Handler) orelse
					 is_atom(Handler) ->
  {ok, RegEx} = re:compile(RoutePred, [caseless]),
  [{name, Name},
   {route_pred, RegEx},
   {handler, Handler}].

add_route(Route, RouteTable) ->
  case proplists:get_value(name, Route) of
    undefined ->
      throw(illegal_route);
    Name ->
      [{Name, Route}|RouteTable]
  end.

del_route(RouteName, RouteTable) ->
  lists:filter(fun({Name, _Route}) ->
		   case Name =:= RouteName of
		     true ->
		       false;
		     false ->
		       true
		   end end, RouteTable).

route(RouteName, RouteTable) ->
  proplists:get_value(RouteName, RouteTable).

find_matching_route(Path, QueryString, Headers, RouteTable) ->
  case search(Path, QueryString, Headers, RouteTable) of
    nomatch ->
      {error, nomatch};
    {{_, Route}, RemainingPath} ->
      {ok, RemainingPath, proplists:get_value(handler, Route)}
  end.

%% Internal functions
search(Path, QueryString, Headers, [H|T]) ->
  case test_route(Path, QueryString, Headers, H) of
    {true, RemainingPath} ->
      {H, RemainingPath};
    false ->
      search(Path, QueryString, Headers, T)
  end;
search(Path, QueryString, Headers, []) ->
  nomatch.

test_route(Path, QueryString, Headers,
	   {_, [{name, _Name}, {route_pred, RoutePred}, {handler, Hander}]}) when is_function(RoutePred) ->
  RoutePred(Path, QueryString, Headers);
test_route(Path, QueryString, Headers,
	   {_, [{name, _Name}, {route_pred, RoutePred}, {handler, Hander}]}) ->
  case re:run(Path, RoutePred) of
    nomatch ->
      false;
    {match, [{_Start, End}]} ->
      if
	End + 2 >= length(Path) ->
	  {true, ""};
	true ->
	  {true, string:substr(Path, End + 2)}
      end
  end.
