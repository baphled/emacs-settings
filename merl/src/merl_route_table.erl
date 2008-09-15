-module(merl_route_table).

-author("kevin@hypotheticalabs.com").

-export([new/0, new_route/3, add_route/2, del_route/2, route/2]).
-export([find_matching_route/4]).

new() ->
  [].

new_route(Name, RoutePred, Handler) when is_function(Handler),
					 is_function(RoutePred) ->
  [{name, Name},
   {route_pred, RoutePred},
   {handler_fun, Handler}];

new_route(Name, RoutePred, Handler) when is_function(Handler) ->
  {ok, RegEx} = re:compile(RoutePred, [caseless]),
  [{name, Name},
   {route_pred, RegEx},
   {handler_fun, Handler}].

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
    Route ->
      {ok, proplists:get_value(handler_fun, Route)}
  end.

%% Internal functions
search(Path, QueryString, Headers, [H|T]) ->
  case test_route(Path, QueryString, Headers, H) of
    true ->
      H;
    false ->
      search(Path, QueryString, Headers, T)
  end;
search(Path, QueryString, Headers, []) ->
  nomatch.

test_route(Path, QueryString, Headers,
	   [{name, _Name}, {route_pred, RoutePred}, {handler_fun, Hander}]) when is_function(RoutePred) ->
  RoutePred(Path, QueryString, Headers);
test_route(Path, QueryString, Headers,
	   [{name, _Name}, {route_pred, RoutePred}, {handler_fun, Hander}]) ->
  case re:run(Path, RoutePred) of
    nomatch ->
      false;
    {match, _} ->
      true
  end.
