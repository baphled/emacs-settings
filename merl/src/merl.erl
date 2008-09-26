-module(merl).

-author("kevin@hypotheticalabs.com").

-export([generator/1, generator/2]).

generator(URL) when is_list(URL) ->
  case merl_router:find_match(URL) of
    {ok, Route} ->
      {ok, Route:attr(generator)};
    Error ->
      Error
  end.

generator(URL, Params) when is_list(URL), is_list(Params) ->
  case merl:generator(URL) of
    {ok, Gen} ->
      apply(Gen, Params);
    Error ->
      Error
  end.
