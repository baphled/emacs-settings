-module(merl_route, [URL, RawURL, Paramspec, FunName, FunRef, Generator]).

-author("kevin@hypotheticalabs.com").

-export([matches/1, invoke/1, attr/1]).

-define(tail, fun(L) -> lists:nth(length(L), L) end).

attr(url) ->
  URL;
attr(name) ->
  FunName;
attr(mapping) ->
  RawURL;
attr(generator) ->
  Generator.

matches(CandidateURL) ->
  case re:run(CandidateURL, URL) of
    {match, [{Start, End}]} ->
      {match, End - Start};
    Other ->
      Other
  end.

invoke(SubmittedURL) ->
  Params = read_params(SubmittedURL),
  case ref_type(FunRef) of
    function ->
      erlang:apply(FunRef, Params);
    mfa ->
      {M, F} = FunRef,
      erlang:apply(M, F, Params)
  end.

%% Internal Functions
read_params(SubmittedURL) ->
  case length(Paramspec) > 0 of
    true ->
      RawParams = string:tokens(extract_params(SubmittedURL), "/"),
      case length(RawParams) == length(Paramspec) orelse
	?tail(Paramspec) =:= rest of
	true ->
	  convert_params(RawParams);
	false ->
	  throw({invalid_params, URL, RawParams})
      end;
    false ->
      []
  end.

convert_params(RawParams) ->
  convert_params(RawParams, Paramspec, []).

convert_params([Param|PT]=Params, [Spec|ST], Accum) ->
  Value = case Spec of
	    int ->
	      list_to_integer(Param);
	    float ->
	      list_to_float(Param);
	    atom ->
	      list_to_atom(Param);
	    rest ->
	      reassemble_rest(Params, ["/"]);
	    string ->
	      Param
	  end,
  if
    Spec =:= rest ->
      convert_params([], [], [Value|Accum]);
    true ->
      convert_params(PT, ST, [Value|Accum])
  end;
convert_params([], [], Accum) ->
  lists:reverse(Accum).

extract_params(SubmittedURL) ->
  {match, Captured} = matches(SubmittedURL),
  string:substr(SubmittedURL, Captured + 1).

reassemble_rest([H|T], Accum) ->
  A1 = [H|Accum],
  case length(T) > 0 of
    true ->
      reassemble_rest(T, ["/"|A1]);
    false ->
      reassemble_rest(T, A1)
  end;
reassemble_rest([], Accum) ->
  lists:flatten(lists:reverse(Accum)).

ref_type(FunRef) ->
  case is_function(FunRef) of
    true ->
      function;
    false ->
      case is_tuple(FunRef) andalso
	size(FunRef) == 2 of
	true ->
	  mfa;
	false ->
	  unknown
      end
  end.
