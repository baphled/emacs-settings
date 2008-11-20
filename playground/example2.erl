-module(example2).

-export([init/0, undefined_function/2]).

-author("kevin@hypotheticalabs.com").

init() ->
  code:ensure_loaded(function_missing),
  process_flag(error_handler, function_missing),
  ok.

undefined_function(FunName, Args) when is_atom(FunName) ->
  io:format("Generating function...~n"),
  FunNameStr = atom_to_list(FunName),
  case is_finder(atom_to_list(FunName)) of
    false ->
      {false, []};
    true ->
      {ok, M1} = smerl:for_module(?MODULE),
      FieldName = extract_find_field(FunNameStr),
      {NewFunName, NewFunBody} = generate_finder(FieldName, Args),
      {ok, M2} = smerl:add_func(M1, NewFunBody),
      smerl:compile(M2, [debug_info,
			 {outdir, "./ebin"}]),
      code:ensure_loaded(?MODULE),
      {true, apply(?MODULE, NewFunName, Args)}
  end.

%% Internal functions
is_finder(Name) ->
  string:str(Name, "find_by_") == 1.

extract_find_field(Name) ->
  Chunks = string:tokens(Name, "_"),
  erlang:hd(lists:reverse(Chunks)).

generate_finder(FieldName, Args) ->
  Name = lists:flatten(io_lib:format("find_by_~s", [FieldName])),
  ParamList = build_param_list(lists:seq(1, length(Args)), ""),
  Signature = io_lib:format("~s(~s) -> ", [Name, ParamList]),
  FormattedList = lists:flatten(io_lib:format("[~s]", [ParamList])),
  Body = "io:format(\"find_by_" ++ FieldName ++ ": ~p~n\", " ++ FormattedList ++ ").",
  {list_to_atom(Name), lists:flatten([Signature, Body])}.

build_param_list([H|T], Accum) ->
  case H > 1 of
    true ->
      case length(T) > 0 of
	true ->
	  build_param_list(T, Accum ++ "P" ++ integer_to_list(H) ++ ",");
	false ->
	  build_param_list(T, Accum ++ "P" ++ integer_to_list(H))
      end;
    false ->
      build_param_list(T, Accum ++ "P" ++ integer_to_list(H))
  end;
build_param_list([], Accum) ->
  Accum.
