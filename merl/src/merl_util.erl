-module(merl_util).

-author("kevin@hypotheticalabs.com").

-export([formatb/2, fun_name/1, make_generator/2, pretty_float/1]).

formatb(Template, Values) when is_list(Values) ->
  list_to_binary(lists:flatten(io_lib:format(Template, Values))).

fun_name(TargetFun) when is_function(TargetFun) ->
  case erlang:fun_info(TargetFun, module) of
    {module, erl_eval} ->
      "anonymous";
    {module, ModName} ->
      {name, FunName} = erlang:fun_info(TargetFun, name),
      {arity, Arity} = erlang:fun_info(TargetFun, arity),
      lists:flatten([atom_to_list(ModName), ":", atom_to_list(FunName), "/", integer_to_list(Arity)])
  end.

make_generator(URL, ParamSpec) when is_list(ParamSpec) ->
  case length(ParamSpec) of
    0 ->
      fun() -> URL end;
    Value ->
      S1 = lists:foldl(fun(P, Acc) ->
			   if P < Value ->
			       Acc ++ "P" ++ integer_to_list(P) ++ ",";
			      true ->
			       Acc ++ "P" ++ integer_to_list(P) ++ ") "
			   end end,
		       "fun(", lists:seq(1, Value)),
      {_, S2} = lists:foldl(fun(PS, {Pos, Sig}) ->
			  NewSig = Sig ++ find_guard(PS) ++ "(P" ++ integer_to_list(Pos) ++ ")",
			  {Pos + 1, if
				      Pos < Value ->
					NewSig ++ ", ";
				      true ->
					NewSig ++ " -> "
				    end} end,
		      {1, S1 ++ "when "},
		      ParamSpec),
      {_, S3} = lists:foldl(fun(PS, {Pos, Body}) ->
				NewBody = Body ++ "++ " ++ make_converter_call(PS, Pos),
				{Pos + 1, if
					    Pos < Value ->
					      NewBody ++ " ++ \"/\"";
					    true ->
					      NewBody ++ " end."
					  end} end,
			    {1, S2 ++ "\"" ++ URL ++ "\"" ++ " ++ \"/\" "},
			    ParamSpec),
      {ok, T, _} = erl_scan:string(S3),
      {ok, IR} = erl_parse:parse_exprs(T),
      {value, FC, _} = erl_eval:exprs(IR, erl_eval:new_bindings()),
      FC
  end.

pretty_float(Number) when is_float(Number) ->
  lists:flatten(io_lib:format("~f", [Number])).

%% Internal functions
find_guard(int) ->
  "is_integer";
find_guard(string) ->
  "is_list";
find_guard(float) ->
  "is_float".

make_converter_call(PS, Pos) ->
  case find_converter(PS) of
    "" ->
     "P" ++ integer_to_list(Pos);
    C ->
      C ++ "(P" ++ integer_to_list(Pos) ++ ")"
  end.

find_converter(int) ->
  "integer_to_list";
find_converter(string) ->
  "";
find_converter(float) ->
  "merl_util:pretty_float".
