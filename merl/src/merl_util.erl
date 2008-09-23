-module(merl_util).

-author("kevin@hypotheticalabs.com").

-export([formatb/2, fun_name/1]).

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
