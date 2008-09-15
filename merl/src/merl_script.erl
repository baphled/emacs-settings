-module(merl_script).

-author("kevin@hypotheticalabs.com").

-export([execute/2]).

execute(Program, Bindings) when is_binary(Program) ->
  execute(binary_to_list(Program), Bindings);

execute(Program, Bindings) when is_list(Program) ->
  {ok, Tokens, _} = erl_scan:string(Program),
  TokenList = build_token_list(Tokens, [], []),
  execute_exprs(TokenList, Bindings).

%% Internal functions
build_token_list([{dot, _}=H|T], Current, Stack) ->
  build_token_list(T, [], [lists:reverse([H|Current])|Stack]);
build_token_list([H|T], Current, Stack) ->
  build_token_list(T, [H|Current], Stack);
build_token_list([], _Current, Stack) ->
  lists:reverse(Stack).

execute_exprs([H|T], Bindings) ->
  {ok, Exprs} = erl_parse:parse_exprs(H),
  {value, _, NewBindings} = erl_eval:exprs(Exprs, Bindings),
  execute_exprs(T, merge_bindings(Bindings, NewBindings));
execute_exprs([], Bindings) ->
  Bindings.

merge_bindings(Bindings, NewBindings) ->
  B = lists:flatten([NewBindings|Bindings]),
  remove_duplicates(B, []).

remove_duplicates([H|T], Accum) ->
  case lists:member(H, T) of
    true ->
      remove_duplicates(T, Accum);
    false ->
      remove_duplicates(T, [H|Accum])
  end;
remove_duplicates([], Accum) ->
  lists:reverse(Accum).
