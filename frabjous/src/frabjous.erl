-module(frabjous).

-export([parse_transform/2, build_ast/1, atom_to_var/1]).

-define(UPPER_TABLE, [{$a, "A"}, {"a", "A"},
		      {$b, "B"}, {"b", "B"},
		      {$c, "C"}, {"c", "C"},
		      {$d, "D"}, {"d", "D"},
		      {$e, "E"}, {"e", "E"},
		      {$f, "F"}, {"f", "F"},
		      {$g, "G"}, {"g", "G"},
		      {$h, "H"}, {"h", "H"},
		      {$i, "I"}, {"i", "I"},
		      {$j, "J"}, {"j", "J"},
		      {$k, "K"}, {"k", "K"},
		      {$l, "L"}, {"l", "L"},
		      {$m, "M"}, {"m", "M"},
		      {$n, "N"}, {"n", "N"},
		      {$o, "O"}, {"o", "O"},
		      {$p, "P"}, {"p", "P"},
		      {$q, "Q"}, {"q", "Q"},
		      {$r, "R"}, {"r", "R"},
		      {$s, "S"}, {"s", "S"},
		      {$t, "T"}, {"t", "T"},
		      {$u, "U"}, {"u", "U"},
		      {$v, "V"}, {"v", "V"},
		      {$w, "W"}, {"w", "W"},
		      {$x, "X"}, {"x", "X"},
		      {$y, "Y"}, {"y", "Y"},
		      {$z, "Z"}, {"z", "Z"}]).

parse_transform(AST, Opts) ->
  Modules = find_xform_mods(AST, []),
  if
    Modules == not_found ->
      exit("Transform module not found");
    true ->
      NewAST = run_transforms(AST, Opts, Modules),
      lists:flatten(NewAST)
  end.

build_ast(Code) ->
  ScanRes = erl_scan:string(Code),
  {ok, F, _} = ScanRes,
  ParseRes = erl_parse:parse_form(F),
  {ok, Form} = ParseRes,
  Form.

atom_to_var(AtomVar) ->
  Var = atom_to_list(AtomVar),
  capitalize(Var).


%% Private functions
find_xform_mods([{attribute, _, xform_mod, Opts}|T], Accum) ->
  Xformer =
    case proplists:get_value(module, Opts, not_found) of
      not_found ->
	exit("Bad xform_mod attribute");
      Mod ->
	verify_module(Mod),
	{Mod, false}
  end,
  case proplists:get_value(debug, Opts, false) of
    true ->
      {M1, _} = Xformer,
      find_xform_mods(T, lists:append(Accum, [{M1, true}]));
    false ->
      find_xform_mods(T, lists:append(Accum, [Xformer]))
  end;
find_xform_mods([_H|T], Accum) ->
  find_xform_mods(T, Accum);
find_xform_mods([], Accum) ->
  if
    length(Accum) == 0 ->
      not_found;
    true ->
      Accum
  end.

run_transforms(AST, Opts, [{Mod, Debug}|T]) ->
  State = Mod:start(),
  put(xform_state, State),
  run_transforms(run_transformer(AST, Opts, Mod, Debug), Opts, T);
run_transforms(AST, _Opts, []) ->
  AST.

run_transformer(AST, Opts, Mod, Debug) ->
  F = fun(Node) ->
	  S = get(xform_state),
	  {S1, N1} = Mod:transform(Node, Opts, S),
	  put(xform_state, S1),
	  N1 end,
  NewAST = lists:map(F, AST),
  print_debug(Debug, AST, NewAST),
  IsComplete = Mod:is_complete(get(xform_state)),
  if
    IsComplete == true ->
      erase(xform_state),
      NewAST;
    true ->
      run_transformer(NewAST, Opts, Mod, Debug)
  end.


print_debug(true, AST, NewAST) ->
  io:format("~n~n-----AST Before:~n~p~n~nAST After:~n~p~n-----~n~n", [AST, NewAST]);
print_debug(false, _AST, _NewAST) ->
  ok.

capitalize(Arg) when length(Arg) > 1 ->
  [H|T] = Arg,
  lists:flatten([char_to_upper(H) | T]);
capitalize(Arg) when length(Arg) == 1 ->
  char_to_upper(Arg).

char_to_upper(C) ->
  proplists:get_value(C, ?UPPER_TABLE, C).

verify_module(Mod) ->
  ModInfo = Mod:module_info(),
  Attrs = proplists:get_value(attributes, ModInfo, []),
  IsTransformer = is_transformer(Attrs),
  if IsTransformer == true ->
      ok;
     true ->
      exit("Module '" ++ atom_to_list(Mod) ++ "' doesn't implement ast_transformer behavior")
  end.

is_transformer(Attrs) ->
  Behaviors = proplists:get_value(behavior, Attrs, []),
  lists:member(ast_transformer, Behaviors).
