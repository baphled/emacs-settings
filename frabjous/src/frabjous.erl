%% Copyright 2008 Kevin A. Smith
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(frabjous).

-export([parse_transform/2, build_ast/1, atom_to_var/1]).

%%---------------------------------------------------
%% parse_transform(AST, Opts) -> AST
%%
%%  AST -> list(terms)
%%  Opts -> list(terms)
%%---------------------------------------------------
parse_transform(AST, Opts) ->
  Modules = find_xform_mods(AST, []),
  if
    Modules == not_found ->
      exit("Transform module not found");
    true ->
      NewAST = run_transforms(AST, Opts, Modules),
      lists:flatten(NewAST)
  end.

%%---------------------------------------------------
%% build_ast(Code) -> AST
%%
%%  Code -> string()
%%  AST -> list(terms)
%%---------------------------------------------------
build_ast(Code) ->
  ScanRes = erl_scan:string(Code),
  {ok, F, _} = ScanRes,
  ParseRes = erl_parse:parse_form(F),
  {ok, Form} = ParseRes,
  Form.

%%---------------------------------------------------
%% atom_to_var(AtomVar) -> string()
%%
%%  AtomVar -> atom
%%---------------------------------------------------
atom_to_var(AtomVar) ->
  Var = atom_to_list(AtomVar),
  capitalize(Var).


%% Private functions
find_xform_mods([{attribute, _, xform_mod, Opts}|T], Accum) ->
  %% Find and verify the transformer module
  Xformer =
    case proplists:get_value(module, Opts, not_found) of
      not_found ->
	exit("Bad xform_mod attribute");
      Mod ->
	verify_module(Mod),
	{Mod, false}
  end,
  %% We've gotten this far, so store the transformer
  %% module name away along with its debug setting
  case proplists:get_value(debug, Opts, false) of
    true ->
      {M1, _} = Xformer,
      find_xform_mods(T, lists:append(Accum, [{M1, true}]));
    false ->
      find_xform_mods(T, lists:append(Accum, [Xformer]))
  end;

%% These don't match so let them pass thru
find_xform_mods([_H|T], Accum) ->
  find_xform_mods(T, Accum);

%% We're done iterating over the AST, so let's return
%% the list of transformers we've accumulated
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
  F = fun(Node) -> parse_node(Node, Mod, Opts) end,
  NewAST = lists:map(F, AST),
  print_debug(Debug, AST, NewAST),
  case Mod:is_complete(get(xform_state)) of
    %% Current transformer is done
    %% Erase transformer state and return the
    %% newly massaged AST
    true ->
      erase(xform_state),
      NewAST;
    %% Current transformer wants another pass
    %% So let's iterate
    {false, NewState} ->
      put(xform_state, NewState),
      run_transformer(NewAST, Opts, Mod, Debug);
    false ->
      run_transformer(NewAST, Opts, Mod, Debug)
  end.

parse_node(Node, Mod, Opts) ->
  S = get(xform_state),
  {S1, N1} = Mod:transform(Node, Opts, S),
  put(xform_state, S1),
  N1.

print_debug(true, AST, NewAST) ->
  io:format("~n~n-----AST Before:~n~p~n~nAST After:~n~p~n-----~n~n", [AST, NewAST]);
print_debug(false, _AST, _NewAST) ->
  ok.

capitalize(Arg) when is_list(Arg) ->
  [H|T] = Arg,
  lists:append([string:to_upper(H)], T).

verify_module(Mod) ->
  case code:which(Mod) of
    non_existing ->
      exit("Module '" ++ atom_to_list(Mod) ++ "' doesn't exist");
    _ ->
      ok
  end,
  ModInfo = Mod:module_info(),
  Attrs = proplists:get_value(attributes, ModInfo, []),
  case is_transformer(Attrs) of
    true ->
      ok;
    false ->
      exit("Module '" ++ atom_to_list(Mod) ++ "' doesn't implement ast_transformer behavior")
  end.

is_transformer(Attrs) ->
  lists:member(ast_transformer, proplists:get_value(behavior, Attrs)).
