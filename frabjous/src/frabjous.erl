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

-export([parse_transform/2, invoke_transformer/3]).

-define(PASS_COUNT_THRESHOLD, 5).

%%---------------------------------------------------
%% parse_transform(AST, Opts) -> AST
%%
%%  AST -> list(terms)
%%  Opts -> proplist()
%%---------------------------------------------------
parse_transform(AST, Opts) ->
  Transformers = resolve_transformers(AST),
  case length(Transformers) == 0 of
    true ->
      AST;
    false ->
      run_all_transformers(Transformers, AST, Opts)
  end.

%%---------------------------------------------------
%% invoke_transformer(Transformer, AST, Opts) -> AST
%%
%%  Transformer -> atom
%%  AST -> list(terms)
%%  Opts -> proplist()
%%---------------------------------------------------
invoke_transformer(Transformer, AST, Opts) ->
  increment_and_check_counter(Transformer),
  setup_state(Transformer),
  NewAST = ast_walker:walk(Transformer, AST, Opts),
  EndingState = get(xform_state),
  case Transformer:is_complete(EndingState) of
    true ->
      NewAST;
    false ->
      invoke_transformer(Transformer, NewAST, Opts)
  end.

%% Private functions

%% Increment the pass counter
%% Emit a warning if we've exceeded the pass count threshold for a single transformer
increment_and_check_counter(Transformer) ->
  PassCount = get(xform_pass) + 1,
  if
    PassCount > ?PASS_COUNT_THRESHOLD ->
      io:format("WARNING: Transformer has executed ~s passes -- is it stuck?~n", [integer_to_list(PassCount)]),
      if
	PassCount > ?PASS_COUNT_THRESHOLD * 3 ->
	  exit("Killing runaway transformer '" ++ atom_to_list(Transformer) ++ "'");
	true ->
	  ok
      end;
    true ->
      ok
  end,
  put(xform_pass, PassCount).

%% Run down the list of transformers and execute each one
run_all_transformers([Transformer|T], AST, Opts) ->
  put(xform_state, undef),
  put(xform_pass, 0),
  run_all_transformers(T, run_transformer(Transformer, AST, Opts), Opts);
run_all_transformers([], AST, _Opts) ->
  AST.

%% Wrapper around the recursive AST walk performed by each transformer
run_transformer({Transformer, Debug}, AST, Opts) ->
  NewAST = invoke_transformer(Transformer, AST, Opts),
  if
    Debug == true ->
      print_debug_trace(AST, NewAST);
    true ->
      ok
  end,
  NewAST.

%% Sets up state for each pass
setup_state(Transformer) ->
  case get(xform_state) of
    undef ->
      put(xform_state, Transformer:start(nil));
    State ->
      put(xform_state, Transformer:start(State))
  end.

%% Resolve the transformers given in the frabjous attribute
%% We stop walking the AST after we find the first frabjous attribute
resolve_transformers(AST) ->
  resolve_transformers(AST, []).
resolve_transformers([{attribute, _, frabjous, Options}|_T], Accum) ->
  extract_transformers(Options, Accum);
resolve_transformers([_H|T], Accum) ->
  resolve_transformers(T, Accum);
resolve_transformers([], Accum) ->
  Accum.

%% Validate the transformer and determine its desired debug output levels
extract_transformers([{ModName}|T], Accum) ->
  extract_transformers(T, lists:append(Accum, [build_transformer_record(ModName, [])]));
extract_transformers([{ModName, ModOpts}|T], Accum) ->
  %% Can we load the module?
  case code:which(ModName) of
    non_existing ->
      exit("Module '" ++ atom_to_list(ModName) ++ "' couldn't load");
    _ ->
      %% Does the module implement the ast_transformer behavior
      case is_transformer(ModName) of
	true ->
	  extract_transformers(T, lists:append(Accum, [build_transformer_record(ModName, ModOpts)]));
	false ->
	  exit("Module '" ++ atom_to_list(ModName) ++ "' doesn't implement ast_transformer behavior")
      end
  end;
extract_transformers([], Accum) ->
  Accum.

build_transformer_record(ModName, ModOpts) ->
  %% Can we load the module?
  case code:which(ModName) of
    non_existing ->
      exit("Couldn't load module '" ++ atom_to_list(ModName) ++ "'");
    _ ->
      %% Does the module implement the ast_transformer behavior
      case is_transformer(ModName) of
	true ->
	  {ModName, proplists:get_value(debug, ModOpts, false)};
	false ->
	  exit("Module '" ++ atom_to_list(ModName) ++ "' doesn't implement ast_transformer behavior")
      end
  end.

%% Does the transformer module implement the ast_transformer behavior (required)
is_transformer(ModName) ->
  ModInfo = ModName:module_info(),
  lists:member(ast_transformer, proplists:get_value(behavior, proplists:get_value(attributes, ModInfo))).

%% Debug dump before/after snapshots of ASTs
print_debug_trace(AST, NewAST) ->
  io:format("----------~nStarting AST:~n~p~nFinal AST:~n~p~n----------~n", [AST, NewAST]).
