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
-module(ast_walker).

-export([walk/3, walk/4]).

walk(Transformer, AST, Opts) ->
  walk(Transformer, AST, Opts, []).

walk(Transformer, [H|T], Opts, Accum) ->
  case parse_node(H, Transformer, Opts) of
    delete ->
      walk(Transformer, T, Opts, Accum);
    parse_interior ->
      walk(T, Opts, parse_interior(H, Transformer, Opts, Accum));
    NewNode ->
      walk(Transformer, T, Opts, [NewNode|Accum])
  end;
walk(_Transformer, [], _Opts, Accum) ->
  lists:reverse(Accum).

parse_interior({function, _LineNo, _FunName, _FunArity, FunBody}, Transformer, Opts, Accum) ->
  [walk(Transformer, FunBody, Opts, [])|Accum];
parse_interior(_, Transformer, Opts, Accum) ->
  Accum.

%% Apply a transformer to an AST node
parse_node(Node, Transformer, Opts) ->
  S = get(xform_state),
  {N1, S1} = Transformer:transform(Node, Opts, S),
  put(xform_state, S1),
  N1.
