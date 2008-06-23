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
-module(frabjous_helper).

-export([capitalize_string/1, string_to_ast/1, atom_to_var/1]).

string_to_ast(Code) when is_list(Code) ->
  ScanRes = erl_scan:string(Code),
  {ok, F, _} = ScanRes,
  ParseRes = erl_parse:parse_form(F),
  {ok, Form} = ParseRes,
  Form.

atom_to_var(AtomVar) when is_atom(AtomVar) ->
  capitalize_string(atom_to_list(AtomVar)).

capitalize_string([H|T]) when length(T) > 0 ->
  string:join([string:to_upper(H), T], "");
capitalize_string([H|T]) when length(T) == 0->
  string:to_upper(H).
