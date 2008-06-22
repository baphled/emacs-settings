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
-module(test_frabjous).

-include_lib("eunit/include/eunit.hrl").

missing_module_test() ->
  case compile:file("priv/test_data/missing_module.erl") of
    {ok, _} ->
      exit("Compile error expected");
    _Error ->
      ok
  end.

bad_module_test() ->
  code:add_pathz("/tmp"),
  compile:file("priv/test_data/bad_transformer.erl", [{outdir, "/tmp"}]),
  case compile:file("priv/test_data/bad_module.erl") of
    {ok, _} ->
      exit("Compile error expected");
    _Error ->
      ok
  end.

single_pass_test() ->
  code:add_pathz("/tmp"),
  compile:file("priv/test_data/single_pass.erl", [{outdir, "/tmp"}]),
  {ok, _} = compile:file("priv/test_data/single_pass_test.erl").

multi_pass_test() ->
  code:add_pathz("/tmp"),
  compile:file("priv/test_data/multi_pass.erl", [{outdir, "/tmp"}]),
  {ok, _} = compile:file("priv/test_data/multi_pass_test.erl").
