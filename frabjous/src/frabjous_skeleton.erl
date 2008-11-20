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
-module(frabjous_skeleton).

-export([parse_transform/2]).

-define(PASS_COUNT_THRESHOLD, 5).

-record(start,
	{event,
	 lineno}).

%%---------------------------------------------------
%% parse_transform(AST, Opts) -> AST
%%
%%  AST -> list(terms)
%%  Opts -> proplist()
%%---------------------------------------------------
parse_transform(AST, Opts) ->
  parse_transform(AST, Opts, []).

parse_transform([{attribute, _LineNo, AttrType, AttrOpts}=H|T], Opts, Accum) ->
  io:format("Got an attribute~n"),
  parse_transform(T, Opts, lists:append(Accum, [H]));
parse_transform([{function, _LineNo, FunName, FunArity, FunBody}=H|T], Opts, Accum) ->
  io:format("Got a function~n"),
  io:format("FunBody: ~p~n", [FunBody]),
  parse_transform(FunBody, Opts),
  parse_transform(T, Opts, lists:append(Accum, [H]));
parse_transform([{clause, _LineNo, ClauseBody}=H|T], Opts, Accum) ->
  io:format("Got a clause~n"),
  parse_transform(ClauseBody, Opts, []),
  parse_transform(T, Opts, lists:append(Accum, [H]));
parse_transform([{clause, _LineNo, _, _, ClauseBody}=H|T], Opts, Accum) ->
  io:format("Got a clause~n"),
  parse_transform(ClauseBody, Opts, []),
  parse_transform(T, Opts, lists:append(Accum, [H]));

parse_transform([H|T], Opts, Accum) ->
  io:format("Skipping ~p~n", [H]),
  parse_transform(T, Opts, lists:append(Accum, [H]));
parse_transform([], Opts, Accum) ->
  Accum.
