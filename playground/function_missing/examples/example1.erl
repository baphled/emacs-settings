-module(example1).

-export([init/0, find_by_field/2, undefined_function/2]).

-author("kevin@hypotheticalabs.com").

init() ->
  code:ensure_loaded(function_missing),
  process_flag(error_handler, function_missing),
  ok.

undefined_function(FunName, Args) when is_atom(FunName) ->
  FunNameStr = atom_to_list(FunName),
  case is_finder(atom_to_list(FunName)) of
    false ->
      {false, []};
    true ->
      FieldName = extract_find_field(FunNameStr),
      {true, apply(?MODULE, find_by_field, [FieldName, Args])}
  end.

%% Internal functions
is_finder(Name) ->
  string:str(Name, "find_by_") == 1.

extract_find_field(Name) ->
  Chunks = string:tokens(Name, "_"),
  erlang:hd(lists:reverse(Chunks)).

find_by_field(FieldName, Attrs) ->
  io:format("Finding by ~p using data ~p~n", [FieldName, Attrs]).
