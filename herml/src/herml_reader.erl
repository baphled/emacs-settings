-module(herml_reader).

-include("tree.hrl").

-export([file/1]).

-define(EOLS, [$\r, $\n]).
-define(TAG_START, [$\%, $., $#]).
-define(INDENT, "  ").

-spec(file/1 :: (Filename :: string()) -> {'ok', [any()]} | {'error', string() | atom()}).
file(Filename) when is_list(Filename) ->
  case file:read_file(Filename) of
    {ok, Contents} ->
      parse_nodes(Contents);
    Error ->
      Error
  end.

parse_nodes(Contents) ->
  Lines = string:tokens(binary_to_list(Contents), ?EOLS),
  build_raw_tree(Lines).
  %%transform(Tree).

build_raw_tree(Lines) ->
  build_raw_tree(Lines, 1, dict:new()).

build_raw_tree([H|T], LineCounter, State) ->
  case length(string:strip(H)) of
    0 ->
      build_raw_tree(T, LineCounter + 1, State);
    _ ->
      IndentLevel = find_indent_level(H),
      NewState = case dict:find(IndentLevel, State) of
		   {ok, Nodes} ->
		     dict:store(IndentLevel, [#node{data=H, line=LineCounter, parent=find_latest_parent(IndentLevel - 1, State)}|Nodes], State);
		   error ->
		     dict:store(IndentLevel, [#node{data=H, line=LineCounter, parent=find_latest_parent(IndentLevel - 1, State)}], State)
		 end,
      build_raw_tree(T, LineCounter + 1, NewState)
  end;
build_raw_tree([], _, State) ->
  T = dict:fold(fun(Key, Value, Acc) ->
		    [{Key, lists:reverse(Value)}|Acc] end,
		[], State),
  reparent(lists:reverse(lists:keysort(1, T))).

reparent(Nodes) when length(Nodes) == 1 ->
  [{_, NodeList}] = Nodes,
  NodeList;
reparent(Nodes) ->
  [{_, Current}|T] = Nodes,
  [{PI, Parents}|PT] = T,
  reparent([{PI, reparent_nodes(Current, Parents)}|PT]).

reparent_nodes([H|T], Parents) ->
  reparent_nodes(T, reparent_node(H, Parents));
reparent_nodes([], Parents) ->
  lists:reverse(Parents).

reparent_node(Node, Parents) ->
  lists:foldl(fun(Parent, Acc) ->
		  case Parent#node.line == Node#node.parent of
		    true ->
		      [Parent#node{children=lists:append(Parent#node.children, [Node])}|Acc];
		    false ->
		      [Parent|Acc]
		  end end, [], Parents).

find_indent_level(Line) ->
  Count = string:span(Line, " "),
  case Count rem 2 == 0 of
    true ->
      Count div 2;
    false ->
      throw({bad_indent, Line})
  end.

find_latest_parent(PreviousIndent, _State) when PreviousIndent < 0 ->
  0;

find_latest_parent(PreviousIndent, State) ->
  {ok, Parents} = dict:find(PreviousIndent, State),
  lists:foldr(fun(Parent, LineNo) ->
		  case Parent#node.line > LineNo of
		    true ->
		      Parent#node.line;
		    false ->
		      LineNo
		  end end, 0, Parents).
