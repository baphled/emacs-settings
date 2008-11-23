-module(herml_reader).

-include("tree.hrl").

-export([string/1, file/1]).

-define(EOLS, [$\r, $\n]).
-define(TAG_START, [$\%, $., $#]).
-define(INDENT, "  ").


-spec(file/1 :: (Filename :: string()) -> list() | {'error', string() | atom()}).
file(Filename) when is_list(Filename) ->
  case file:read_file(Filename) of
    {ok, Contents} ->
      herml_reader:string(Contents);
    Error ->
      Error
  end.

-spec(string/1 :: (Template :: binary()) -> list() | {'error', string() | atom()}).
string(Template) when is_binary(Template) ->
  parse_nodes(Template).

%% Internal functions

parse_nodes(Contents) ->
  Lines = string:tokens(binary_to_list(Contents), ?EOLS),
  T = build_raw_tree(Lines),
  T1 = herml_tree_consolidator:consolidate(T),
  transform(T1).

transform(Tree) ->
  transform(Tree, []).

transform([H|T], Acc) when is_tuple(H) ->
  Children = transform(H#node.children),
  H1 = herml_tags:analyze(H#node.data),
  transform(T, [H1#herml_node{children=Children}|Acc]);
transform([H|T], Acc) when length(H) == 0 ->
  transform(T, Acc);
transform([], Acc) ->
  lists:reverse(Acc).

build_raw_tree(Lines) ->
  build_raw_tree(Lines, 1, 0, dict:new()).

build_raw_tree([H|T], LineCounter, PrevIndentLevel, State) ->
  case length(string:strip(H)) of
    0 ->
      build_raw_tree(T, LineCounter + 1, PrevIndentLevel, State);
    _ ->
      IndentLevel = find_indent_level(H, PrevIndentLevel),
      NewState = case dict:find(IndentLevel, State) of
		   {ok, Nodes} ->
		     dict:store(IndentLevel, [#node{data=H, line=LineCounter, parent=find_latest_parent(IndentLevel - 1, State)}|Nodes], State);
		   error ->
		     dict:store(IndentLevel, [#node{data=H, line=LineCounter, parent=find_latest_parent(IndentLevel - 1, State)}], State)
		 end,
      build_raw_tree(T, LineCounter + 1, IndentLevel, NewState)
  end;
build_raw_tree([], _, _, State) ->
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

find_indent_level(Line, PrevIndentLevel) ->
  Count = string:span(Line, " "),
  RawIndentLevel = case Count rem 2 == 0 of
                     true ->
                       Count div 2;
                     false ->
                       throw({bad_indent, Line})
                   end,
  if
    RawIndentLevel == PrevIndentLevel ->
      PrevIndentLevel;
    RawIndentLevel > PrevIndentLevel ->
      PrevIndentLevel + 1;
    RawIndentLevel < PrevIndentLevel ->
      RawIndentLevel
  end.

find_latest_parent(PreviousIndent, _State) when PreviousIndent < 0 ->
  0;

find_latest_parent(PreviousIndent, State) ->
  case dict:find(PreviousIndent, State) of
    {ok, Parents} ->
      lists:foldr(fun(Parent, LineNo) ->
                      case Parent#node.line > LineNo of
                        true ->
                          Parent#node.line;
                        false ->
                          LineNo
                      end end, 0, Parents);
    error ->
      find_latest_parent(PreviousIndent - 1, State)
  end.
