-module(herml_render).

-include("tree.hrl").

-author("kevin@hypotheticalabs.com").

-export([render/1]).

render(Tree) ->
  render(Tree, []).

%% Internal functions
render([H|T], Acc) ->
  Acc1 = case H#herml_node.type of
           tag ->
             render_tag(H, Acc);
           text ->
             render_text(H, Acc)
         end,
  render(T, Acc1);
render([], Acc) ->
  lists:flatten(Acc).

render_text(H, Acc) ->
  Acc ++ proplists:get_value(body, H#herml_node.attrs).

render_tag(H, Acc) ->
  if
    length(H#herml_node.children) == 0 ->
      Acc ++ build_tag_begin(H) ++ "/>";
    true ->
      Acc1 = Acc ++ build_tag_begin(H) ++ ">",
      Acc2 = render(H#herml_node.children, Acc1),
      Acc2 ++ build_end_tag(H)
  end.

build_tag_begin(H) ->
  #herml_node{attrs=Attrs} = H,
  Buf = "<" ++ proplists:get_value(tag_name, Attrs),
  if
    length(Attrs) > 1 ->
      lists:foldl(fun({Name, Value}, Acc) ->
                      case Name of
                        tag_name ->
                          Acc;
                        _ ->
                          Acc ++ " " ++ atom_to_list(Name) ++ "=\"" ++ Value ++ "\""
                      end end, Buf, Attrs);
    true ->
      Buf
  end.

build_end_tag(H) ->
  "</" ++ proplists:get_value(tag_name, H#herml_node.attrs) ++ ">".
