-module(test_component).

-compile([export_all]).

index() ->
  {ok, {output, "text/html", <<"<html><body>Index page</body></html>">>}}.

foo(Value) ->
  {ok, {output, "text/html", list_to_binary(lists:flatten(["<html><body>The value is ", Value, ".</body></html>"]))}}.

bar(String, Number) when is_list(String),
			 is_number(Number) ->
  {ok, {redirect, lists:flatten(["/foo/", String])}}.

baz(Path, QueryString, Headers, Data) ->
  {ok, {forward, "/foo/quux", <<"<html><body>baz put this here<p />">>}}.

quux(Path, QueryString, Headers, Data) ->
  {ok, {output, "text/html", <<"quux put this here<p /></body></html>">>}}.
