-module(herml_tags).

-author("kevin@hypotheticalabs.com").

-include("tree.hrl").

%% !
-define(FUNCALL_TAG, 33).
%% #
-define(DIV_ID_TAG, 35).
%% %
-define(GENERIC_TAG, 37).
%% .
-define(DIV_CLASS_TAG, 46).
%% @
-define(VAR_TAG, 64).

-export([analyze/1]).

-spec(analyze/1 :: (Line :: string()) -> any()).
analyze(L) ->
  Line = string:strip(L),
  convert(erlang:hd(Line), Line, L).

convert(?DIV_ID_TAG, Line, _) ->
  #herml_node{type=div_id_, attrs=parse_div(Line)};
convert(?DIV_CLASS_TAG, Line, _) ->
  #herml_node{type=div_class, attrs=parse_div(Line)};
convert(_, _, L) ->
  #herml_node{type=text_, attrs=[{body, L}]}.

parse_div(_Line) ->
  [].
