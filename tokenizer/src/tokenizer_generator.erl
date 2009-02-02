-module(tokenizer_generator).

-author("kevin@hypotheticalabs.com").

-export([next_token/0, next_token/1]).

next_token() ->
  next_token("").

next_token(String) when length(String) == 0 ->
  [next_char()];
next_token(String) when length(String) == 1 ->
  [C] = String,
  [next_char(C)];
next_token(String) ->
  [H|T] = lists:reverse(String),
  case next_char(H) of
    add_char ->
      String ++ [next_char()];
    Char ->
      lists:reverse(T) ++ [Char]
  end.

%% Internal functions

next_char() ->
  $a.
next_char(C) when C >= $a,
                  C =< $y ->
  C + 1;
next_char(C) when C == $z ->
  $A;
next_char(C) when C >= $A,
                  C =< $Y ->
  C + 1;
next_char(C) when C == $Z ->
  $0;
next_char(C) when C >= $0,
                  C =< $8 ->
  C + 1;
next_char(C) when C == $9 ->
  add_char.
