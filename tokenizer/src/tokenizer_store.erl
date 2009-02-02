-module(tokenizer_store).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include_lib("stdlib/include/qlc.hrl").

-include("tokenizer_models.hrl").

%% API
-export([start_link/0, associate_token/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

associate_token(URL) ->
  gen_server:call(?SERVER, {associate_token, URL}, 30000).

init([]) ->
  insure_table(url_entry, record_info(fields, url_entry)),
  insure_table(last_token, record_info(fields, last_token)),
  {ok, []}.

handle_call({associate_token, URL}, From, State) ->
  proc_lib:spawn(fun() ->
                     gen_server:reply(From, generate_token(URL)) end),
  {noreply, State, 30000};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, State) ->
  {noreply, State, hibernate};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  mnesia:stop().

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
insure_table(Name, Fields) ->
  try
    mnesia:table_info(Name, cookie),
    ok
  catch
    exit: {aborted, _} ->
      case mnesia:create_table(Name, [{disc_only_copies, [node()]},
                                      {attributes, Fields}]) of
        {atomic, ok} ->
          ok;
        Error ->
          throw({error, {Error, erlang:get_stacktrace()}})
      end
  end.

generate_token(URL) ->
  T = fun() ->
          case find_existing_token(URL) of
            [] ->
              NewToken = next_token(),
              mnesia:write(#url_entry{url=URL, token=NewToken}),
              NewToken;
            [Token] ->
              Token
          end end,
  case mnesia:transaction(T) of
    {atomic, Result} ->
      Result;
    Error ->
      throw({Error, erlang:get_stacktrace()})
  end.

find_existing_token(URL) ->
  qlc:e(qlc:q([E#url_entry.token || E <- mnesia:table(url_entry),
                                    E#url_entry.url =:= URL])).

next_token() ->
  NewToken = case qlc:e(qlc:q([LT || LT <- mnesia:table(last_token),
                                    LT#last_token.id == 0])) of
               []->
                 tokenizer_generator:next_token();
               [Token] ->
                 io:format("Token: ~p~n", [Token]),
                 mnesia:delete_object(Token),
                 tokenizer_generator:next_token(Token#last_token.token)
             end,
  mnesia:write(#last_token{token=NewToken}),
  NewToken.
