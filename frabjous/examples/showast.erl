-module(showast).

-behavior(ast_transformer).

-export([transform/3, start/0, is_complete/1]).

-define(EXPORT_STMT, "-export([~p/~p]).").
-define(GEN_SERVER_CALL, "~p(~s) -> gen_server:call(~p, {~p, ~s}).").

-record(state,
	{fun_list,
	export}).

start() ->
  #state{fun_list=[],
	 export=false}.

is_complete(State) ->
  HasFuns = has_funs(State),
  #state{export=HasExport} = State,
  if
    HasFuns andalso HasExport ->
      true;
    true ->
      false
  end.

transform({attribute, LineNo, module, ModName}, Opts, State) ->
  #state{fun_list = FunList} = State,
  HasFuns = has_funs(State),
  if
    HasFuns ->
     {State#state{export=true}, lists:flatten([{attribute, LineNo, module, ModName}, 
					       frabjous:build_ast(gen_export_stmts(State))])};
    true ->
      {State, {attribute, LineNo, module, ModName}}
  end;

transform({attribute, _LineNo, remote_call, Options}, Opts, State) ->
  Server = proplists:get_value(server, Options, undef),
  Name = proplists:get_value(name, Options, undef),
  Vars = proplists:get_value(vars, Options, []),
  ArgList = build_arg_list(Vars, ""),
  {add_fun(State, Name, length(Vars)),
	   frabjous:build_ast(build_raw_code(Name, Server, ArgList))};

transform(Node, Opts, State) ->
  io:format("Node: ~p~n", [Node]),
  {State, Node}.

build_arg_list([H|T], ArgList) ->
  if
    length(T) > 0 ->
      build_arg_list(T, string:concat(string:concat(ArgList, frabjous:atom_to_var(H)), ","));
    true ->
      build_arg_list(T, string:concat(ArgList, frabjous:atom_to_var(H)))
  end;
build_arg_list([], ArgList) ->
  ArgList.
build_raw_code(Name, Server, ArgList) ->
  Code = lists:flatten(io_lib:format(?GEN_SERVER_CALL, [Name,
							ArgList, Server, Name,
							ArgList])),
  io:format("Code: ~p~n", [Code]),
  Code.

has_funs(State) ->
  #state{fun_list = FunList} = State,
  length(FunList) > 0.

gen_export_stmts(State) ->
  #state{fun_list=FunList} = State,
  gen_export_stmts(FunList, []).

gen_export_stmts([{FunName, Arity}|T], Accum) ->
  gen_export_stmts(T, lists:append(Accum, [io_lib:format(?EXPORT_STMT, [FunName, Arity])]));
gen_export_stmts([], Accum) ->
  Accum.

add_fun(State, Name, Arity) ->
  #state{fun_list=FunList} = State,
  FunList2 = lists:append(FunList, [{Name, Arity}]),
  #state{fun_list=FunList2}.
