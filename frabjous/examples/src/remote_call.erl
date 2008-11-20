-module(remote_call).

-behavior(ast_transformer).

-export([transform/3, start/1, is_complete/1]).

-record(state,
	{inserted_exports,
	 pass,
	 exports}).

transform(Node, _Opts, State) ->
  io:format("Node: ~p~n", [Node]),
  {Node, State}.


%% If State == nil then this is the first pass
start(State) ->
  case State of
    nil ->
      #state{exports=[], pass=1, inserted_exports=false};
    _ ->
      State#state{pass=State#state.pass + 1}
  end.

is_complete(State) ->
  length(State#state.exports) == 0.

%% Private functions
process_attribute(CallType, Options, State) ->
  Server = atom_to_list(proplists:get_value(server, Options)),
  FunName = atom_to_list(proplists:get_value(name, Options)),
  Args = proplists:get_value(vars, Options, []),
  case Server =:= "undefined" orelse FunName =:= "undefined" of
    true ->
      exit("Remote call " ++ FunName ++ " invalid");
    false ->
      Export = string:join([FunName, "/", integer_to_list(length(Args))], ""),
      Fun = generate_function(CallType, FunName, Args, Server),
      {frabjous_helper:string_to_ast(Fun), State#state{exports=lists:append(State#state.exports, [Export])}}
  end.

generate_function(CallType, FunName, Args, Server) ->
  ArgList = build_arg_list(Args),
  string:join([FunName, "(", ArgList, ") -> gen_server:",
	       atom_to_list(CallType), "(", Server,
	       ",{", FunName, ",", ArgList, "})."], "").

build_arg_list(Args) ->
  case build_arg_list(Args, []) of
    [] ->
      [""];
    ArgList ->
      string:join(ArgList, ",")
  end.

build_arg_list([H|T], Accum) ->
  build_arg_list(T, lists:append(Accum, [[frabjous_helper:atom_to_var(H)]]));
build_arg_list([], Accum) ->
  Accum.
