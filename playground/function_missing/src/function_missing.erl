-module(function_missing).

-export([undefined_function/3, undefined_lambda/3]).

-author("kevin@hypotheticalabs.com").

undefined_function(Module, Function, Args) ->
  code:soft_purge(Module),
  code:purge(Module),
  code:ensure_loaded(Module),
  case erlang:function_exported(Module, undefined_function, 2) of
    false ->
      invoke_default_handler(Module, Function, Args);
    true ->
      case Module:undefined_function(Function, Args) of
	{false, _} ->
	  invoke_default_handler(Module, Function, Args);
	{true, Result} ->
	  Result
      end
  end.

undefined_lambda(Module, Fun, Args) ->
  error_handler:undefined_lambda(Module, Fun, Args).

%% Internal functions
invoke_default_handler(Mod, Fun, Args) ->
  try
    process_flag(error_handler, error_handler),
    error_handler:undefined_function(Mod, Fun, Args)
  catch
    Error ->
      throw(Error)
  after
    process_flag(error_handler, ?MODULE)
  end.
