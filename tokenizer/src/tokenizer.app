{application, tokenizer,
 [{description, "tokenizer"},
  {vsn, "0.18"},
  {modules, [
    tokenizer,
    tokenizer_app,
    tokenizer_sup,
    tokenizer_deps,
    tokenizer_token_resource
  ]},
  {registered, [tokenizer_store]},
  {mod, {tokenizer_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto, mnesia]}]}.
