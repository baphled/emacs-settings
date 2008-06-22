-module(bad_module).

-include_lib("frabjous/include/frabjous.hrl").

-xform_mod([{module, bad_transformer}, {debug, true}]).
