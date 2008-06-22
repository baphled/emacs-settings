-module(missing_module).

-include_lib("frabjous/include/frabjous.hrl").

-xform_mod([{module, foo}, {debug, true}]).
