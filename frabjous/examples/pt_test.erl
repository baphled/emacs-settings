-module(pt_test).

-compile([{parse_transform, frabjous}]).
-xform_mod([{module, showast}, {function, transform}, {debug, true}]).

-define(SERVER, ?MODULE).

-remote_call([{server, ?SERVER}, {name, foo}, {vars, [x,y]}]).
