-module(single_pass_test).

-compile([{parse_transform, frabjous}]).
-xform_mod([{module, single_pass}, {debug, false}]).
