-module(multi_pass_test).

-compile([{parse_transform, frabjous}]).
-xform_mod([{module, multi_pass}, {debug, false}]).
