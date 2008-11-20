(provide 'erlang_mode_config)

(add-to-list 'load-path "/opt/lib/erlang/lib/tools-2.6.2/emacs")
(require 'erlang-start)
(setq erlang-indent-level 2)
(add-to-list 'exec-path "/opt/bin")
(setq erlang-root-dir "/opt")

(setq erlang-skel-mail-address "kevin@hypotheticalabs.com")