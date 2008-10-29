(provide 'erlang_mode_config)

(add-to-list 'load-path "/opt/lib/erlang/lib/tools-2.6.1/emacs")
(require 'erlang-start)
(add-hook 'erlang-mode-hook
	  (lambda()
	    (setq indent-tabs-mode nil)
	    (setq erlang-indent-level 2)
	    (setq erlang-tab-always-indent nil)
	    (add-to-list 'exec-path "/opt/bin")
	    (setq erlang-root-dir "/opt")))