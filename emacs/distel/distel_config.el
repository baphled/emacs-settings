(provide 'distel_config)
(require 'distel)
(distel-setup)

(setq erlang-refactor-status 0)

(defun toggle-erlang-refactor ()
  (interactive)
  (cond ((= erlang-refactor-status 0)
	 (call-interactively 'erlang-refactor-on)
	 (setq erlang-refactor-status 1))
	((= erlang-refactor-status 1)
	 (call-interactively 'erlang-refactor-off)
	 (setq erlang-refactor-status 0))))

(global-set-key (kbd "C-c C-r") 'toggle-erlang-refactor)
