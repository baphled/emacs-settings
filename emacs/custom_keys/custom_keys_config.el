(provide 'custom_keys_config)



(defun set-colors(backgrnd foregrnd)
  (set-background-color backgrnd)
  (set-foreground-color foregrnd)
  (set-cursor-color foregrnd))

(defun toggle-colors-white ()
  (interactive)
  (custom-set-faces
   '(default ((t (:stipple nil :background "White" :foreground "Black"
			   :inverse-video nil :box nil :strike-through nil
			   :overline nil :underline nil :slant normal
			   :weight normal :height 120 :width normal
			   :family "apple-monaco")))))
  (set-colors "White" "Black"))

(defun toggle-colors-black ()
  (interactive)
  (custom-set-faces
   '(default ((t (:stipple nil :background "Black" :foreground "White"
			   :inverse-video nil :box nil :strike-through nil
			   :overline nil :underline nil :slant normal
			   :weight normal :height 120 :width normal
			   :family "apple-monaco")))))
  (set-colors "Black" "White"))

;; Switches buffers between two Emacs windows
;; Obtained from http://www.emacswiki.org/cgi-bin/wiki?SwitchingBuffers#toc5
(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;; Kills all them buffers except scratch
;; Obtained From http://www.chrislott.org/geek/emacs/dotemacs.html
(defun nuke-all-buffers ()
  "kill all buffers, leaving *scratch* only"
  (interactive)
  (mapcar (lambda (x) (kill-buffer x))
	  (buffer-list))
  (delete-other-windows))

(global-set-key (kbd "M-w") 'toggle-colors-white)
(global-set-key (kbd "M-b") 'toggle-colors-black)
(global-set-key (kbd "C-x 4 t") 'transpose-buffers)
(define-key osx-key-mode-map [home] 'beginning-of-line)
(define-key osx-key-mode-map [end] 'end-of-line)
(define-key osx-key-mode-map [f3] 'nuke-all-buffers)
(define-key osx-key-mode-map [f5] 'rgrep)
