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
			   :weight normal :height 135 :width normal
			   :family "apple-monaco")))))
  (set-colors "Black" "White"))

;; Kills all them buffers except scratch
;; obTained From http://www.chrislott.org/geek/emacs/dotemacs.html
(defun nuke-all-buffers ()
  "kill all buffers, leaving *scratch* only"
  (interactive)
  (mapcar (lambda (x) (kill-buffer x))
	  (buffer-list))
  (delete-other-windows))

(global-set-key (kbd "M-w") 'toggle-colors-white)
(global-set-key (kbd "M-b") 'toggle-colors-black)
(define-key osx-key-mode-map [home] 'beginning-of-line)
(define-key osx-key-mode-map [end] 'end-of-line)
(define-key osx-key-mode-map [f3] 'nuke-all-buffers)
(define-key osx-key-mode-map [f5] 'rgrep)
