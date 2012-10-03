;; Setup for elpa maintained packages

(persp-mode)
(browse-kill-ring-default-keybindings) 	; use M-y to browse kill ring
(require 'autopair)

;; AC
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;; make auto-complete show immediately
(setq ac-auto-show-menu 0.)
