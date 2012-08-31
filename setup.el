;; Setup the mode hooks and any other odd bits

;; perspectives
(persp-mode)
;; some default perspectives...
(persp-switch "paper")
(persp-switch "scala")
(persp-switch "main")
(persp-rename "thesis")

;; AC
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; make auto-complete show immediately
(setq ac-auto-show-menu 0.)

;; Haskell
(defun haskell-hooks ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent)
  (rainbow-delimiters-mode)
  (show-paren-mode)
  (autopair-mode))

(add-hook 'haskell-mode-hook 'haskell-hooks)

;; Scala
(defun scala-hooks() 
  (autopair-mode)
  (show-paren-mode)
  (rainbow-delimiters-mode))

(add-hook 'scala-mode-hook 'scala-hooks)
(add-hook 'scala-mode-inf-hook 'scala-hooks)
(add-hook 'java-mode-hook 'scala-hooks)

;; Browse kill ring
(browse-kill-ring-default-keybindings) 	; use M-y to browse kill ring

(require 'autopair)
