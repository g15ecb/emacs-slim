;; Setup the mode hooks and any other odd bits

;; perspectives
(persp-mode)

;; AC
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; make auto-complete show immediately
(setq ac-auto-show-menu 0.)

;; ;; Haskell
;; (defun haskell-hooks ()
;;   (turn-on-haskell-doc-mode)
;;   (turn-on-haskell-indent)
;;   (rainbow-delimiters-mode)
;;   (show-paren-mode)
;;   (autopair-mode))

;; (add-hook 'haskell-mode-hook 'haskell-hooks)

;; Scala
(defun default-hooks() 
  (autopair-mode)
  (show-paren-mode)
  (rainbow-delimiters-mode))

(add-hook 'scala-mode-hook 'default-hooks)
(add-hook 'scala-mode-inf-hook 'default-hooks)
(add-hook 'java-mode-hook 'default-hooks)
(add-hook 'eshell-mode-hook 'default-hooks)

;; Browse kill ring
(browse-kill-ring-default-keybindings) 	; use M-y to browse kill ring

(require 'autopair)

(require 'javadoc-help)
