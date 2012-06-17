;; Setup the mode hooks and any other odd bits

;; perspectives
(persp-mode)
;; some default perspectives...
(persp-switch "hs")
(persp-switch "clj")
(persp-switch "main")
(persp-rename "wk")

;; AC
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; Clojure
(defun clojure-hooks()
  (paredit-mode))

(add-hook 'clojure-mode-hook 'clojure-hooks)

(add-hook 'slime-repl-mode-hook
          (defun clojure-mode-slime-font-lock ()
            (require 'clojure-mode)
            (let (font-lock-mode)
              (clojure-mode-font-lock-setup))))

(add-hook 'slime-repl-mode-hook 'clojure-hooks)
(add-hook 'inferior-lisp-mode-hook 'clojure-hooks)

; ac-slime
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;; Haskell
(require 'autopair)			; this isn't setup in elpa package yet ;-(

(defun haskell-hooks ()
  (autopair-mode)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent)
  (highlight-parentheses-mode)
  (idle-highlight))

(add-hook 'haskell-mode-hook 'haskell-hooks)

;; Browse kill ring
(browse-kill-ring-default-keybindings) 	; use M-y to browse kill ring
