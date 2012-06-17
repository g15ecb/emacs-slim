;; Setup the mode hooks and any other odd bits

;; perspectives
(persp-mode)
;; some default perspectives...
(persp-switch "hs")
(persp-switch "clj")
(persp-switch "main")
(persp-rename "wk")

;; Clojure
(defun clojure-hooks()
  (paredit-mode)
  (highlight-sexp-mode)
  (rainbow-delimiters-mode))

(add-hook 'clojure-mode-hook 'clojure-hooks)

(add-hook 'slime-repl-mode-hook
          (defun clojure-mode-slime-font-lock ()
            (require 'clojure-mode)
            (let (font-lock-mode)
              (clojure-mode-font-lock-setup))))

(add-hook 'slime-repl-mode-hook 'clojure-hooks)
(add-hook 'inferior-lisp-mode-hook 'clojure-hooks)

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
