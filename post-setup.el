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

(add-hook 'slime-repl-mode-hook 'paredit-mode)

;; Haskell
(require 'autopair)			; this isn't setup in elpa package yet ;-(

(defun haskell-hooks ()
  (autopair-mode)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent)
  (highlight-parentheses-mode)
  (rainbow-delimiters-mode))

(add-hook 'haskell-mode-hook 'haskell-hooks)

;; Browse kill ring
(browse-kill-ring-default-keybindings) 	; use M-y to browse kill ring

;; Custom variables
(custom-set-variables
 '(hl-sexp-background-color "#efefef"))
