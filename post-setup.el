;; Clojure
(add-hook 'clojure-mode-hook 'paredit-mode)

(add-hook 'slime-repl-mode-hook
          (defun clojure-mode-slime-font-lock ()
            (require 'clojure-mode)
            (let (font-lock-mode)
              (clojure-mode-font-lock-setup))))

(add-hook 'slime-repl-mode-hook 'paredit-mode)

;; Haskell
(require 'autopair)			; this isn't setup in package yet ;-(

(defun haskell-hooks ()
  (autopair-mode)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent)
)
(add-hook 'haskell-mode-hook 'haskell-hooks)

(browse-kill-ring-default-keybindings) 	; use M-y to browse kill ring


