;; Clojure
(add-hook 'clojure-mode-hook 'paredit-mode)

;; Haskell
(require 'autopair)			; this isn't setup in package yet ;-(
(add-hook 'haskell-mode-hook 'autopair-mode)

(browse-kill-ring-default-keybindings) 	; use M-y to browse kill ring


