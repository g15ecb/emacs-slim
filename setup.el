;; Setup the mode hooks and any other odd bits

;; Haskell
(require 'autopair)			; this isn't setup in elpa package yet ;-(

(defun haskell-hooks ()
  (autopair-mode)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent)
  (highlight-parentheses-mode)
  (rainbow-delimiters-mode))

(add-hook 'haskell-mode-hook 'haskell-hooks)
(add-hook 'inferior-haskell-hook 'haskell-hooks)

;; Browse kill ring
(browse-kill-ring-default-keybindings) 	; use M-y to browse kill ring
