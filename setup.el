;; Setup the mode hooks and any other odd bits

;; perspectives
(persp-mode)

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
