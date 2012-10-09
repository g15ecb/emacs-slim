(defun default-hooks() 
  (autopair-mode)
  (show-paren-mode)
  (rainbow-delimiters-mode))

;; Scala
(add-hook 'scala-mode-hook 'default-hooks)
(add-hook 'scala-mode-inf-hook 'default-hooks)

;; eshell
(add-hook 'eshell-mode-hook 'default-hooks)

;; Haskell
(defun haskell-hooks ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent)
  (rainbow-delimiters-mode)
  (show-paren-mode)
  (autopair-mode))

(add-hook 'haskell-mode-hook 'haskell-hooks)
(add-hook 'inf-haskell 'default-hooks)

;; OCaml
(add-hook 'tuareg-mode-hook 'default-hooks)
(add-hook 'tuareg-interactive-mode-hook 'default-hooks)




