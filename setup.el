;; Setup the mode hooks and any other odd bits

;; perspectives
(persp-mode)
;; some default perspectives...
(persp-switch "haskell")
;; (persp-switch "clj")
(persp-switch "main")
;;(persp-rename "wrk")

;; AC
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; Clojure
(defun clojure-hooks()
  (paredit-mode)
  (show-paren-mode)
  (rainbow-delimiters-mode))

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
(defun haskell-hooks ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent)
  (show-paren-mode)
  (paredit-mode)
  (auto-complete-mode -1))

(add-hook 'haskell-mode-hook 'haskell-hooks)

;; Browse kill ring
(browse-kill-ring-default-keybindings) 	; use M-y to browse kill ring

;; smart mode line
;; Example configs...
;; (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/Projects/In-Development/" ":ProjDev:"))
;; (add-to-list 'sml/replacer-regexp-list '("^~/Documents/Work/" ":Work:))
;; ;; Added in the right order, they even work sequentially:
;; (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:"))
;; (add-to-list 'sml/replacer-regexp-list '("^:DB:Documents" ":DDocs:"))
(require 'smart-mode-line)
(sml/setup)

(add-to-list 'sml/replacer-regexp-list '("^~/phd/" ":PhD"))
(add-to-list 'sml/replacer-regexp-list '("^~/phd/" ":Haskell"))
