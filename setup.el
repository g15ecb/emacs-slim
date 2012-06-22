;; Setup the mode hooks and any other odd bits

;; perspectives
(persp-mode)
;; some default perspectives...
(persp-switch "hs")
(persp-switch "clj")
(persp-switch "sch")
(persp-switch "main")
(persp-rename "wrk")

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
  (paredit-mode))

(add-hook 'haskell-mode-hook 'haskell-hooks)

;; Browse kill ring
(browse-kill-ring-default-keybindings) 	; use M-y to browse kill ring

;; scheme stuff
(require 'quack)
(setq scheme-program-name "petite")

(defun scheme-hooks ()
  (paredit-mode)
  (rainbow-delimiters-mode))

(add-hook 'scheme-mode-hook 'scheme-hooks)

;; this is from http://www.cs.indiana.edu/classes/c311/emacs/dot_emacs_full
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(paren-blinking "t" t)
 '(paren-matching t t) 
 '(quack-default-program "petite")
 '(quack-fontify-style (quote plt))
 '(quack-pltish-fontify-keywords-p t)
 '(quack-pltish-keywords-to-fontify (quote ("and" "begin" "begin0" "c-declare" "c-lambda" "call-with-current-continuation" "call-with-input-file" "call-with-output-file" "call/cc" "case" "case-lambda" "class" "class*" "class*/names" "class100" "class100*" "compound-unit/sig" "cond" "cond-expand" "define" "define-macro" "define-module" "define-public" "define-signature" "define-struct" "define-syntax" "define-syntax-set" "define-values" "define-values/invoke-unit/sig" "define/contract" "define/override" "define/private" "define/public" "delay" "do" "else" "exit-handler" "field" "if" "import" "inherit" "inherit-field" "init" "init-field" "init-rest" "instantiate" "interface" "lambda" "let" "let*" "let*-values" "let+" "let-syntax" "let-values" "let/ec" "letrec" "letrec-values" "letrec-syntax" "match" "match-lambda" "match-lambda*" "match-let" "match-let*" "match-letrec" "match-define" "mixin" "module" "opt-lambda" "or" "override" "override*" "namespace-variable-bind/invoke-unit/sig" "parameterize" "private" "private*" "protect" "provide" "provide-signature-elements" "provide/contract" "public" "public*" "quote" "receive" "rename" "require" "require-for-syntax" "send" "send*" "set!" "set!-values" "signature->symbols" "super-instantiate" "syntax-case" "syntax-case*" "syntax-error" "syntax-rules" "unit/sig" "unless" "when" "with-handlers" "with-method" "with-syntax")))
 '(quack-pretty-lambda-p nil)
 '(quack-programs (quote ("petite" "bigloo" "csi" "gosh" "gsi" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M errortrace" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(quack-remap-find-file-bindings-p nil)
 '(quack-run-scheme-always-prompts-p nil)
 '(quack-switch-to-scheme-method (quote cmuscheme)))
