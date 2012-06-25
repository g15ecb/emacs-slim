(load "~/.emacs.d/vanilla.el")  
(load "~/.emacs.d/elpa.el")  
(load "~/.emacs.d/setup.el")
(load "~/.emacs.d/setup-non-elpa.el")
(load "~/.emacs.d/keybindings.el")
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
 '(quack-global-menu-p nil)
 '(quack-pltish-fontify-keywords-p t)
 '(quack-pltish-keywords-to-fontify (quote ("and" "begin" "begin0" "c-declare" "c-lambda" "call-with-current-continuation" "call-with-input-file" "call-with-output-file" "call/cc" "case" "case-lambda" "class" "class*" "class*/names" "class100" "class100*" "compound-unit/sig" "cond" "cond-expand" "define" "define-macro" "define-module" "define-public" "define-signature" "define-struct" "define-syntax" "define-syntax-set" "define-values" "define-values/invoke-unit/sig" "define/contract" "define/override" "define/private" "define/public" "delay" "do" "else" "exit-handler" "field" "if" "import" "inherit" "inherit-field" "init" "init-field" "init-rest" "instantiate" "interface" "lambda" "let" "let*" "let*-values" "let+" "let-syntax" "let-values" "let/ec" "letrec" "letrec-values" "letrec-syntax" "match" "match-lambda" "match-lambda*" "match-let" "match-let*" "match-letrec" "match-define" "mixin" "module" "opt-lambda" "or" "override" "override*" "namespace-variable-bind/invoke-unit/sig" "parameterize" "private" "private*" "protect" "provide" "provide-signature-elements" "provide/contract" "public" "public*" "quote" "receive" "rename" "require" "require-for-syntax" "send" "send*" "set!" "set!-values" "signature->symbols" "super-instantiate" "syntax-case" "syntax-case*" "syntax-error" "syntax-rules" "unit/sig" "unless" "when" "with-handlers" "with-method" "with-syntax")))
 '(quack-pretty-lambda-p nil)
 '(quack-programs (quote ("petite" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(quack-remap-find-file-bindings-p nil)
 '(quack-run-scheme-always-prompts-p nil)
 '(quack-switch-to-scheme-method (quote cmuscheme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
