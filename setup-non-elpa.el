;; AucTeX
;; download auctex; untar; mv to non-elpa/auctex
;; ./configure --with-texmf-dir=/usr/local/texlive/texmf-local
;; make
(add-to-list 'load-path "~/.emacs.d/non-elpa/auctex")
(add-to-list 'load-path "~/.emacs.d/non-elpa/auctex/preview")
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq TeX-auto-save t)                  
(setq TeX-parse-self t)
(setq-default TeX-master nil)           ;set up AUCTeX to deal with
                                        ;multiple file documents.
(setq reftex-plug-into-AUCTeX t)

(setq reftex-label-alist
   '(("axiom"   ?a "ax:"  "~\\ref{%s}" nil ("axiom"   "ax.") -2)
     ("theorem" ?h "thr:" "~\\ref{%s}" t   ("theorem" "th.") -3)))

(setq reftex-cite-format 'natbib)

(add-hook 'LaTeX-mode-hook 'reftex-mode)

;; ghc-mod
;; (add-to-list 'load-path "~/.emacs.d/non-elpa/ghc-mod")
;; (autoload 'ghc-init "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; Haskell

(add-to-list 'load-path "~/.emacs.d/non-elpa/ac-haskell")
(require 'auto-complete-haskell)

;; Erlang
(add-to-list 'load-path "~/.emacs.d/non-elpa/distel/elisp")
(require 'distel)
(distel-setup)

;; Scala
;; load the ensime lisp code...
(add-to-list 'load-path "~/.emacs.d/non-elpa/ensime/elisp/")
(require 'ensime)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
