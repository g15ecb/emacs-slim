;; Granville Barnett's Emacs Config
;; granvillebarnett@gmail.com

;; Prerequisites:
;; - OCaml (utop + merlin)
;; - Clang Async (https://github.com/Golevka/emacs-clang-complete-async)

;; Note: not all packages can be found in elpa, e.g. AucTeX and Prolog (from
;; Bruda). These packages live in no-elpa. Also, for OCaml. opam installs the
;; relevant Emacs packages.

;; Structure
;; - GUI + Basics 
;; - Elpa 
;; - Package Configuration
;; - Hooks
;; - Global Keybindings

;; *****************************************************************************
;; GUI + Basics
;; *****************************************************************************
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)            
(setq inhibit-startup-message t inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)                                   
(line-number-mode t)                     
(column-number-mode t)                   
(size-indication-mode t)                 
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq default-major-mode 'text-mode)
(global-font-lock-mode t)
(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have
(setq ido-enable-last-directory-history nil) ; forget latest selected directory

;; dump all backup files in specific location
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let (backup-root bpath)
    (setq backup-root "~/.emacs.d/emacs-backup")
    (setq bpath (concat backup-root fpath "~"))
    (make-directory (file-name-directory bpath) bpath)
    bpath
  )
)
(setq make-backup-file-name-function 'my-backup-file-name)

(require 'uniquify)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; *****************************************************************************
;; Elpa 
;; *****************************************************************************
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(magit 
                      autopair
		      perspective
		      rainbow-delimiters
		      rust-mode
		      yasnippet-bundle
		      auto-complete-clang-async
		      d-mode
		      haskell-mode
		      google-this
		      helm
		      helm-gtags
                      auto-complete
		      solarized-theme
                      tangotango-theme
		      nimrod-mode
		      sml-mode
		      tuareg
		      evil)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; *****************************************************************************
;; Package Configuration 
;; *****************************************************************************
(require 'solarized-dark-theme)

(evil-mode)
(helm-mode 1)

(persp-mode)
(persp-rename "1")
(persp-switch "2")
(persp-switch "3")
(persp-switch "4")
(persp-switch "1")

;; autocomplete 
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(setq ac-auto-show-menu 0.)		; show immediately

;; AucTeX: not elpa. 
(add-to-list 'load-path "~/.emacs.d/no-elpa/auctex")
(add-to-list 'load-path "~/.emacs.d/no-elpa/auctex/preview")
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

;; Prolog: not elpa.
;; http://bruda.ca/emacs/prolog_mode_for_emacs
(add-to-list 'load-path "~/.emacs.d/no-elpa/prolog-bruda")
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
			      auto-mode-alist))

;; OCaml: not elpa. Install via opam: merlin, ocp-indent.
;`(add-to-list 'load-path "~/.opam/4.00.1/share/emacs/site-lisp/")
;`(require 'merlin)
;`(setq merlin-use-auto-complete-mode t)
;`(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
;`(load-file "~/.opam/4.00.1/share/typerex/ocp-indent/ocp-indent.el")

;; clang-format
;; (load "~/.emacs.d/no-elpa/clang-format/clang-format.el")
;; (defun clang-format-before-save ()
;;   (interactive)
;;   (when (or (eq major-mode 'c++-mode) (eq major-mode 'c-mode)) (clang-format-buffer)))

;; (add-hook 'before-save-hook 'clang-format-before-save)
;; *****************************************************************************
;; Hooks 
;; *****************************************************************************
(defun common-hooks() 
  (autopair-mode)
  (show-paren-mode)
  (rainbow-delimiters-mode))

;; Nimrod
(require 'nimrod-mode)
(add-hook 'nimrod-mode-hook 'common-hooks)

;; SML
;; (add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\)\\'" . sml-mode))
(defun sml-hooks()
  (local-set-key (kbd "M-e") 'sml-send-buffer))

(add-hook 'sml-mode-hook 'common-hooks)
(add-hook 'inferior-sml-mode-hook 'common-hooks)

;; Ocaml
;; (defun ocaml-hooks()
;;   (local-set-key (kbd "M-e") 'tuareg-eval-buffer)
;;   (local-set-key (kbd "M-/") 'utop-edit-complete))

;; (add-hook 'tuareg-mode-hook 'common-hooks)
;; (add-hook 'tuareg-mode-hook 'ocaml-hooks)
;; (add-hook 'tuareg-mode-hook 'merlin-mode)
;; (add-hook 'tuareg-mode-hook 'common-hooks)
;; (add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
;; (add-hook 'typerex-mode-hook 'utop-setup-ocaml-buffer)

(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)
(add-hook 'haskell-mode-hook 'common-hooks)
(defun haskell-hooks()
  (local-set-key (kbd "M-e") 'inferior-haskell-load-file))
(add-hook 'haskell-mode-hook 'haskell-hooks)

;; C 
(setq c-default-style "linux" c-basic-offset 4)

(defun c-hooks()
  (helm-gtags-mode)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (c-set-offset 'arglist-intro '+)	; aligns args split across lines
)

(add-hook 'c-mode-common-hook 'common-hooks)
(add-hook 'c-mode-common-hook 'c-hooks)

;; clang async stuff
;; https://github.com/Golevka/emacs-clang-complete-async
;; (require 'auto-complete-clang-async)

;; (defun ac-cc-mode-setup ()
;;   (setq ac-clang-complete-executable "~/.emacs.d/clang-complete")
;;   (setq ac-sources '(ac-source-clang-async))
;;   (ac-clang-launch-completion-process)
;; )

;; (defun my-ac-config ()
;;   (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;;   (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;   (global-auto-complete-mode t))

;; (my-ac-config)

;; Prolog
(defun prolog-hooks()
  (local-set-key (kbd "M-e") 'prolog-consult-buffer))

(add-hook 'prolog-mode-hook 'common-hooks)
(add-hook 'prolog-mode-hook 'prolog-hooks)
(add-hook 'prolog-inferior-mode-hook' common-hooks)

;; *****************************************************************************
;; Global Keybindings 
;; *****************************************************************************
(global-set-key (kbd "M-f") 'ido-find-file)
(global-set-key (kbd "M-b") 'ido-switch-buffer)
(global-set-key (kbd "M-#") 'helm-mini)
(global-set-key (kbd "M-?") 'google-this)


;; I prefer this with ac
(global-set-key (kbd "M--") 'ac-isearch)

(global-set-key (kbd "M-4") 'persp-switch)
(global-set-key (kbd "M-7") 'magit-status)
(global-set-key (kbd "M-9") 'query-replace)
(global-set-key (kbd "M-1") 'align-regexp)
(global-set-key (kbd "M-2") 'ack)

;; window stuff
;(global-set-key (kbd "M-" 'split-window-below))
(global-set-key (kbd "M-+") 'enlarge-window)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-0") 'compile)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-clang-cflags (quote ("-std=c++11")))
 '(ac-modes (quote (emacs-lisp-mode prolog-mode prolog-inferior-mode bibtex-mode d-mode lisp-mode latex-mode LaTeX-mode lisp-interaction-mode slime-repl-mode c-mode cc-mode c++-mode go-mode java-mode malabar-mode clojure-mode clojurescript-mode scala-mode scheme-mode ocaml-mode tuareg-mode coq-mode haskell-mode agda-mode agda2-mode perl-mode cperl-mode python-mode ruby-mode lua-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode ts-mode sclang-mode verilog-mode markdown-mode sml-mode inferior-sml-mode)))
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "b1e54397de2c207e550dc3a090844c4b52d1a2c4a48a17163cce577b09c28236" default)))
 '(sml-program-name "poly"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(set-face-attribute 'default nil :height 180)

(setq
 python-shell-interpreter "ipython3"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
