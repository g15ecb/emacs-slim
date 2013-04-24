;; Granville Barnett's Emacs Config
;; granvillebarnett@gmail.com

;; NB. Some of the settings in this file require external tools to have been
;; installed, they include:
;; - OCaml and Opam (brew install ocaml opam; opam must be put on PATH)
;; - opam install merlin ocp-indent
;; - LaTeX (MacTeX)

;; Vanila Settings BEGIN
;; *****************************************************************************
;; UI customisations
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)            
(setq inhibit-startup-message t inhibit-startup-echo-area-message t)
(set-face-attribute 'default nil :height 240 :font "Menlo")

(setq ring-bell-function 'ignore)                                   
(line-number-mode t)                     
(column-number-mode t)                   
(size-indication-mode t)                 

;; some basic buffer stuff
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq default-major-mode 'text-mode)
(global-font-lock-mode t)

(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have
(setq ido-enable-last-directory-history nil) ; forget latest selected directory

;; some backup stuff
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

(server-start)

(setq-default ispell-program-name "/usr/local/bin/aspell")
(setq mac-command-modifier 'meta)

;; Vanila Settings END
;; *****************************************************************************



;; Elpa BEGIN
;; *****************************************************************************

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; PACKAGES BEGIN  ----------------------------------------------
(defvar my-packages '(magit 
		      auto-complete
                      autopair
		      browse-kill-ring
		      perspective
		      rainbow-delimiters
		      evil)
		      
  "A list of packages to ensure are installed at launch.")
;; PACKAGES END -------------------------------------------------

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Elpa END
;; *****************************************************************************


;; =============================================================================
;; -----------------------------------------------------------------------------
;; =============================================================================

;; Setup BEGIN
;; *****************************************************************************

(evil-mode)

(persp-mode)
(browse-kill-ring-default-keybindings) 	; M-y to browse kill ring

(require 'autopair)

;; AC BEGIN -----------------------------------------------------
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;; make auto-complete show immediately
(setq ac-auto-show-menu 0.)
;; AC END -------------------------------------------------------

;; OCaml BEGIN --------------------------------------------------
(add-to-list 'load-path "~/.opam/4.00.1/share/emacs/site-lisp/")
(require 'merlin)

(add-hook 'tuareg-mode-hook 'merlin-mode)
;; utop
;;(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
;;(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
;;(add-hook 'typerex-mode-hook 'utop-setup-ocaml-buffer)
;; OCaml END ----------------------------------------------------

;; AucTeX BEGIN -------------------------------------------------
;; rake; then cd into auctex dir
;; ./configure --with-texmf-dir=/usr/local/texlive/texmf-local
;; make
 (add-to-list 'load-path "~/.emacs.d/auctex-11.87")
 (add-to-list 'load-path "~/.emacs.d/auctex-11.87/preview")
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
;; AucTeX END ---------------------------------------------------

;; Setup END
;; *****************************************************************************


;; Hooks BEGIN
;; *****************************************************************************
(defun default-hooks() 
  (autopair-mode)
  (show-paren-mode)
  (rainbow-delimiters-mode))

;; Hooks END
;; *****************************************************************************



;; Keybindings BEGIN
;; *****************************************************************************
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))


(global-set-key (kbd "M-f") 'ido-find-file)
(global-set-key (kbd "M-b") 'ido-switch-buffer)
(global-set-key (kbd "M-s") 'ac-isearch)

(global-set-key (kbd "M-4") 'persp-switch)
(global-set-key (kbd "M-7") 'magit-status)
(global-set-key (kbd "M-9") 'query-replace)
(global-set-key (kbd "M-1") 'align-regexp)
(global-set-key (kbd "M-2") 'ack)

;; window stuff
(global-set-key (kbd "M-+") 'enlarge-window)
(global-set-key (kbd "M-+") 'enlarge-window)
(global-set-key (kbd "M-o") 'other-window)

;; UI
;; (global-set-key [f9] 'ns-toggle-fullscreen)


;; Keybindings END
;; *****************************************************************************
