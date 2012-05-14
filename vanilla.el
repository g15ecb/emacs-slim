;; UI customisations
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)            
(setq inhibit-startup-message t inhibit-startup-echo-area-message t)
(set-face-attribute 'default nil :height 220)
(setq ring-bell-function 'ignore)                                   
(line-number-mode t)                     
(column-number-mode t)                   
(size-indication-mode t)                 

;; some basic buffer stuff
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq default-major-mode 'text-mode)
(global-font-lock-mode t)

;; I use a mac keyboard on desktop and machine (m'b pro)
(fset 'insertPound
   "#")
(global-set-key (kbd "M-3") 'insertPound)

(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have
(setq ido-enable-last-directory-history nil) ; forget latest selected directory

; return a backup file path of a give file path
; with full directory mirroring from a root dir
; non-existant dir will be created
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

(add-to-list 'default-frame-alist '(width . 85))
(add-to-list 'default-frame-alist '(left . 270))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq mac-command-modifier 'meta)

(setq-default ispell-program-name "/usr/local/bin/aspell")
