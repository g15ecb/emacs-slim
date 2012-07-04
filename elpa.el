(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; packages to install if not present
(defvar my-packages '(magit 
		      ;; paredit
		      ;; slime
		      ;; clojure-mode
		      ;; clojure-test-mode
		      haskell-mode 
		      ;; auto-complete
		      ;; ac-slime
		      scala-mode
		      browse-kill-ring
		      perspective
		      rainbow-delimiters
		      fastnav)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
