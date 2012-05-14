(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; packages to install if not present
(defvar my-packages '(autopair 
		      paredit 
		      magit 
		      clojure-mode
		      haskell-mode 
		      browse-kill-ring
		      slime 
		      clojure-test-mode
		      highlight-parentheses
		      rainbow-delimiters)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
