(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; packages to install if not present
(defvar my-packages '(autopair 
		      paredit
		      slime
		      clojure-mode
		      clojure-test-mode
		      magit 
		      haskell-mode 
		      browse-kill-ring
		      perspective
		      auto-complete
		      ac-slime
		      quack
		      rainbow-delimiters
		      highlight-parentheses)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
