Setup:
------
cd ~
git clone git@github.com:gbarnett/emacs-slim.git .emacs.d
cd .emacs.d
rake
cd auctex
./configure --with-texmf-dir=/usr/local/texlive/texmf-local
make

Note: these steps are for OSX with an installation of MacTeX
(http://tug.org/mactex/).

If you have no use for AucTeX then comment out the appropriate region of init.el
and ignore the 'rake' step onwards.
