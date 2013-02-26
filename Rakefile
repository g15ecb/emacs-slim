task :default do
     sh "wget http://ftp.gnu.org/pub/gnu/auctex/auctex-11.87.tar.gz"
     sh "mv auctex-11.87.tar.gz auctex.tar.gz"
     sh "tar xfz auctex.tar.gz"
     sh "cd auctex"
     sh "./configure --with-texmf-dir=/usr/local/texlive/texmf-local"
     sh "make"
end
