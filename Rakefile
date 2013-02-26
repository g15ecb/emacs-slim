task :default do
     sh "wget http://ftp.gnu.org/pub/gnu/auctex/auctex-11.87.tar.gz"
     sh "tar xfz auctex-11.87.tar.gz"
     sh "cd auctex-11.87"
     sh "./configure --with-texmf-dir=/usr/local/texlive/texmf-local"
     sh "make"
end
