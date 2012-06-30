My Emacs Configuration
======================

# Target Users

Those who are mainly hacking on functional languages (Haskell and Clojure), the hybrid
language Scala and doing the odd bit of LaTeX here and there. There is also Git
support in the form of the excellent Magit.

# Requirements

* Emacs 24+ (mainly due to package.el)

# Folder Layout

* Root/
** elpa/
** non-elpa/
* init.el
* vanilla.el 
* setup.el
* non-elpa-setup.el
* keybindings.el

Briefly: init.el loads the *.el files from the root directory (typically this is
.emacs.d). The directory elpa is where the elpa packages will be put. vanilla.el
comprises the basic Emacs settings such as font, etc. setup.el comprises of the
setup steps that are required for the elpa packages to work. non-elpa-setup.el
is the same as setup.el but for packages that are not in the elpa repositories
at this moment in time. All non-elpa packages should be placed in the non-elpa
directory and "setup" in non-elpa-setup.el. Currently this comprises only
AucTeX. (I doubt this will ever go into elpa due to its size and configuration
requirements.) keybindings.el is where you should put all your custom key
bindings (surprise, surprise!).

That's it. Hopefully it's useful to someone out there. I've found this setup to
be the most pleasant thus far after using Emacs for years.



Granville
