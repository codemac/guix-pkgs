(define-module (codemac packages cscope)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages ncurses))

(define-public cscope
  (package
    (name "cscope")
    (version "15.8a")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://downloads.sourceforge.net/" name "/" name "-" version ".tar.gz"))
              (sha256
               (base32 "07jdhxvp3dv7acvp0pwsdab1g2ncxjlcf838lj7vxgjs1p26lwzb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list (string-append "--with-ncurses=" (assoc-ref %build-inputs "ncurses")))))
    (inputs `(("ncurses" ,ncurses)))
    (home-page "http://cscope.sourceforge.net")
    (synopsis "Tool for browsing source code, with ncurses and editor interfaces")
    (description "A tool for browsing source code. It has an information
database generated for faster searches and later reference, a fuzzy parser
that supports C, but is flexible enough to be useful for C++ and Java, and has
a command line mode for inclusion in scripts or as a backend to a
GUI/frontend.")
    (license license:bsd-3)))

