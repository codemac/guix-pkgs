(define-module (codemac packages sysstat)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages linux)
    #:use-module (gnu packages pkg-config))

(define-public sysstat
  (package
    (name "sysstat")
    (version "11.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://pagesperso-orange.fr/sebastien.godard/"
             name "-" version ".tar.xz"))
       (sha256
        (base32 "0jkh85m8p0m008jj92k0ls3qx7wf64w09q7pkvqz7rm3wrwb8zlw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests?
       #f
       #:make-flags (list "MANGRPARG= "
                          "PREFIX="
                          (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:configure-flags
       '("--enable-install-isag"
         "--enable-copy-only"
         "--disable-nls")))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("lm-sensors" ,lm-sensors)))
    (home-page "http://sebastien.godard.pagesperso-orange.fr/")
    (synopsis "A collection of performance monitoring tools (iostat,isag,mpstat,pidstat,sadf,sar)")
    (description "Monitors tons of stuff.")
    (license gpl2)))
