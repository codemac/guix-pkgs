(define-module (codemac packages rc)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (guix build gnu-build-system)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix packages))


(define-public rc
  (package
    (name "rc")
    (version "1.7.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/rakitzis/rc/tarball/"
				 "c884da53a7c885d46ace2b92de78946855b18e92"))
             (sha256
              (base32 "05hlnqcxaw08m1xypk733hajwaap5pr354ndmrm86k0flisjk0fw"))))
    (build-system gnu-build-system)
    (arguments `(#:configure-flags
		 '("--with-edit=gnu")
		 #:phases
		 (modify-phases %standard-phases
		   (add-before 'configure 'autoreconf (lambda _
							(zero? (system* "autoreconf" "-vfi")))))
		 #:tests? #f))
    (inputs `(("readline" ,readline)
	      ("perl" ,perl)))
    (native-inputs `(("autoconf" ,autoconf)
		     ("automake" ,automake)
		     ("libtool" ,libtool)
		     ("pkg-config" ,pkg-config)))
    (synopsis "An alternative implementation of the plan 9 rc shell.")
    (description "Byron's rc implementation. Features much wine and such.")
    (home-page "http://github.com/rakitzis/rc")
    (license zlib)))
