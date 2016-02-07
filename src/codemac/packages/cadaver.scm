(define-module (codemac packages cadaver)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages certs)
    #:use-module (gnu packages tls)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public cadaver
  (package
    (name "cadaver")
    (version "0.23.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.webdav.org/"
                                  name "/" name "-" version ".tar.gz"))
              (sha256
               (base32 "1jizq69ifrjbjvz5y79wh1ny94gsdby4gdxwjad4bfih6a5fck7x"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--with-ssl=openssl"
                               (string-append "--with-ca-bundle=" (assoc-ref %build-inputs "nss-certs")
                                              "/etc/ssl/certs/ca-certificats.crt"))
       #:tests? #f))
    (native-inputs `(("openssl" ,openssl)))
    (inputs `(("nss-certs" ,nss-certs)))
    (home-page "http://www.webdav.org")
    (synopsis "A command line client for webdav")
    (description "Cadaver is a command line client for webdav.")
    (license license:gpl2+)))
