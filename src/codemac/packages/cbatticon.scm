(define-module (codemac packages cbatticon)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnome)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public cbatticon
  (package
    (name "cbatticon")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/valr/cbatticon/tarball/7ee95555024420d9b5b047cc3fd9f31100be4eda"))
       (sha256
        (base32 "1rbbi0011r4i911fd7hldz22vlqpjsn57zllsgx9qf4i721j2ypj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (add-before 'build 'makeenv
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "CC" (which "gcc"))
             (setenv "PREFIX" (assoc-ref outputs "out")))))))
    (propagated-inputs `(("hicolor-icon-theme" ,hicolor-icon-theme)))
    (native-inputs `(("gcc" ,gcc)
                     ("pkg-config" ,pkg-config)
                     ("gettext" ,gnu-gettext)))
    (inputs `(("libnotify" ,libnotify)
              ("gtk+" ,gtk+)))
    (synopsis "Battery tray icon")
    (description "Displayes a battery tray icon")
    (home-page "http://github.com/valr/cbatticon")
    (license gpl2)))
