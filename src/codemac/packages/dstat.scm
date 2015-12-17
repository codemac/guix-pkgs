(define-module (codemac packages dstat)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages python)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses))

(define-public dstat
  (package
    (name "dstat")
    (version "0.0-git")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dagwieers/dstat.git")
             (commit "64f354c53d3a7e197735007e31ed7d91a4e602a5")))
       (sha256
        (base32 "03bwks3caw3kg73zz1fkqskzgvrbwyr2nj129fxh39nv66pr7z53"))
       (file-name (string-append name "-" version))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((destdir (assoc-ref outputs "out")))
               (unless (zero? (system* "make" (string-append "DESTDIR=" destdir)
                                       "prefix="
                                       "install"))
                 (error "Failed to install"))))))))
    (inputs `(("python" ,python-2)))
    (home-page "http://github.com/dagwieers/dstat/")
    (synopsis "Simple tool for getting many stats")
    (description "A simple tool for getting many stats. Much like iostat +
everything else.")
    (license gpl2+)))
