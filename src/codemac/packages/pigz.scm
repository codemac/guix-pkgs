
(define-module (codemac packages pigz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc))

(define-public pigz
  (package
    (name "pigz")
    (version "2.3.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://zlib.net/pigz/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "172hdf26k4zmm7z8md7nl0dph2a7mhf3x7slb9bhfyff6as6g2sf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (bin (string-append out "/bin"))
                           (man (string-append out "/share/man/man1")))
                      (mkdir-p bin)
                      (copy-file "pigz" (string-append bin "/pigz"))
                      (symlink
                       (string-append bin "/pigz")
                       (string-append bin  "/unpigz"))
                      (mkdir-p man)
                      (copy-file "pigz.1" (string-append man "/pigz.1")))
                    #t)))
       #:make-flags (list "CC=gcc")
       #:test-target "tests"))
    (inputs `(("zlib" ,zlib)))
    (home-page "http://zlib.net/pigz/")
    (synopsis "Parallel implementation of gzip for multi-processor,
multi-core machines")
    (description "A parallel implementation of gzip that is a fully functional
replacement for gzip that exploits multiple processors and multiple cores when
compressing data. pigz was written by Mark Adler, and uses the zlib and
pthread libraries.")
    (license license:bsd-3)))
