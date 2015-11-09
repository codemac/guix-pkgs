(define-module (codemac packages ledger)
  #:use-module (guix packages)
  #:use-module ((guix licenses)  #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages base)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages python)
  #:use-module (gnu packages texinfo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu))

(define-public utfcpp
  (package
    (name "utfcpp")
    (version "2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jwiegley/utfcpp.git")
                    (commit "2233ec933f5661c7050b94d3b14f5f9f51ae3d55")))
              (sha256
               (base32 "13909xsr1lk35cch72piqi8pk3s8zzvbh5mz988is20bbwwppq2h"))
              (file-name (string-append name "-" version))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((incdir (string-append (assoc-ref outputs "out") "/include")))
               (copy-recursively "source" incdir)))))))
    (home-page "http://github.com/jwiegley/utfcpp")
    (synopsis "Small generic C++ library for UTF-8 encoded strings")
    (description "A small generic C++ library for UTF-8 encoded strings.  ICU
or other libraries are preferred, this is jwiegly's fork used for ledger.")
    (license license:bsd-3)))

(define-public boost-1.57.0
  (package (inherit boost)
           (version "1.57.0")
           (source (origin
                     (method url-fetch)
                     (uri (string-append
                           "mirror://sourceforge/boost/boost_"
                           (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)
                           ".tar.bz2"))
                     (sha256
                      (base32
                       "0rs94vdmg34bwwj23fllva6mhrml2i7mvmlb11zyrk1k5818q34i"))))))

;; boost v1.58.0 fails building ledger with _xdata != NULL checks failing (due
;; to their use of boost::optional). This is why we wrap the current boost
;; version and set it to 1.57.0.
;;
;; TODO: The next version of ledger will support boost 1.58.0+, so make sure
;; to upgrade boost then.
(define-public ledger
  (package
    (name "ledger")
    (version "3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/"
                                  name "/" name "/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "1576ghsln9y4blznlil19x1hq2539k2vq8j8r4zr4d4qf9hd5dgf"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-testfile
           (lambda _
             (substitute* "test/LedgerHarness.py"
               (("/usr/bin/valgrind") (which "valgrind")))
             (substitute* "test/fullcheck.sh"
               (("/usr/bin/valgrind") (which "valgrind"))))))))
    (native-inputs `(("boost" ,boost-1.57.0)
                     ("python" ,python-2)
                     ("valgrind" ,valgrind)
                     ("sed" ,sed)
                     ("utfcpp" ,utfcpp)
                     ("texinfo" ,texinfo)))
    (inputs `(("libedit" ,libedit)
              ("gmp" ,gmp)
              ("mpfr" ,mpfr)
              ("icu4c" ,icu4c)))
    (synopsis "Ledger is a powerful, double-entry accounting system that is
accessed from the UNIX command-line.")
    (description "Ledger is a powerful, double-entry accounting system that is
accessed from the UNIX command-line.  This may put off some users, as there is
no flashy UI, but for those who want unparalleled reporting access to their
data, there really is no alternative.")
    (home-page "http://ledger-cli.org")
    (license license:bsd-3)))
