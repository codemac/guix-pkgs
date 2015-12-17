(define-module (codemac packages extempore)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages base)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages compression)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial))

(define-public extempore-llvm
  (package (inherit llvm)
    (name "extempore-llvm")
    (version "3.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://llvm.org/releases/"
                                  version "/llvm-" version ".src.tar.xz"))
              (sha256
               (base32 "0lrirklh4nrcb078qc2f6vbmmc34kxqgsy9s18a1xbfdkmgqjidb"))
              (patches '("/home/codemac/code/guix-pkgs/tmp/codemac/packages/patches/extempore-llvm-3.7.0.patch"))))
    ))

(define-public extempore
  (package
    (name "extempore")
    (version "0.59-git")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/digego/extempore.git")
                    (commit "cae12f29dd03be60123ba0337aa8988d0dc845c5")))
              (sha256
               (base32 "1j7qka3vmc7q4mlb6hlq9qypdv0imcyrrlis1lizzrxhwr08a8fr"))
              (file-name (string-append name "-" version "-cae12f29dd03be60123ba0337aa8988d0dc845c5"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'symlink-llvm
           (lambda* (#:key inputs #:allow-other-keys)
             (symlink (assoc-ref inputs "extempore-llvm") "llvm")))
         (add-before 'install 'compile-stdlib
           (lambda* (#:key inputs #:allow-other-keys)
             (unless (zero? (system* "make" "aot" "aot_extended"))
               (error "could not build aot!")))))
       #:configure-flags
       (list "-DDOWNLOAD_LLVM=OFF"
             (string-append "-DEXT_LLVM_DIR=" (assoc-ref %build-inputs "extempore-llvm")))))
    (native-inputs `(("extempore-llvm" ,extempore-llvm)))
    (inputs
     `(("alsa-lib" ,alsa-lib)))
    (home-page "http://extempore.moso.com.au")
    (synopsis "synth excellences and such")
    (description "This is a thing")
    (license license:bsd-2)))
