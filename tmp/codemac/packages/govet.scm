(define-module (codemac packages govet)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (codemac packages go)
  #:use-module (guix packages)
  #:use-module (guix licenses))

(define-public govet
  (package
    (name "govet")
    (version "5a6a7b4d4072b8742ae111275d27779f79c533e0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git@github.com:golang/tools.git")
                    (commit version)))
              (sha256
               (base32 "0864qqw8dagzdpydbg6z05r1a6i367xxq23d086syis13c43zsvh"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'install)
                  (replace 'build
                           (lambda* (#:keys inputs outputs #:allow-other-keys)
                             (let ((gopath (string-append (assoc-ref outputs "out") "gopath")))
                               (unless (zero? (system* "go" "get" "golang.org/x/tools/cmd/vet")))))))))
    (inputs `(("go" ,go)))
    (synopsis "Tool to vet go source files")
    (description "vet (notably go vet) is run over your files and finds common
errors.")
    (home-page "http://golang.org/x/tools/cmd/vet")
    (license bsd-3)))
