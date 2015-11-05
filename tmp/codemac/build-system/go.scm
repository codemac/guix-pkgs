(define-module (codemac build-system go)
  #:use-module (guix build-system gnu)
  #:use-module (guix store)
  #:use-module (guix utils))

(define %go-build-system-modules
  `((guix build go-build-system)
    ,@%gnu-build-system-modules))

(define (default-go)
  "Return the default go package"
  (let ((go (resolve-interface '(codemac packages go))))
    (module-ref go 'go)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (go (default-go))
                #:allow-othe-keys
                #:rest arguments)
  name
  )


(define* (lower name
                #:key source inputs native-inputs outputs system target
                (haskell (default-haskell))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:haskell #:inputs #:native-inputs))

  (and (not target)                               ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs

                        ;; Keep the standard inputs of 'gnu-build-system'.
                        ,@(standard-packages)))
         (build-inputs `(("haskell" ,haskell)
                         ,@native-inputs))
         (outputs outputs)
         (build haskell-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (go-build store name inputs
                   #:key source
                   (haddock? #t)
                   (haddock-flags ''())
                   (tests? #t)
                   (test-target "test")
                   (configure-flags ''())
                   (phases '(@ (codemac build haskell-build-system)
                               %standard-phases))
                   (outputs '("out"))
                   (search-paths '())
                   (system (%current-system))
                   (guile #f)
                   (imported-modules %haskell-build-system-modules)
                   (modules '((guix build haskell-build-system)
                              (guix build utils))))
  "Build SOURCE using HASKELL, and with INPUTS.  This assumes that SOURCE
provides a 'Setup.hs' file as its build system."
  (define builder
    `(begin
       (use-modules ,@modules)
       (haskell-build #:name ,name
                      #:source ,(match (assoc-ref inputs "source")
                                  (((? derivation? source))
                                   (derivation->output-path source))
                                  ((source)
                                   source)
                                  (source
                                   source))
                      #:configure-flags ,configure-flags
                      #:haddock-flags ,haddock-flags
                      #:system ,system
                      #:test-target ,test-target
                      #:tests? ,tests?
                      #:haddock? ,haddock?
                      #:phases ,phases
                      #:outputs %outputs
                      #:search-paths ',(map search-path-specification->sexp
                                            search-paths)
                      #:inputs %build-inputs)))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system #:graft? #f))
      (#f                                         ; the default
       (let* ((distro (resolve-interface '(gnu packages commencement)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system #:graft? #f)))))

  (build-expression->derivation store name builder
                                #:inputs inputs
                                #:system system
                                #:modules imported-modules
                                #:outputs outputs
                                #:guile-for-build guile-for-build))


(define go-build-system
  (build-system
    (name 'go)
    (description "From .go to $GOPATH, the go build system for guix")
    (lower lower)))
