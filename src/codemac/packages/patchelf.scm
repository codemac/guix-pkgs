(define-module (codemac packages patchelf)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:select (gpl3+ lgpl3+ lgpl2.0+))
  #:use-module (gnu packages)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages compression))

(define-public patchelf-poop
  (package
    (name "patchelf-poop")
    (version "0.8.git")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url "git://github.com/NixOS/patchelf.git")
                                  (commit "b202ad239ed815fbc59dd0a5cb2def5991116c42")))
             (sha256
              (base32
               "1rqpg84wrd3fa16wa9vqdvasnc05yz49w207cz1l0wrl4k8q97y9"))
             (patches (list (search-patch "patchelf-page-size.patch")))))
    (build-system gnu-build-system)

    ;; XXX: The upstream 'patchelf' doesn't support ARM.  The only available
    ;;      patch makes significant changes to the algorithm, possibly
    ;;      introducing bugs.  So, we apply the patch only on ARM systems.
    (inputs
     (if (string-prefix? "arm" (or (%current-target-system) (%current-system)))
         `(("patch/rework-for-arm" ,(search-patch
                                     "patchelf-rework-for-arm.patch")))
         '()))
    (arguments
     (if (string-prefix? "arm" (or (%current-target-system) (%current-system)))
         `(#:phases (alist-cons-after
                     'unpack 'patch/rework-for-arm
                     (lambda* (#:key inputs #:allow-other-keys)
                       (let ((patch-file
                              (assoc-ref inputs "patch/rework-for-arm")))
                         (zero? (system* "patch" "--force" "-p1"
                                         "--input" patch-file))))
                     %standard-phases))
         '()))

    (home-page "http://nixos.org/patchelf.html")
    (synopsis "Modify the dynamic linker and RPATH of ELF executables")
    (description
     "PatchELF allows the ELF \"interpreter\" and RPATH of an ELF binary to be
changed.")
    (license gpl3+)))
