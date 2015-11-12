(define-module (codemac packages go)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rc)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix utils)
  #:use-module (guix packages))

(define (go-dl-uri version)
  (string-append "https://storage.googleapis.com/golang/go" version ".src.tar.gz"))

(define-public broken-go-1.4.3
  (package
    (name "broken-go")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (go-dl-uri version))
       (sha256
        (base32 "0na9yqilzpvq0bjndbibfp07wr796gf252y471cip10bbdqgqiwr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; this isn't really a gnu build system, but we are borrowing many of
         ;; it's rules, easier to replace the parts where go is different.
         (delete 'configure)

         ;; strip cannot recognize statically compiled go programs (prints
         ;; that it can't recagnize the format of the file.
         (delete 'strip)

         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out-goroot (string-append
                                 (assoc-ref outputs "out") "/lib/go"))
                    (ld-linux (string-append (assoc-ref inputs "glibc") "/lib/ld-linux-x86-64.so.2"))
                    (tz (string-append
                         (assoc-ref inputs "tzdata") "/share/zoneinfo"))
                    (iana (string-append
                           (assoc-ref inputs "iana") "/etc")))
               
               (setenv "CGO_ENABLED" "1")
               
               (setenv "GO_LDFLAGS" (string-append
                                     " -r " (getenv "LIBRARY_PATH")
                                     " -extldflags \""
                                     (string-join
                                      (string-split (getenv "LIBRARY_PATH") #\:)
                                      " -Wl,-rpath,"
                                      'prefix)
                                     "\""
                                     ))

               ;; make sure tests pass, they do not respect our more
               ;; permanently set ldflags below. If there is a better
               ;; way of doing this I welcome it.

               (setenv "LD_LIBRARY_PATH" (getenv "LIBRARY_PATH"))
               
               ;; (substitute* "src/runtime/cgo/cgo.go"
               ;;   (("#cgo !android,linux LDFLAGS: -lpthread")
               ;;    (string-append "#cgo !android,linux LDFLAGS: "
               ;;                   (string-join
               ;;                    (string-split (getenv "LIBRARY_PATH") #\:)
               ;;                    " -Wl,-rpath,"
               ;;                    'prefix)
               ;;                   (string-join (string-split (getenv "LIBRARY_PATH") #\:)
               ;;                                " -L" 'prefix)
               ;;                   " -lpthread")))
               (setenv "GOROOT_FINAL" out-goroot)
               
               ;; these all have specific file locations they look for
               ;; or in the case of exec_test resets the environment
               ;; before executing binaries they expect.
               (for-each delete-file '("src/net/multicast_test.go"
                                              "src/net/parse_test.go"
                                              "src/net/port_test.go"
                                              "src/os/exec/exec_test.go"))
               (substitute* "src/os/os_test.go"
                 (("/bin/pwd") (which "pwd"))
                 (("/usr/bin") (dirname (which "pwd")))
                 (("TestHostname") "areturn"))
               
               ;; Disable the unix socket test
               (substitute* "src/net/net_test.go"
                 (("TestShutdownUnix") "areturn"))
               
               (substitute* "src/net/lookup_unix.go"
                 (("/etc/protocols") (string-append iana "protocols")))
               
               ;; ParseInLocation fails the test
               (substitute* "src/time/format_test.go"
                 (("TestParseInSydney") "areturn"))
               
               (substitute* "src/time/zoneinfo_unix.go"
                 (("/usr/share/zoneinfo/") tz))
               
               ;; exec.Command on os.Args[0] from go run for whatever
               ;; reason doesn't work right now. libgcc_s.so link
               ;; missing crap occurs here as well, this may require
               ;; that 6l-wrapper for go run to work.
               (substitute* "src/syscall/syscall_unix_test.go"
                 (("TestPassFD") "areturn"))

               (substitute* "src/cmd/6l/asm.c"
                 (("char linuxdynld.*$") (string-append "char linuxdynld[] = \"" ld-linux "\";\n")))

               ;; arm and i686 should be done as well, don't know about
               ;; multi-arch in guix (substitute* '( "src/cmd/5l/asm.c"
               ;; "src/cmd/8l/asm.c") ) make / run / all.bash must be run from
               ;; src
               (chdir "src")
               (unless (zero? (system* "bash" "./make.bash"))
                 (error "all.bash failed")))
             (chdir "..")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (use-modules (ice-9 ftw))
             (let* ((out (assoc-ref outputs "out"))
                    (out-goroot (string-append out "/lib/go"))
                    (out-gobin (string-append out "/bin")))
               (chdir "..")
               (for-each mkdir-p (list out-goroot out-gobin))
               (for-each (lambda (x)
                           (copy-recursively
                            (string-append "go/" x)
                            (string-append out-goroot "/" x)))
                         (scandir "go" (lambda (x)
                                         (not (or (equal? x "bin")
                                                  (equal? x "..")
                                                  (equal? x "."))))))
               (copy-recursively "go/bin" out-gobin)
               (symlink out-gobin (string-append out-goroot "/bin")))))
         (add-after
             'install
             'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((prog (string-append (assoc-ref outputs "out") "/bin/go")))
               (wrap-program prog
                 `("GO_LDFLAGS" prefix (,(string-append "-r "(getenv "LIBRARY_PATH"))))
                 `("CGO_LDFLAGS" prefix (,(string-join
                                           (string-split (getenv "LIBRARY_PATH") #\:)
                                           " -Wl,-rpath,"
                                           'prefix)))
                 `("GOROOT" = (,(string-append (assoc-ref outputs "out") "/lib/go"))))))))))
    (native-inputs `(("gcc-lib" ,gcc "lib")
                     ("glibc" ,glibc)
                     ("gcc" ,gcc)
                     ("perl" ,perl)
                     ("sed" ,sed)
                     ("pcre" ,pcre)
                     ("rc" ,rc)
                     ("pkg-config" ,pkg-config)))
    (inputs `(("glibc" ,glibc)
              ("gcc-lib" ,gcc "lib")
              ("gcc" ,gcc)
              ("iana" ,net-base)
              ("linux-libre-headers" ,linux-libre-headers)
              ("ld-wrapper" ,ld-wrapper)
              ("tzdata" ,tzdata)))
    (home-page "http://golang.org")
    (synopsis "Compiler and tools for the Go programming language from Google")
    (description "A statically-typed language with syntax derived from C.  Go
adds garbage collection, type safety, some dynamic-typing capabilities,
additional built-in types such as variable-length arrays and key-value maps,
and a large standard library.")
    (license bsd-3)))

(define* (custom-gcc gcc name languages #:key (separate-lib-output? #t))
  "Return a custom version of GCC that supports LANGUAGES."
  (package (inherit gcc)
    (name name)
    (outputs (if separate-lib-output?
                 (package-outputs gcc)
                 (delete "lib" (package-outputs gcc))))
    (arguments
     (substitute-keyword-arguments `(#:modules ((guix build gnu-build-system)
                                                (guix build utils)
                                                (ice-9 regex)
                                                (srfi srfi-1)
                                                (srfi srfi-26))
                                               ,@(package-arguments gcc))
       ((#:configure-flags flags)
        `(cons (string-append "--enable-languages="
                              ,(string-join languages ","))
               (remove (cut string-match "--enable-languages.*" <>)
                       ,flags)))))))

(define-public gccgo-5
  (custom-gcc gcc-5 "gccgo" '("go") #:separate-lib-output? #f))

;; TODO: when GCC is upgraded to gcc 5 this can lose it's dependency on
;; go-1.4.2 for bootstrapping, and use gccgo. This may have bugs though, don't
;; know what the Go's project's test framework is like. It would only need to
;; be a correct Go implementation for the duration of build.
(define-public go-1.5.1
  (package (inherit broken-go-1.4.3)
    (name "go")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (go-dl-uri version))
       (sha256
        (base32 "0ss3xkqcy15sqkcrg47sr84xgp19gicxvdx9jvijm9yrk0z8g2d8"))))
    (arguments
     `(#:tests? #f ; included in go's build
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'strip)
         (replace 'build
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    (let* ((oldgo (assoc-ref inputs "go")))
                      (setenv "GOROOT_BOOTSTRAP" oldgo)

                      ;; for the crippled go to work
                      (setenv "LD_LIBRARY_PATH" (getenv "LIBRARY_PATH"))

                      ;; things to get the test working:
                      (substitute* "src/net/listen_test.go"
                        (("TestIPv4MulticastListener") "areturn"))

                      ;; need to do the iana hack!
                      (substitute* "src/net/parse_test.go"
                        (("TestReadLine") "areturn1"))

                      (substitute* "src/net/port_test.go"
                        (("TestLookupPort") "areturn2"))

                      (substitute* "src/os/os_test.go"
                        (("TestReaddirnamesOneAtATime") "areturn")
                        (("TestStartProcess") "areturn1")
                        (("TestChdirAndGetwd") "areturn2")
                        (("TestHostname") "areturn3"))

                      ;; this had a libgcc_s error, fuck
                      (delete-file "src/os/exec/exec_test.go")
                      
                      (chdir "src")
                      (unless (zero? (system* "bash" "./make.bash"))
                        (error "Failed all.bash")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (use-modules (ice-9 ftw))
             (let* ((out (assoc-ref outputs "out"))
                    (out-goroot (string-append out "/lib/go"))
                    (out-gobin (string-append out "/bin")))
               (chdir "../..")
               (for-each mkdir-p (list out-goroot out-gobin))
               (let ((sd (scandir "go" (lambda (x) (not (or (equal? x "bin")
                                                            (equal? x "..")
                                                            (equal? x ".")))))))
                 (format #t "~%~%~s~%~%" sd)
                 (for-each (lambda (x)
                             (copy-recursively
                              (string-append "go/" x)
                              (string-append out-goroot "/" x)))
                           sd))
               (copy-recursively "go/bin" out-gobin)
               (symlink out-gobin (string-append out-goroot "/bin"))))))))
    (native-inputs `(("go" ,broken-go-1.4.3)
                     ("rc" ,rc)
                     ("perl" ,perl)))))
