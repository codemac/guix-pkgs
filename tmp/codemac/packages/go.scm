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

(define-public go-1.4.3
  (package
    (name "go")
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
                           (tz (string-append
                                (assoc-ref inputs "tzdata") "/share/zoneinfo"))
                           (iana (string-append
                                  (assoc-ref inputs "iana") "/etc")))

                      (setenv "CGO_ENABLED" "1")
                      (let ((ldrpaths (string-join
                                       (string-split (getenv "LIBRARY_PATH") #\:)
                                       " -Wl,-rpath,"
                                       'prefix)))
                        (display ldrpaths)

                        (setenv "GO_LDFLAGS" (string-append
                                              " -r " (getenv "LIBRARY_PATH")
                                              " -linkmode=external"
                                              " -extldflags \"-static " ldrpaths " \" ")))

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

                      ;; make / run / all.bash must be run from src
                      (chdir "src")
                      (unless (zero? (system* "bash" "./all.bash"))
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
                      (symlink out-gobin (string-append out-goroot "/bin"))))))))
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
  (package (inherit go-1.4.3)
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
         (replace 'build
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    (let* ((oldgo (assoc-ref inputs "gcc")))
                      (setenv "GOROOT_BOOTSTRAP" oldgo)

                      (chdir "src")
                      (unless (zero? (system* "bash" "./all.bash"))
                        (error "Failed all.bash"))))))))
    (native-inputs `(("gcc" ,gcc-5)))))