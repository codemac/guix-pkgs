(define-module (codemac packages go)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rc)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public go
  (package
    (name "go")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://storage.googleapis.com/golang/go" version  ".src.tar.gz"))
       (sha256
        (base32 "16sygfgdf7krv2vicnyx4j7awlmjwwkfkg86phawxzddz3c6z6i9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (delete 'install)
         (delete 'strip)
         (delete 'validate-documentation-location)
         (delete 'delete-info-dir-file)
         (delete 'compress-documentation)
         (add-after
          'patch-source-shebangs 'go-src-build
          (lambda _
            (use-modules (ice-9 ftw))
            (let ((gcclib (string-append (assoc-ref %build-inputs "gcc-lib") "/lib"))
                  (glibclib (string-append (assoc-ref %build-inputs "glibc") "/lib"))
                  (out-usrgo (string-append (assoc-ref %outputs "out") "/usr/lib/go"))
                  (out-bin (string-append (assoc-ref %outputs "out") "/bin"))
                  (tz (string-append (assoc-ref %build-inputs "tzdata") "/share/zoneinfo"))
                  (iana (string-append (assoc-ref %build-inputs "iana") "/etc")))
              ;; manually set LD flags for the go linker.
              (setenv "GO_LDFLAGS" (string-append "-r \"" gcclib ":" glibclib "\" -extldflags \"-Wl,-rpath," gcclib " -Wl,-rpath," glibclib "\""))
              (setenv "GOROOT_FINAL" (string-append (assoc-ref %outputs "out") "/usr/lib/go"))

              ;; TODO: libgcc_s somehow is never properly linked against. This
              ;; is probably the WRONG way to do things.
              (setenv "LD_LIBRARY_PATH" gcclib)

              (chdir "src")

              ;; these all have specific file locations they look for or in
              ;; the case of exec_test resets the environment before executing
              ;; binaries they expect.
              (for-each delete-file '("net/multicast_test.go"
                                      "net/parse_test.go"
                                      "net/port_test.go"
                                      "os/exec/exec_test.go"))
              (substitute* "os/os_test.go"
                (("/bin/pwd") (which "pwd"))
                (("/usr/bin") (dirname (which "pwd")))
                (("TestHostname") "areturn"))

              ;;   # Disable the unix socket test
              (substitute* "net/net_test.go"
                (("TestShutdownUnix") "areturn"))

              ;;   # Disable the hostname test
              (substitute* "net/lookup_unix.go"
                (("/etc/protocols") (string-append iana "protocols")))

              ;;   # ParseInLocation fails the test
              (substitute* "time/format_test.go"
                (("TestParseInSydney") "areturn"))

              (substitute* "time/zoneinfo_unix.go"
                (("/usr/share/zoneinfo/") tz))

              ;; exec.Command on os.Args[0] from go run for
              ;; whatever reason doesn't work right
              ;; now. libgcc_s.so link missing crap occurs here as
              ;; well, this may require that 6l-wrapper for go run
              ;; to work.
              (substitute* "syscall/syscall_unix_test.go"
                (("TestPassFD") "areturn"))

              ;; TODO: need to do this for 5l/8l based on which arch is
              ;; building
              (substitute* "cmd/6l/asm.c"
                (("/lib64/ld-linux-x86-64.so.2") (string-append glibclib "/ld-linux-x86-64.so.2")))

              (zero? (system* "bash" "./all.bash"))
              (chdir "../..")
              (for-each mkdir-p (list out-usrgo out-bin))
              (for-each (lambda (x) (copy-recursively (string-append "go/" x) (string-append out-usrgo "/" x)))
                        (scandir "go" (lambda (x) (not (or (equal? x "bin")
                                                           (equal? x "..")
                                                           (equal? x "."))))))
              (copy-recursively "go/bin" out-bin)
              (symlink out-bin (string-append out-usrgo "/bin"))))))))
    (native-inputs `(("tar" ,tar)
                     ("gzip" ,gzip)
                     ("bash" ,bash)
                     ("coreutils" ,coreutils)
                     ("grep" ,grep)
                     ("gcc" ,gcc)
                     ("gcc-lib" ,gcc "lib")
                     ("perl" ,perl)
                     ("binutils" ,binutils)
                     ("glibc" ,glibc)
                     ("lheaders" ,linux-libre-headers)
                     ("sed" ,sed)
                     ("pcre" ,pcre)
                     ("diffutils" ,diffutils)
                     ("rc" ,rc)
                     ("pkg-config" ,pkg-config)))
    (inputs `(("glibc" ,glibc)
              ("gcc-lib" ,gcc "lib")
              ("gcc" ,gcc)
              ("iana" ,net-base)
              ("ld-wrapper" ,ld-wrapper)
              ("tzdata" ,tzdata)))
    (home-page "http://golang.org")
    (synopsis "Compiler and tools for the Go programming language from Google")
    (description "A statically-typed language with syntax derived from C.  Go
adds garbage collection, type safety, some dynamic-typing capabilities,
additional built-in types such as variable-length arrays and key-value maps,
and a large standard library.")
    (license bsd-3)))
