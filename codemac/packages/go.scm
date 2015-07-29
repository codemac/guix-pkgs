(define-module (codemac packages go)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash))

(define-public go
  (package
    (name "go")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://storage.googleapis.com/golang/" name version  ".src.tar.gz"))
       (sha256
        (base32 "16sygfgdf7krv2vicnyx4j7awlmjwwkfkg86phawxzddz3c6z6i9"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
                 #:builder
                 (begin
                   (use-modules (guix build utils))
                   (use-modules (srfi srfi-1))
                   (use-modules (ice-9 ftw))
                   (let* ((tarballgz (assoc-ref %build-inputs "source"))
                          (out-usrgo (string-append (assoc-ref %outputs "out") "/usr/lib/go"))
                          (out-bin (string-append (assoc-ref %outputs "out") "/bin"))
                          (tar (string-append (assoc-ref %build-inputs "tar") "/bin"))
                          (gzip (string-append (assoc-ref %build-inputs "gzip") "/bin"))
                          (bash (string-append (assoc-ref %build-inputs "bash") "/bin"))
                          (cu (string-append (assoc-ref %build-inputs "coreutils") "/bin"))
                          (gcc (string-append (assoc-ref %build-inputs "gcc") "/bin"))
                          (gcclib (string-append (assoc-ref %build-inputs "gcc") "/lib"))
                          (grep (string-append (assoc-ref %build-inputs "grep") "/bin"))
                          (lh (string-append (assoc-ref %build-inputs "lheaders") "/include"))
                          (glibclib (string-append (assoc-ref %build-inputs "glibc") "/lib"))
                          (bu (string-append (assoc-ref %build-inputs "binutils") "/bin"))
                          (iana (string-append (assoc-ref %build-inputs "iana") "/etc"))
                          (sed (string-append (assoc-ref %build-inputs "sed") "/bin/sed"))
                          (tz (string-append (assoc-ref %build-inputs "tzdata") "/share/zoneinfo")))
                     (begin
                       (setenv "PATH" (fold (lambda (x y) (string-append y ":" x)) "."
                                            (list tar gzip bash cu gcc grep bu)))
                       (zero? (system* "tar" "xvf" tarballgz))
                       (chdir "go/src")
                       ;;   # Disabling the 'os/http/net' tests (they want files not available in
                       ;;   # chroot builds)
                       ;;   rm src/net/{multicast_test.go,parse_test.go,port_test.go}

                       (for-each delete-file '("net/multicast_test.go"
                                               "net/parse_test.go"
                                               "net/port_test.go"))
                       
                       ;;   # The os test wants to read files in an existing path. Just don't let it be /usr/bin.
                       ;;   sed -i 's,/usr/bin,'"`pwd`", src/os/os_test.go
                       ;;   sed -i 's,/bin/pwd,'"`type -P pwd`", src/os/os_test.go
                       
                       (substitute* "os/os_test.go"
                         (("/bin/pwd") (which "pwd"))
                         (("/usr/bin") (dirname (which "pwd")))
                         (("TestHostname") "areturn"))

                       ;;   # Disable the unix socket test
                       ;;   sed -i '/TestShutdownUnix/areturn' src/net/net_test.go

                       (substitute* "net/net_test.go"
                         (("TestShutdownUnix") "areturn"))

                       ;;   # Disable the hostname test
                       ;;   sed -i '/TestHostname/areturn' src/os/os_test.go
                       ;;   sed -i 's,/etc/protocols,${iana_etc}/etc/protocols,' src/net/lookup_unix.go

                       (substitute* "net/lookup_unix.go"
                         (("/etc/protocols") (string-append iana "protocols")))

                       ;;   # ParseInLocation fails the test
                       ;;   sed -i '/TestParseInSydney/areturn' src/time/format_test.go
                       (substitute* "time/format_test.go"
                         (("TestParseInSydney") "areturn"))

                       ;; '' + lib.optionalString stdenv.isLinux ''
                       ;;   sed -i 's,/usr/share/zoneinfo/,${tzdata}/share/zoneinfo/,' src/time/zoneinfo_unix.go
                       (substitute* "time/zoneinfo_unix.go"
                         (("/usr/share/zoneinfo/") tz))

                       ;;   sed -i 's,/lib/ld-linux.so.3,${loaderArm},' src/cmd/5l/asm.c
                       ;;   sed -i 's,/lib64/ld-linux-x86-64.so.2,${loaderAmd64},' src/cmd/6l/asm.c
                       ;; (substitute* "cmd/6l/asm.c"
                       ;;   (("/lib64/ld-linux-x86-64.so.2") (string-append glibclib "/ld-linux-x86-64.so.2")))
                       ;;   sed -i 's,/lib/ld-linux.so.2,${loader386},' src/cmd/8l/asm.c

                       ;; patch the go tools rpath before running run.bash. Will repatch everything at the end

                       ;;
                       ;; (string-append "GO_LDFLAGS=-r '" gcclib ":" glibclib "'")
                       ;; (string-append "GO_LDFLAGS=-extldflags -static")
                       
                       ;; (zero? (system* sed "-i"
                       ;;                 (string-append "13i" patchelf " --set-interpreter " (string-append glibclib "/ld-linux-x86-64.so.2") " ../bin/go") "./all.bash"))
                       ;; (zero? (system* sed "-i" (string-append "13i" patchelf " --set-rpath " (string-append gcclib ":" glibclib) " ../bin/go") "./all.bash"))
                       (zero? (system* sed "-i" "3iset -x" "./make.bash"))
                       (zero? (system* sed "-i" "3iset -x" "./run.bash"))
                       (zero? (system* sed "-i" "3iset -x" "./all.bash"))
                       (zero? (system* "env"
                                       (string-append "GO_LDFLAGS=-linkmode external -extldflags \"-Wl,-rpath," gcclib " -Wl,-rpath," glibclib "\"")
                                       (string-append "GOROOT_FINAL=" out-usrgo)
                                       (string-append "CPATH=" lh)
                                       (string-append "LIBRARY_PATH=" glibclib ":" gcclib)
                                       (string-append "LD_LIBRARY_PATH=" gcclib)
                                       "bash" "./all.bash"))
                       (chdir "../..")
                       (for-each mkdir-p (list out-usrgo out-bin))
                       (for-each (lambda (x) (copy-recursively (string-append "go/" x) (string-append out-usrgo "/" x)))
                                 (scandir "go" (lambda (x) (not (or (equal? x "bin")
                                                                    (equal? x "..")
                                                                    (equal? x "."))))))
                       (copy-recursively "go/bin" out-bin)
                       (symlink out-bin (string-append out-usrgo "/bin")))))))
    (native-inputs `(("tar" ,tar)
                     ("gzip" ,gzip)
                     ("bash" ,bash)
                     ("coreutils" ,coreutils)
                     ("grep" ,grep)
                     ("gcc" ,gcc)
                     ("binutils" ,binutils)
                     ("glibc" ,glibc)
                     ("lheaders" ,linux-libre-headers)
                     ("sed" ,sed)))
    (inputs `(("glibc" ,glibc)
              ("gcc-lib" ,gcc "lib")
              ("iana" ,net-base)
              ("tzdata" ,tzdata)))
    (home-page "http://golang.org")
    (synopsis "Go is an open source programming language that makes it easy to
build simple, reliable, and efficient software.")
    (description "Go is expressive, concise, clean, and efficient. Its
concurrency mechanisms make it easy to write programs that get the most out of
multicore and networked machines, while its novel type system enables flexible
and modular program construction. Go compiles quickly to machine code yet has
the convenience of garbage collection and the power of run-time
reflection. It's a fast, statically typed, compiled language that feels like a
dynamically typed, interpreted language.")
  (license bsd-3)))
