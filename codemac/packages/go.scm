(define-module (codemac packages go)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages perl)
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
                          (gcclib (string-append (assoc-ref %build-inputs "gcc-lib") "/lib"))
                          (grep (string-append (assoc-ref %build-inputs "grep") "/bin"))
                          (lh (string-append (assoc-ref %build-inputs "lheaders") "/include"))
                          (glibclib (string-append (assoc-ref %build-inputs "glibc") "/lib"))
                          (bu (string-append (assoc-ref %build-inputs "binutils") "/bin"))
                          (iana (string-append (assoc-ref %build-inputs "iana") "/etc"))
                          (sed (string-append (assoc-ref %build-inputs "sed") "/bin/sed"))
                          (sedbin (string-append (assoc-ref %build-inputs "sed") "/bin"))
                          (perl (string-append (assoc-ref %build-inputs "perl") "/bin"))
                          (du (string-append (assoc-ref %build-inputs "diffutils") "/bin"))
                          (tz (string-append (assoc-ref %build-inputs "tzdata") "/share/zoneinfo")))
                     (begin
                       (let ((dapath (fold (lambda (x y) (string-append y ":" x)) "."
                                           (list tar gzip bash cu gcc grep bu perl du sedbin))))
                         (setenv "PATH" dapath)
                         (setenv "LD_LIBRARY_PATH" (string-append gcclib ":" glibclib))
                         (zero? (system* "tar" "xvf" tarballgz))
                         (chdir "go")
                         (substitute* 
                             '("doc/articles/wiki/test.bash"
                               "doc/codewalk/run"
                               "doc/progs/run"
                               "doc/progs/update.bash"
                               "misc/arm/a"
                               "misc/benchcmp"
                               "misc/cgo/testso/test.bash"
                               "misc/makerelease/darwin/scripts/postinstall"
                               "misc/makerelease/darwin/scripts/preinstall"
                               "misc/nacl/go_nacl_386_exec"
                               "misc/nacl/go_nacl_amd64p32_exec"
                               "misc/nacl/go_nacl_arm_exec"
                               "src/all.bash"
                               "src/androidtest.bash"
                               "src/clean.bash"
                               "src/cmd/go/test.bash"
                               "src/make.bash"
                               "src/nacltest.bash"
                               "src/net/http/cgi/testdata/test.cgi"
                               "src/race.bash"
                               "src/regexp/syntax/make_perl_groups.pl"
                               "src/run.bash"
                               "src/runtime/mknacl.sh"
                               "src/sudo.bash"
                               "src/syscall/mkall.sh"
                               "src/syscall/mkerrors.sh"
                               "src/syscall/mksyscall.pl"
                               "src/syscall/mksyscall_solaris.pl"
                               "src/syscall/mksysctl_openbsd.pl"
                               "src/syscall/mksysnum_darwin.pl"
                               "src/syscall/mksysnum_dragonfly.pl"
                               "src/syscall/mksysnum_freebsd.pl"
                               "src/syscall/mksysnum_linux.pl"
                               "src/syscall/mksysnum_netbsd.pl"
                               "src/syscall/mksysnum_openbsd.pl"
                               "test/bench/shootout/timing.sh"
                               "test/errchk"
                               "test/run")
                             (("/bin/bash") (which "bash"))
                             (("/usr/bin/env bash") (which "bash"))
                             (("/usr/bin/env perl") (which "perl"))
                             (("/usr/bin/perl") (which "perl")))

                         (zero? (system* sed "-i" "3i" (string-append "PATH=" dapath " && export PATH=$PATH") "misc/cgo/errors/test.bash"))
                         (zero? (system* sed "-i" "3i" (string-append "PATH=" dapath) "test/bench/shootout/timing.sh"))



                         (chdir "src")
                         ;;   # Disabling the 'os/http/net' tests (they want files not available in
                         ;;   # chroot builds)
                         ;;
                         ;; os/exec/exec_test.go added as their exec.Commnd
                         ;; manually overrides the Environment. Good for unit
                         ;; testing, bad when it assumes a '/bin' with all the
                         ;; things it needs are arround. guix is not unix!
                         (for-each delete-file '("net/multicast_test.go"
                                                 "net/parse_test.go"
                                                 "net/port_test.go"
                                                 "os/exec/exec_test.go"))
                         
                         ;;   # The os test wants to read files in an existing path. Just don't let it be /usr/bin.
                         (substitute* "os/os_test.go"
                           (("/bin/pwd") (which "pwd"))
                           (("/usr/bin") (dirname (which "pwd")))
                           (("TestHostname") "areturn"))

                         ;; get echo calls working
                         ;; (substitute* "os/exec/exec_test.go"
                         ;;   (("echo") (which "echo")))

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


                         (zero? (system* sed "-i" "3iset -x" "cmd/go/test.bash"))
                         ;; exec.Command on os.Args[0] from go run for
                         ;; whatever reason doesn't work right
                         ;; now. libgcc_s.so link missing crap occurs here as
                         ;; well, this may require that 6l-wrapper for go run
                         ;; to work.
                         (substitute* "syscall/syscall_unix_test.go"
                           (("TestPassFD") "areturn"))

                         (substitute* "cmd/6l/asm.c" (("/lib64/ld-linux-x86-64.so.2") (string-append glibclib "/ld-linux-x86-64.so.2")))
                         (zero? (system* sed "-i" "3iset -x" "./make.bash"))
                         (zero? (system* sed "-i" "3iset -x" "./run.bash"))
                         (zero? (system* sed "-i" "3iset -x" "./all.bash"))
                         (zero? (system* "env"
                                         (string-append "PATH=" dapath)
                                         (string-append "GO_LDFLAGS=-r \""gcclib ":" glibclib "\" -extldflags \"-Wl,-rpath," gcclib " -Wl,-rpath," glibclib "\"")
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
                                                                      (equal? x ".")))))))
                       (copy-recursively "go/bin" out-bin)
                       (symlink out-bin (string-append out-usrgo "/bin")))))))
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
                     ("diffutils" ,diffutils)))
    (inputs `(("glibc" ,glibc)
              ("gcc-lib" ,gcc "lib")
              ("gcc" ,gcc)
              ("iana" ,net-base)
              ("ld-wrapper" ,ld-wrapper)
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
