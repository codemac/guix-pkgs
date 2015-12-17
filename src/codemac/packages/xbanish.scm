(define-module (codemac packages xbanish)
  #:use-module (gnu packages)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public xbanish
  (package
    (name "xbanish")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jcs/" name "/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0r4c5pl1b00gfqlg94yjmd434m99mi8k2sivznyy05925kzd346z"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'setcc
           (lambda _
             (setenv "INSTALL_PROGRAM" "mkdir -p $(PREFIX)/bin && install -s")
             (setenv "PREFIX" (assoc-ref %outputs "out"))
             (setenv "CC" "gcc"))))))
    (native-inputs `(("libxt" ,libxt)))
    (inputs `(("libxfixes" ,libxfixes)
              ("libxi" ,libxi)))
    (home-page "http://github.com/jcs/xbanish")
    (synopsis "xbanish hides the mouse cursor when you start typing.")
    (description "xbanish hides the mouse cursor when you start typing, and shows it again when
the mouse cursor moves or a mouse button is pressed.  This is similar to
xterm's pointerMode setting, but xbanish works globally in the X11 session.

unclutter's -keystroke mode is supposed to do this, but it's broken[0].  I
looked into fixing it, but the unclutter source code is terrible, so I wrote
xbanish.

The name comes from ratpoison's \"banish\" command that sends the cursor to the
corner of the screen.")
    (license bsd-3)))
