(define-module (codemac packages volumeicon)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (codemac packages alsa-plugins))

(define-public volumeicon
  (package
    (name "volumeicon")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://softwarebakery.com/maato/files/"
                           name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "182xl2w8syv6ky2h2bc9imc6ap8pzh0p7rp63hh8nw0xm38c3f14"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--enable-notify")
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'alsa-plugins-hack
          (lambda* (#:key inputs #:allow-other-keys)
            (setenv "ALSA_LDFLAGS" (string-append "-L" (assoc-ref inputs "alsa-lib") "/lib "
                                                 "-L" (assoc-ref inputs "alsa-plugins") "/lib "
                                                 "-lasound"))
            (setenv "ALSA_CFLAGS" (string-append "-I" (assoc-ref inputs "alsa-lib") "/include "
                                                 "-I" (assoc-ref inputs "alsa-lib") "/include/alsa "
                                                 "-I" (assoc-ref inputs "alsa-plugins") "/include "
                                                 "-I" (assoc-ref inputs "alsa-plugins") "/include/alsa ")))))
                         ))
    (native-inputs `(("intltool" ,intltool)
                     ("pkg-config" ,pkg-config)))
    (inputs `(("libnotify" ,libnotify)
              ("alsa-lib" ,alsa-lib)
              ("alsa-plugins" ,alsa-plugins)
              ("gtk+" ,gtk+)))
    (home-page "http://softwarebakery.com/maato/volumeicon.html")
    (synopsis "Volume tray icon")
    (description "volumeicon is a tray icon that can listen to keybindings
and displays the volume status")
    (license gpl3)))
