;; alsa-lib
;; dbus-glib
;; desktop-file-utils
;; gtk2
;; hicolor-icon-theme
;; hunspell
;; icu
;; libevent
;; libvpx
;; libxt
;; mime-types
;; mozilla-common
;; nss
;; sqlite
;; startup-notification 









(define-module (codemac packages firefox)
  )






;; . $topsrcdir/browser/config/mozconfig

;; ac_add_options --prefix=/usr
;; ac_add_options --libdir=/usr/lib
;; ac_add_options --enable-pie

;; ac_add_options --enable-official-branding

;; # System libraries
;; ac_add_options --with-system-nspr
;; ac_add_options --with-system-nss
;; ac_add_options --with-system-jpeg
;; ac_add_options --with-system-zlib
;; ac_add_options --with-system-bz2
;; ac_add_options --with-system-png
;; ac_add_options --with-system-libevent
;; ac_add_options --with-system-libvpx
;; ac_add_options --with-system-icu
;; ac_add_options --enable-system-hunspell
;; ac_add_options --enable-system-sqlite
;; ac_add_options --enable-system-ffi
;; #ac_add_options --enable-system-cairo
;; ac_add_options --enable-system-pixman

;; # Features
;; ac_add_options --enable-startup-notification
;; ac_add_options --enable-pulseaudio
;; ac_add_options --enable-gstreamer=1.0

;; ac_add_options --disable-crashreporter
;; ac_add_options --disable-updater
;; ac_add_options --disable-installer
;; ac_add_options --disable-debug-symbols
(define (mozconfig)

  (let ((mozconf-start
         '(". $topsrcdir/browser/config/mozconfig"

           "ac_add_options --prefix=/usr"
           "ac_add_options --libdir=/usr/lib"
           "ac_add_options --enable-pie"

           "ac_add_options --enable-official-branding"
           "ac_add_options --with-system-nspr"
           "ac_add_options --with-system-nss"
           "ac_add_options --with-system-jpeg"
           "ac_add_options --with-system-zlib"
           "ac_add_options --with-system-bz2"
           "ac_add_options --with-system-png"
           "ac_add_options --with-system-libevent"
           "ac_add_options --with-system-libvpx"
           "ac_add_options --with-system-icu"
           "ac_add_options --enable-system-hunspell"
           "ac_add_options --enable-system-sqlite"
           "ac_add_options --enable-system-ffi"
           ;;"#ac_add_options --enable-system-cairo"
           "ac_add_options --enable-system-pixman"

           "ac_add_options --enable-startup-notification"
           "ac_add_options --enable-pulseaudio"
           "ac_add_options --enable-gstreamer=1.0"

           "ac_add_options --disable-crashreporter"
           "ac_add_options --disable-updater"
           "ac_add_options --disable-installer"
           "ac_add_options --disable-debug-symbols")))
    (fold (lambda (x) (string-append x "\n")) mozconf-start)
    ))



;; // Use LANG environment variable to choose locale
;; pref("intl.locale.matchOS", true);

;; // Disable default browser checking.
;; pref("browser.shell.checkDefaultBrowser", false);

;; // Don't disable our bundled extensions in the application directory
;; pref("extensions.autoDisableScopes", 11);
;; pref("extensions.shownSelectionUI", true);

(define (vendorjs)
  '(("intl.locale.matchOS" "true")
    ("browser.shell.checkDefaultBrowser" "false")
    ("extensions.autoDisableScopes" "11")
    ("extensions.shownSelectionUI" "true")))
