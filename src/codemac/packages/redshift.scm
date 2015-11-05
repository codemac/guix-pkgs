(define-module (codemac packages redshift)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix download))

(define redshift-base-cfgflags
  '("--enable-drm"
    "--enable-randr"
    "--enable-vidmode"
    "--enable-geoclue2"))

(define redshift-no-gui-flags
  (cons "--disable-gui" redshift-base-cfgflags))

(define redshift-gui-flags
  (cons "--enable-gui" redshift-base-cfgflags))

(define-public redshift
  (package
    (name "redshift")
    (version "1.10")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/jonls/" name "/releases/download/v"
                    version "/" name "-" version ".tar.xz"))
              (sha256
               (base32 "19pfk9il5x2g2ivqix4a555psz8mj3m0cvjwnjpjvx0llh5fghjv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags redshift-no-gui-flags))
    (inputs `(("geoclue" ,geoclue)
              ("libdrm" ,libdrm)
              ("libxcb" ,libxcb)
              ("libxxf86vm" ,libxxf86vm)
              ("hicolor-icon-theme" ,hicolor-icon-theme)
              ("librsvg" ,librsvg)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("intltool" ,intltool)))
    (synopsis "Adjusts the color tempurature of your screen according to your surroundings")
    (home-page "http://jonls.dk/redshift/")
    (description "Redshift adjusts the color temperature according to the
position of the sun. A different color temperature is set during night and
daytime. During twilight and early morning, the color temperature transitions
smoothly from night to daytime temperature to allow your eyes to slowly
adapt. At night the color temperature should be set to match the lamps in your
room.")
    (license gpl3+)))

;; This still can't seem to find the freaking icons! so frustrating!
(define-public redshift-gtk
  (package
    (inherit redshift)
    (name "redshift-gtk")
    (arguments
     `(#:configure-flags
       '("--enable-drm"
         "--enable-randr"
         "--enable-vidmode"
         "--enable-geoclue2"
         "--enable-gui")
       #:phases
       (modify-phases
           %standard-phases
         (add-after 'install 'wrap-program
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (prog (string-append out "/bin/redshift-gtk")))
                        (wrap-program prog
                          `("PYTHONPATH" = (,(getenv "PYTHONPATH")))
                          `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))
                          `("XDG_DATA_DIRS" = (,(getenv "XDG_DATA_DIRS"))))
                        (let ((hicolor (assoc-ref inputs "hicolor-icon-theme"))
                              (dir "/share/icons/hicolor/")
                              (name "index.theme"))
                          (install-file (string-append hicolor dir name) (string-append out dir))
                          )
                        #t))))))
    (inputs `(("geoclue" ,geoclue)
              ("libdrm" ,libdrm)
              ("libxcb" ,libxcb)
              ("libxxf86vm" ,libxxf86vm)
              ("hicolor-icon-theme" ,hicolor-icon-theme)
              ("python-pygobject" ,python-pygobject)
              ("python-pyxdg" ,python-pyxdg)
              ("python" ,python-wrapper)
              ("gobject-introspection" ,gobject-introspection)
              ("gtk+" ,gtk+)
              ("librsvg" ,librsvg)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("intltool" ,intltool)))))

