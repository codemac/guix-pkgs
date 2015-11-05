(define-module (codemac packages redshift)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages freedesktop)
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
;; this still fails horribly with errors around the python in python-gtk.

;; notably the current error is:
#|
; /gnu/store/jpmvli29cjiyr4qpjbv2q94il8r6rdc1-redshift-gtk-1.10/bin/redshift-gtk
Traceback (most recent call last):
  File "<frozen importlib._bootstrap>", line 2158, in _find_spec
AttributeError: 'DynamicImporter' object has no attribute 'find_spec'

During handling of the above exception, another exception occurred:

Traceback (most recent call last):
  File "/gnu/store/jpmvli29cjiyr4qpjbv2q94il8r6rdc1-redshift-gtk-1.10/bin/redshift-gtk", line 26, in <module>
    from redshift_gtk.statusicon import run
  File "/gnu/store/jpmvli29cjiyr4qpjbv2q94il8r6rdc1-redshift-gtk-1.10/lib/python3.4/site-packages/redshift_gtk/statusicon.py", line 32, in <module>
    from gi.repository import Gtk, GLib, GObject
  File "/gnu/store/6sbk41a6zrzzzm3r2s1fb75ihc5wnrkj-python-pygobject-3.18.0/lib/python3.4/site-packages/gi/importer.py", line 145, in find_module
    'introspection typelib not found' % namespace)
ImportError: cannot import name Gtk, introspection typelib not found
; 

|#
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
         (add-before 'configure 'replace-pythondir
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (substitute* "src/redshift-gtk/redshift-gtk.in"
                         (("^sys\\.path\\.append\\('\\@pythondir\\@'\\)")
                          (string-append "sys.path.extend(['"
                                         (string-join (cons (string-append (assoc-ref outputs "out")
                                                                           "/lib/python3.4/site-packages")
                                                            (string-split (getenv "PYTHONPATH") #\:)
                                                            ) "', '")
                                         "'])"))))))))
    (inputs `(("geoclue" ,geoclue)
              ("libdrm" ,libdrm)
              ("libxcb" ,libxcb)
              ("libxxf86vm" ,libxxf86vm)
              ("hicolor-icon-theme" ,hicolor-icon-theme)
              ("python-pygobject" ,python-pygobject)
              ("python-pyxdg" ,python-pyxdg)
              ("librsvg" ,librsvg)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("python" ,python)
                     ("intltool" ,intltool)))))
