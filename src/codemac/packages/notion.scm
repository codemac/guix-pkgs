(define-module (codemac packages notion)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages texlive))


;; depends=('glib2' 'gettext' 'lua' 'libxext' 'libsm')
;; optdepends=('libxinerama' 'libxrandr')
;; makedepends=('git' 'pkgconfig' 'libxinerama' 'libxrandr'
;; 	     'rubber' 'latex2html' 'texlive-htmlxml' 'texlive-latexextra')


(define-public notion
  (package
    (name "notion")
    (version "3.2015061300")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/raboof/" name "/archive/"
                           (string-map
                            (lambda (x) (if (equal? x #\.) #\- x)) version)
                           ".tar.gz"))
       (sha256
        (base32
         "01f3qncgvvy19i94a18lssvhajhsga7y4vxpcg11f3gvm4srz79w"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "PREFIX" (assoc-ref outputs "out"))
             (setenv "CC" "gcc"))))))
    (inputs `(("glib" ,glib)
              ("gnu-gettext" ,gnu-gettext)
              ("lua" ,lua)
              ("libxext" ,libxext)
              ("libsm" ,libsm)
              ("libxinerama" ,libxinerama)
              ("libxrandr" ,libxrandr)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("rubber" ,rubber)
                     ("perl" ,perl)
                     ("which" ,which)
                     ("gcc" ,gcc)
                     ("groff" ,groff)
                     ("texlive-bin" ,texlive-bin)))
    (home-page "http://notion.sourceforge.net")
    (synopsis "Tiling tabbed window manager")
    (description "Notion is a tiled tabbed window manager, which unlike many
other tiled window managers has static window configurations. This allows for
a more declarative approach to window sizing.")
    (license (non-copyleft "file://LICENSE"))))
