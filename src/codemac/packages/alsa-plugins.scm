(define-module (codemac packages alsa-plugins)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages video)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages linux))

(define-public alsa-plugins
  (package
    (name "alsa-plugins")
    (version "1.0.29")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "ftp://ftp.alsa-project.org/pub/plugins/" name "-" version ".tar.bz2"))
       (sha256
        (base32 "0ck5xa0vnjhn5w23gf87y30h7bcb6hzsx4817sw35xl5qb58ap9j"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("pulseaudio" ,pulseaudio)
              ("alsa-lib" ,alsa-lib)
              ("ffmped", ffmpeg)
              ("jack" ,jack-1)))
    (synopsis "ALSA plugins")
    (description "Varias ALSA plugins")
    (home-page "http://alsa-project.org")
    (license gpl2)))
