(define-module (codemac packages youtube-dl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages python))

(define-public youtube-dl
  (package
    (name "youtube-dl")
    (version "2015.12.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://youtube-dl.org/downloads/"
                                  version "/youtube-dl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "13qy35lcfccprn6vhzl3m9a5iix35qi9ila2nx7kvdgxp17l3yfk"))))
    (build-system python-build-system)
    (inputs `(("setuptools" ,python-setuptools)))
    (home-page "http://youtube-dl.org")
    (synopsis "Download videos from YouTube.com and other sites")
    (description
     "Youtube-dl is a small command-line program to download videos from
YouTube.com and a few more sites.")
    (license license:public-domain)))
