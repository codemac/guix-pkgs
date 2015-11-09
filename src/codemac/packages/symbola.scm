(define-module (codemac packages symbola)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages zip))

(define-public font-symbola
  (package
    (name "font-symbola")
    (version "8.00")
    (source
     (origin
       (method url-fetch)
       (uri "http://users.teilar.gr/~g1951d/Symbola.zip")
       (sha256 (base32 "1lfs2j816332ysvpb5ibj2gwpmyqyispqdl7skkshf2gra18hmhd"))
       (file-name (string-append name "-" version ".zip"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules
       ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((PATH     (string-append
                          (assoc-ref %build-inputs "unzip") "/bin:" 
                          (assoc-ref %build-inputs "findutils") "/bin"))
               (font-dir (string-append
                          %output "/share/fonts/truetype")))
           (setenv "PATH" PATH)
           (system* "unzip" (assoc-ref %build-inputs "source"))
           (mkdir-p font-dir)
           (for-each (lambda (ttf)
                       (copy-file ttf
                                  (string-append font-dir "/"
                                                 (basename ttf))))
                     (find-files "." "\\.ttf$"))))))
    
    (native-inputs `(("source" ,source)
                     ("unzip" ,unzip)
                     ("findutils" ,findutils)))
    (home-page "http://users.teilar.gr/~g1951d/")
    (synopsis "Font for unicode symbols (part of Unicode Fonts for Ancient Scripts).")
    (description "💩 will now work")
    (license
     (license:non-copyleft "file://LICENSE"
      "See LICENSE in the distribution."))))
