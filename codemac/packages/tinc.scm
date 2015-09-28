;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Jeff Mickey <j@codemac.net>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (codemac packages tinc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tls))

(define-public tinc
  (package
   (name "tinc")
   (version "1.0.26")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://tinc-vpn.org/packages/"
                                name "-" version ".tar.gz"))
            (sha256
             (base32
              "08ds8s32cjslms1q227ihd6jz35583v378ij4pknfa5xngfijhrb"))))
   (build-system gnu-build-system)
   (inputs `(("zlib" ,zlib)
             ("lzo" ,lzo)
             ("openssl" ,openssl)))
   (home-page "http://tinc-vpn.org")
   (synopsis "A Virtual Private Network (VPN) daemon")
   (description
    "Tinc is a VPN that uses tunnelling and encryption to create a secure
private network between hosts on the internet.")
   (license license:gpl2+)))
