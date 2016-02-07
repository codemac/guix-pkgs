(define-module (codemac packages git-annex))

(define-public git-annex
  (package
    (name "git-annex")
    (version "6.20160114")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  name "-" version "/" name "-" version ".tar.gz"))
              (sha256
               (base32 "1081a2vn3mw2ilkjnfk7w3yxnm5aqr0hgdk0c2lbk0x5krgic7b7"))))
    (build-system haskell-build-system)
    ;; dependencies from cabal:
    ;; async
    ;; base (>=4.5 && <4.9)
    ;; bloomfilter
    ;; bytestring
    ;; case-insensitive
    ;; containers (>=0.5.0.0)
    ;; data-default
    ;; directory
    ;; dlist
    ;; edit-distance
    ;; esqueleto
    ;; exceptions (>=0.6)
    ;; filepath
    ;; hslogger
    ;; http-client
    ;; http-conduit
    ;; http-types
    ;; IfElse
    ;; json
    ;; MissingH
    ;; monad-control
    ;; monad-logger
    ;; mtl (>=2)
    ;; old-locale
    ;; optparse-applicative (>=0.11.0)
    ;; persistent
    ;; persistent-sqlite
    ;; persistent-template
    ;; process
    ;; QuickCheck (>=2.1)
    ;; random
    ;; resourcet
    ;; SafeSemaphore
    ;; sandi
    ;; stm (>=2.3)
    ;; text
    ;; time
    ;; transformers
    ;; unix-compat
    ;; utf8-string
    ;; uuid
    ;; if flag(network-uri)
    ;;     network (>=2.6)
    ;;     network-uri (>=2.6)
    ;; else
    ;;     network (>=2.0 && <2.6)
    ;; if flag(cryptonite)
    ;;     cryptonite
    ;; else
    ;;     cryptohash (>=0.11.0)
    ;; if os(windows)
    ;;     process (>=1.3.0.0)
    ;;     setenv
    ;;     unix-compat (>=0.4.1.3)
    ;;     Win32
    ;;     Win32-extras
    ;; else
    ;;     unix
    ;; if flag(testsuite)
    ;;     crypto-api
    ;;     tasty (>=0.7)
    ;;     tasty-hunit
    ;;     tasty-quickcheck
    ;;     tasty-rerun
    ;; if flag(tdfa)
    ;;     regex-tdfa
    ;; else
    ;;     regex-compat
    ;; if flag(s3)
    ;;     aws (>=0.9.2)
    ;;     conduit
    ;;     conduit-extra
    ;; if flag(webdav)
    ;;     DAV (>=1.0)
    ;; if flag(assistant)
    ;;     if os(linux)
    ;;         hinotify
    ;;     else
    ;;         if os(osx)
    ;;             hfsevents
    ;;         else
    ;;             if os(windows)
    ;;                 Win32-notify
    ;;             else
    ;;                 if !os(solaris) && !os(linux)
    ;;                     if flag(android)
    ;;                         hinotify
    ;; if os(linux)
    ;;     dbus (>=0.10.7)
    ;;     fdo-notify (>=0.3)
    ;; if flag(android)
    ;;     data-endian
    ;; if flag(webapp)
    ;;     aeson
    ;;     blaze-builder
    ;;     clientsession
    ;;     crypto-api
    ;;     path-pieces (>=0.1.4)
    ;;     shakespeare (>=2.0.0)
    ;;     template-haskell
    ;;     wai
    ;;     wai-extra
    ;;     warp (>=3.0.0.5)
    ;;     warp-tls
    ;;     yesod (>=1.2.6)
    ;;     yesod-core (>=1.2.19)
    ;;     yesod-default (>=1.2.0)
    ;;     yesod-form (>=1.3.15)
    ;;     yesod-static (>=1.2.4)
    ;; if flag(webapp) && flag(webapp-secure)
    ;;     byteable
    ;;     securemem
    ;;     warp-tls (>=1.4)
    ;; if flag(pairing)
    ;;     network-info
    ;;     network-multicast
    ;; if !os(windows)
    ;;     gnutls (>=0.1.4)
    ;;     network-protocol-xmpp
    ;;     xml-types
    ;; if flag(dns)
    ;;     dns
    ;; if flag(feed)
    ;;     feed (>=0.3.4)
    ;; if flag(quvi)
    ;;     aeson
    ;; if flag(tahoe)
    ;;     aeson
    ;; if flag(torrentparser)
    ;;     torrent (>=10000.0.0)
    ;; if flag(concurrentoutput)
    ;;     concurrent-output (>=1.6)
    ;; if flag(ekg)
    ;;     ekg
    ;; if flag(benchmark)
    ;;     criterion
    ;;     deepseq
    
    (inputs `(()))))
