(define-module (codemac systems vm-test)
  #:use-module (gnu))
;(use-modules (gnu))
(use-service-modules networking ssh)
(use-package-modules admin rc)

(operating-system
  (host-name "vm-komp")
  (timezone "America/Los_Angeles")
  (locale "en_US.UTF-8")

  ;; Assuming /dev/sdX is the target hard disk, and "root" is
  ;; the label of the target root file system.
  (bootloader (grub-configuration (device "/dev/sda")))
  (file-systems (cons (file-system
                        (device "root")
                        (title 'label)
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  (users (cons (user-account
                (name "codemac")
                (comment "jeff mickey")
                (group "users")
                (shell rc)
                (supplementary-groups '("wheel"
                                        "audio" "video"))
                (home-directory "/home/codemac"))
               %base-user-accounts))

  (packages (cons* tcpdump rc %base-packages))

  (services (cons* (dhcp-client-service)
                   (lsh-service #:port-number 2222)
                   %base-services)))
