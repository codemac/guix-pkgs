(use-modules (gnu))
(use-service-modules networking ssh)
(use-package-modules admin rc certs)

(operating-system
  (host-name "usbboot")
  (timezone "Americas/Los_Angeles")
  (locale "en_US.UTF-8")

  (bootloader (grub-configuration (device "/dev/sdd")))
  
  (file-systems (cons (file-system
                        (device "root")
                        (title 'label)
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  (users (cons (user-account
                (name "codemac")
                (comment "the mac of the codes")
                (group "users")
                (home-directory "/home/codemac")
                (supplementary-groups
                 '("wheel"
                   "audio"
                   "video")))
               %base-user-accounts))

  (packages (cons* rc nss-certs %base-packages))

  (services (cons* (dhcp-client-service)
                   %base-services))

  (name-service-switch %mdns-host-lookup-nss))
