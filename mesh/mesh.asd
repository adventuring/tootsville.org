:FIXME

              (:module "mesh"
                      :components
                      ((:module "assets"
                                :components
                                ((:file "archives")
                                 (:file "service")
                                 (:file "torrent")))
                       (:file "assets")
                       (:file "babylon")
                       (:file "chatter")
                       (:module "device"
                                :components
                                ((:file "ambient-light")
                                 (:file "network")
                                 (:file "orientation")
                                 (:file "vibration")))
                       (:module "gossip"
                                :components
                                ((:file "events")
                                 (:file "genesis")
                                 (:file "integrity")
                                 (:file "net")))
                       (:file "login")
                       (:file "package")
                       (:module "parrot"
                                :components
                                ((:file "buddy-list")
                                 (:file "child-account")
                                 (:file "child-login")
                                 (:file "new-player")))
                       (:file "parrot")
                       (:module "player-input"
                                :components
                                ((:file "bluetooth-gamepad")
                                 (:file "bluetooth-wiimote")
                                 (:file "events")
                                 (:file "gamepad")
                                 (:file "keyboard")
                                 (:file "lipread")
                                 (:file "listen")
                                 (:file "tabletpad")
                                 (:file "touch")))
                       (:module "Romans"
                                :components
                                ((:file "Aelius-Galenus/Galen")
                                 (:file "Appius-Claudius-Caecus/Appius")
                                 (:file "Clodia-Metelli-Pulcher/Clodia")
                                 (:file "Gaius-Asinius-Pollio/Asinius")
                                 (:file "Gaius-Julius-Caesar/Caesar")
                                 (:file "Gaius-Lutatius-Catulus/Lutatius")
                                 (:file "Lucius-Aemilius-Regillus/Regillus")
                                 (:file "Marcus-Vitruvius-Pollio/Vitruvius")
                                 (:file "Narcissus/Narcissus")
                                 (:file "Publius-Cornelius-Tacitus/Tacitus")
                                 (:file "Rabirius/Rabirius")
                                 (:file "Rahab/Rahab")
                                 (:file "Sextus-Julius-Frontinus/Frontinus")))
                       (:module "ux"
                                :components
                                ((:file "events")
                                 (:file "gossip-mouse")
                                 (:file "make-noise")
                                 (:file "overlay")
                                 (:file "parrot")
                                 (:file "speak")))
                       (:file "ciwta")
                       (:file "webdebug")
                       (:file "webinspect")
                       (:file "webrepl")
                       (:file "world/events")
                       (:file "xhr")))


:FIXME