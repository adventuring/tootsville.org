(cl:in-package :cl-user)

(defpackage Tootsville-ASD
  (:use :cl :asdf))
(in-package :Tootsville-ASD)

(defvar *setup* nil)

(unless *setup*
  (load (merge-pathnames
         #p"./src/setup.lisp"
         (make-pathname
          :directory
          (pathname-directory (or *load-pathname*
                                  *compile-file-pathname*
                                  *default-pathname-defaults*))))))



(defsystem Tootsville
  :version "0.3.5"
  :author "Bruce-Robert Pocock <BRPocock@ciwta.org>"
  :license "AGPL v3+"
  :bug-tracker "https://github.com/adventuring/Tootsville.org/issues"
  :description
  "The server software monolith for REST services of Tootsville.org"
  :long-description
  "The  REST  services  for  Tootsville.org, while  running  on  several
hostnames, are  handled from  a proxied HTTP  server. This  provides the
REST services for the front-end."
  :depends-on (
               :bordeaux-threads
               :cl-ppcre
               :cl-threadpool
               :datafly
               :drakma
               :envy
               :memoize
               :restas
               :swank
               :sxql
               :trivial-backtrace
               :uiop
               :uuid
               :yason

               :oliphaunt
               :rollbar
               :thread-pool-taskmaster
               
               )
  :components
  ((:module "src"
            :components
            #+sbcl
            ((:file "package" :depends-on ("utils"))
             (:file "machine" :depends-on ("utils"))
             (:file "utils")
             (:file "main" :depends-on ("config" "view" "db" "web" "package"))
             (:file "db-player" :depends-on ("db" "package"))
             (:file "web"
                    :depends-on ("view" "db-player" "errors" "config"))
             
             (:file "errors")
             
             (:module "endpoints"
                      :depends-on ("web")
                      :components
                      ((:file "login")
                       (:file "version")
                       (:file "redirect")
                       (:file "maintenance")
                       (:file "meta-game")
                       
                       (:file "gossip")
                       (:file "users")
                       (:file "world")))
             
             (:file "view" :depends-on ("config"))
             (:file "db" :depends-on ("config"))
             (:file "config" :depends-on ("package"))
             #+jscl
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
                       (:file "xhr"))))))
  :in-order-to ((test-op (load-op Tootsville-test))))
