(cl:in-package :cl-user)
(load (merge-pathnames
       #p"./src/setup.lisp"
       (make-pathname
        :directory
        (pathname-directory (or *load-pathname*
                                *compile-file-pathname*
                                *default-pathname-defaults*)))))
(defpackage tootsville-asd
  (:use :cl :asdf))
(in-package :tootsville-asd)

(defsystem tootsville
  :version "0.3.4"
  :author "Bruce-Robert Pocock <BRPocock@ciwta.org>"
  :license "AGPL v3+"
  :bug-tracker "https://github.com/ciwta/tootsville.org/issues"
  :description
  "The server software monolith for REST services of Tootsville.org"
  :long-description
  "The REST services for Tootsville.org, while running on several
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
               )
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "machine")
     (:file "taskmaster" :depends-on ("machine" "config"))
     (:file "main" :depends-on ("config" "view" "db" "web" "package" "taskmaster"))
     (:file "db-player" :depends-on ("db" "package"))
             (:file "web" :depends-on ("config" "view" "db-player" "errors" "package"))

     (:file "login" :depends-on ("web"))
             (:file "errors" :depends-on ("config" "package"))
     (:file "version" :depends-on ("web"))
     (:file "redirect" :depends-on ("web" "version"))
     (:file "maintenance" :depends-on ("web"))
     (:file "meta-game" :depends-on ("web"))

     (:file "gossip" :depends-on ("web"))
     (:file "users" :depends-on ("web"))
     (:file "world" :depends-on ("web"))

     (:file "view" :depends-on ("config"))
     (:file "db" :depends-on ("config"))
     (:file "config" :depends-on ("package"))
     #+jscl
     (:module "mesh"
      :components
      ((:file "assets/archives" :depends-on ("package"))
       (:file "assets" :depends-on ("package"))
       (:file "assets/service" :depends-on ("package"))
       (:file "assets/torrent" :depends-on ("package"))
       (:file "babylon" :depends-on ("package"))
       (:file "chatter" :depends-on ("package"))
       (:file "device/ambient-light" :depends-on ("package"))
       (:file "device/network" :depends-on ("package"))
       (:file "device/orientation" :depends-on ("package"))
       (:file "device/vibration" :depends-on ("package"))
       (:file "gossipnet/events" :depends-on ("package"))
       (:file "gossipnet/genesis" :depends-on ("package"))
       (:file "gossipnet/integrity" :depends-on ("package"))
       (:file "gossipnet/net" :depends-on ("package"))
       (:file "login" :depends-on ("package"))
       (:file "package")
       (:file "parrot/buddy-list" :depends-on ("package"))
       (:file "parrot/child-account" :depends-on ("package"))
       (:file "parrot/child-login" :depends-on ("package"))
       (:file "parrot" :depends-on ("package"))
       (:file "parrot/new-player" :depends-on ("package"))
       (:file "player-input/bluetooth-gamepad" :depends-on ("package"))
       (:file "player-input/bluetooth-wiimote" :depends-on ("package"))
       (:file "player-input/events" :depends-on ("package"))
       (:file "player-input/gamepad" :depends-on ("package"))
       (:file "player-input/keyboard" :depends-on ("package"))
       (:file "player-input/lipread" :depends-on ("package"))
       (:file "player-input/listen" :depends-on ("package"))
       (:file "player-input/tabletpad" :depends-on ("package"))
       (:file "player-input/touch" :depends-on ("package"))
       (:file "romans/Aelius-Galenus/galen" :depends-on ("package"))
       (:file "romans/Appius-Claudius-Caecus/appius" :depends-on ("package"))
       (:file "romans/Clodia-Metelli-Pulcher/clodia" :depends-on ("package"))
       (:file "romans/Gaius-Asinius-Pollio/asinius" :depends-on ("package"))
       (:file "romans/Gaius-Julius-Caesar/caesar" :depends-on ("package"))
       (:file "romans/Gaius-Lutatius-Catulus/lutatius" :depends-on ("package"))
       (:file "romans/Lucius-Aemilius-Regillus/regillus" :depends-on ("package"))
       (:file "romans/Marcus-Vitruvius-Pollio/vitruvius" :depends-on ("package"))
       (:file "romans/Narcissus/narcissus" :depends-on ("package"))
       (:file "romans/Publius-Cornelius-Tacitus/tacitus" :depends-on ("package"))
       (:file "romans/Rabirius/rabirius" :depends-on ("package"))
       (:file "romans/Rahab/rahab" :depends-on ("package"))
       (:file "romans/Sextus-Julius-Frontinus/frontinus" :depends-on ("package"))
       (:file "ux/events" :depends-on ("package"))
       (:file "ux/gossip-mouse" :depends-on ("package"))
       (:file "ux/make-noise" :depends-on ("package"))
       (:file "ux/overlay" :depends-on ("package"))
       (:file "ux/parrot" :depends-on ("package"))
       (:file "ux/speak" :depends-on ("package"))
       (:file "ciwta" :depends-on ("package"))
       (:file "webdebug" :depends-on ("package"))
       (:file "webinspect" :depends-on ("package"))
       (:file "webrepl" :depends-on ("package"))
       (:file "world/events" :depends-on ("package"))
       (:file "xhr" :depends-on ("package")))))))
  :description "tootsville"
  :in-order-to ((test-op (load-op tootsville-test))))
