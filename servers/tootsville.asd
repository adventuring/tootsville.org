(cl:in-package :cl-user)

(defpackage Tootsville-ASD
  (:use :cl :asdf))
(in-package :Tootsville-ASD)



(defsystem Tootsville
  :version "0.3.9"
  :author "Bruce-Robert Pocock <BRPocock@ciwta.org>"
  :license "AGPL v3+"
  :bug-tracker "https://github.com/adventuring/tootsville.org/issues"
  :description
  "The server software monolith for REST services of Tootsville.org"
  :long-description
  "The  REST  services  for  Tootsville.org, while  running  on  several
hostnames, are  handled from  a proxied HTTP  server. This  provides the
REST services for the front-end."
  :depends-on (

               ;; systems from Quicklisp

               :bordeaux-threads
               :cl-memcached
               :cl-ppcre
               :cl-threadpool
               :clouchdb
               :drakma
               :envy
               :fare-memoization
               :jonathan
               :pngload
               :swank
               :symbol-munger ;; XXX factor out
               :trivial-backtrace
               :trivial-ldap
               :uiop
               :uuid

               ;; Systems that travel bundled with Tootsville

               :dreamhost
               :oliphaunt
               :rollbar
               :thread-pool-taskmaster
               )
  :components
  ((:module
    "src"
    :components
    ((:file "lib/Chœrogryllum/Chœrogryllum")
     (:file "package")
     (:file "utils" :depends-on ("package"))
     (:file "types" :depends-on ("utils"))
     (:file "config" :depends-on ("package" "types"))
     (:file "view" :depends-on ("config"))
     (:file "ldap-player" :depends-on ("package"))
     (:file "users" :depends-on ("utils" "ldap-player"))
     (:file "toots" :depends-on ("utils" "users"))
     (:file "players" :depends-on ("utils" "users"))
     (:file "errors" :depends-on ("package"))
     (:file "terrain" :depends-on ("package"))
     (:file "version" :depends-on ("package" "config"))
     (:file "logging" :depends-on ("package" "version"))
     (:file "write-docs" :depends-on ("package"))
     (:file "power-on-self-test" :depends-on ("web" "endpoints"))
     (:file "command-line" :depends-on ("main" "logging" "write-docs"))
     (:file "endpoint" :depends-on ("package"))
     (:file "web"
            :depends-on ("view" "players" "errors" "config" "endpoint"))
     (:file "http-error" :depends-on ("web"))
     (:file "redirect" :depends-on ("web"))
     (:file "http-status-messages" :depends-on ("package"))
     (:file "acceptor" :depends-on ("types" "endpoint" "web" "auth"
                                            "http-status-messages"))
     (:file "main" :depends-on ("config" "view" "package" "acceptor"))
     (:module "auth"
              :depends-on ("package" "users")
              :components
              ((:file "auth-oauth2")
               (:file "auth-google" :depends-on ("auth-oauth2"))))
     (:module
      "endpoints"
      :depends-on ("web" "terrain")
      :components
      ((:file "slash-login")
       (:file "slash-version")
       (:file "slash-maintenance")
       (:file "slash-meta-game")
       
       (:file "slash-gossip")
       (:file "slash-toots")
       (:file "slash-users")
       (:file "slash-world")
       (:module
        "gossip"
        :depends-on ("slash-gossip")
        :components 
        ((:module
          "alexa"
          :components
          ((:file "alexa")
           (:file "info" :depends-on ("alexa"))
           (:file "chat" :depends-on ("alexa"))
           (:file "clock" :depends-on ("alexa"))))))))))))
