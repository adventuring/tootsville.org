(cl:in-package :cl-user)

(defpackage Tootsville-ASD
  (:use :cl :asdf))
(in-package :Tootsville-ASD)



(defsystem Tootsville
  :version "0.3.17"
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
               :cl-base64
               :cl-dbi
               :cl-memcached
               :cl-ppcre
               :cl-threadpool
               :cljwt-custom
               :clouchdb
               :cxml
               :dbd-mysql
               :drakma
               :envy
               :fare-memoization
               :hunchentoot
               :hunchensocket
               :jonathan
               :pngload
               :swank
               :symbol-munger
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
     (:file "utils" :depends-on ("package-post"))
     (:module "types" :depends-on ("utils")
              :components ((:file "binary")
                           (:file "color+pattern")
                           (:file "date+time")
                           (:file "http-types")
                           (:file "string-characteristics")
                           (:file "uri-types")
                           (:file "toot-names")))
     (:file "config" :depends-on ("package-post" "types"))
     (:file "view" :depends-on ("config"))
     (:file "browser" :depends-on ("config"))
     (:file "users" :depends-on ("utils"))
     (:file "contacts" :depends-on ("utils"))
     (:file "items" :depends-on ("utils"))
     (:file "toots" :depends-on ("utils" "users"))
     (:file "players" :depends-on ("utils" "users"))
     (:file "errors" :depends-on ("package-post"))
     (:file "terrain" :depends-on ("package-post"))
     (:file "version" :depends-on ("package-post" "config"))
     (:file "logging" :depends-on ("package-post" "version"))
     (:file "write-docs" :depends-on ("package-post"))
     (:file "power-on-self-test" :depends-on ("package"))
     (:file "package-post" :depends-on ("power-on-self-test"))
     (:file "command-line" :depends-on ("main" "logging" "write-docs"))
     (:file "endpoint" :depends-on ("package-post"))
     (:file "web"
            :depends-on ("view" "players" "errors" "config" "endpoint"))
     (:file "http-error" :depends-on ("web"))
     (:file "redirect" :depends-on ("web"))
     (:file "ws")
     (:file "gossip" :depends-on ("db"))
     (:file "http-status-messages" :depends-on ("package-post"))
     (:file "acceptor" :depends-on ("types" "endpoint" "web" "auth"
                                            "http-status-messages"))
     (:file "main" :depends-on ("config" "view" "package-post" "acceptor"))
     (:module "db"
              :depends-on ("package-post")
              :components ((:file "memcached")
                           (:file "couch")
                           (:file "maria" :depends-on ("memcached"))
                           (:file "db-central" :depends-on ("maria"))
                           (:file "friendly" :depends-on ("db-central"))))
     (:module "auth"
              :depends-on ("package-post" "users")
              :components
              ((:file "auth-firebase")))
     (:module
      "endpoints"
      :depends-on ("web" "terrain" "db")
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
        ((file "twilio")
         (:module
          "alexa"
          :components
          ((:file "alexa")
           (:file "info" :depends-on ("alexa"))
           (:file "chat" :depends-on ("alexa"))
           (:file "clock" :depends-on ("alexa"))))))))))))
