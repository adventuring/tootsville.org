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



(defsystem tootsville
    :version "0.3.5"
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
                 :bordeaux-threads
                 :cl-ppcre
                 :cl-threadpool
                 :datafly
                 :drakma
                 :envy
                 :fare-memoization
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
              ((:file "utils")
               (:file "package" :depends-on ("utils"))
               (:file "config" :depends-on ("package"))
               (:file "view" :depends-on ("config"))
               (:file "db" :depends-on ("config"))
               (:file "users" :depends-on ("utils" "db"))
               (:file "types" :depends-on ("utils"))
               (:file "machine" :depends-on ("utils"))
               (:file "errors" :depends-on ("package"))
               (:file "logging" :depends-on ("package" "version"))
               (:file "write-docs" :depends-on ("package"))
               (:file "power-on-self-test" :depends-on ("web" "endpoints"))
               (:file "version" :depends-on ("package" "config"))
               (:file "command-line" :depends-on ("main" "logging" "write-docs"))
               (:file "db-player" :depends-on ("db" "package"))
               (:file "web"
                      :depends-on ("view" "db-player" "errors" "config"))
               (:file "http-error" :depends-on ("web"))

               (:file "redirect" :depends-on ("web"))
               (:file "main" :depends-on ("config" "view" "db" "web" "package"))
               (:module "endpoints"
                        :depends-on ("web")
                        :components
                        ((:file "login")
                         (:file "version")
                         (:file "maintenance")
                         (:file "meta-game")
                         
                         (:file "gossip")
                         (:file "users")
                         (:file "world")))))))
             
