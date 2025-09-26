(in-package :tootsville)

(defparameter |docker-config|
  (list :databases (list
                    (list :cache (list (list :ip "localhost" :name "MemcachedContainer")))
                    (list :friendly :mysql
                          :host "mariadb"
                          :database-name "tootsville"
                          :username "tootsville"
                          :password "tootsville_password"))
        :taskmaster (list :devel t)
        :hunchentoot (list :catch-errors nil
                           :log-warnings t
                           :log-errors t
                           :log-backtraces t
                           :show-errors t
                           :show-backtraces t)
        :websocket-port 5004
        :http-port 5000
        :static-files-path "/app/static/"))

(defparameter |devel| |docker-config|)
(defparameter |test| |docker-config|)