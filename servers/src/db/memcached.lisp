(in-package :Tootsville)



(defun query-to-memcache-key (db prepared args)
  (let ((query (format nil "~a➕~{~a~^✜~}" prepared args)))
    (if (< (length query) 128)
        (format nil "~:(~a~):~a" db query)
        (format nil "~:(~a~):~a…#~a"
                db (subseq query 0 100) (sha1-hex query)))))

(defmacro with-memcached-query ((db query args) &body body)
  (let (($db (gensym "DB-"))
        ($query (gensym "QUERY-"))
        ($key (gensym "KEY-"))
        ($value (gensym "VALUE-"))
        ($args (gensym "ARGS-")))
    `(if cl-memcached:*memcache*
         (let* ((,$db ,db) (,$query ,query) (,$args ,args)
                (,$key (query-to-memcache-key ,$db ,$query ,$args)))
           (if-let (,$value (cl-memcached:mc-get-value ,$key))
             (apply #'values (read-from-string (trivial-utf-8:utf-8-bytes-to-string ,$value)))
             (let ((,$value (multiple-value-list (progn ,@body))))
               (cl-memcached:mc-store ,$key (trivial-utf-8:string-to-utf-8-bytes (princ-to-string ,$value)))
               (apply #'values ,$value))))
         (progn ,@body))))



(defun connect-cache ()
  (dolist (server (config :cache))
    (cl-memcached:make-memcache :ip (extract server :ip)
                                :port (or (extract server :port) 11211)
                                :name (extract server :name)))
  (let ((n (princ-to-string (random (expt 2 63))))
        (key (format nil "~a.~a" (machine-instance) (cluster-name))))
    (cl-memcached:mc-set key n)
    (let ((m (cl-memcached:mc-get key)))
      (assert (= n m) ()
              "MemCacheD did not return the random number (~x) for key ~a"
              n key))))
