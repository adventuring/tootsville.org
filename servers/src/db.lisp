(in-package :Tootsville)

(defvar *db-secrets* nil)

(defun db-secrets-pathname ()
  (merge-pathnames
   (make-pathname :directory '(:relative ".config" "Tootsville")
                  :name "db-secrets" :type "lisp")
   (user-homedir-pathname)))

(defun connection-settings (&optional (db :maindb))
  (unless *db-secrets*
    (let ((secret-file (db-secrets-pathname)))
      (if (probe-file secret-file)
          (load secret-file)
          (warn "db-secrets not set; database access will probably fail; tried ~s" secret-file))))
  (or (ignore-errors (cdr (assoc db *db-secrets*)))
      (cdr (assoc db (config :databases)))))

(defun db (&optional (db :maindb))
  (apply #'connect-cached (connection-settings db)))

(defmacro with-connection ((conn) &body body)
  `(let ((*connection* ,(if (keywordp conn) `(db ,conn) conn)))
     ,@body))



(defvar *now* nil
  "The time of the current transaction set; consistent through recursive updates")

(defun journal-node (node &optional prior)
  (etypecase node
    (list (datafly:execute (sxql:insert-into :journal-nodes
                             :event-time *now*
                             :node-type "list"
                             :key nil
                             :value nil
                             :prior prior))
     (let ((parent-id (datafly:retrieve-one (sxql:select (:last-insert-id)))))
       (loop for child in node
             with prior-child
             do (journal-node child prior-child)
             do (setq prior-child child))))
    (hash-table (datafly:execute (sxql:insert-into :journal-nodes
                                   :event-time *now*
                                   :node-type "list"
                                   :key nil
                                   :value nil
                                   :prior prior))
     (let ((parent-id (datafly:retrieve-one (sxql:select (:last-insert-id)))))
       (loop for child in (hash-table-keys node)
             with prior-child
             do (journal-node child prior-child)
             do (setq prior-child child))))
    (atom (datafly:execute (sxql:insert-into :journal-nodes
                             :event-time *now*
                             :node-type (type-of node)
                             :key nil
                             :value nil
                             :prior prior)))))

(defun journal (&rest tree)
  (with-connection (:journal)
    (let ((*now* (local-time:timestamp-to-universal (local-time:now))))
      (journal-node tree))))
