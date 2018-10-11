(in-package :Tootsville)


(defun flatten-plist-tree (node &optional (prefix ""))
  (loop for (key value) on node by #'cddr
     for hier-key = (concatenate 'string prefix (string key))
     appending
       (etypecase value
         (cons (flatten-plist-tree value (concatenate 'string hier-key "/")))
         (atom (list (make-keyword hier-key) value)))))

(assert (equalp (flatten-plist-tree '(:a (:b 2 :c (:d 42)) :e 5))
                (list :A/B 2 :A/C/D 42 :E 5)))

(defmethod acceptor-status-message ((acceptor Tootsville-restas-acceptor)
                                    http-status-code &rest properties &key &allow-other-keys)
  "Add interesting values that can be used in templates for variable-substitution"
  (let ((more-properties (append properties
                                 (flatten-plist-tree (version-info-list))
                                 (list :machine-instance (machine-instance)
                                       ))))
    (call-next-method acceptor http-status-code :properties more-properties)))
