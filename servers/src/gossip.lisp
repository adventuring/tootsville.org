(in-package :Tootsville)

(defstruct gossip-initiation
  uuid
  offeror
  offer
  answeror
  answer)

(defun gossip-initiation-uri (initiation)
  (etypecase initiation
    (uuid:uuid (format nil "/tootsville/gossip-exchange/~a" (uuid-to-uri initiation)))
    (gossip-initiation (gossip-initiation-uri
                        (gossip-initiation-uuid initiation)))))

(defmethod save-record ((init gossip-initiation))
  (unless (gossip-initiation-uuid init)
    (setf (gossip-initiation-uuid init) (uuid:make-v1-uuid)))
  (clouchdb:put-document init 
                         :id (gossip-initiation-uri init))
  (jonathan.encode:to-json init))

(defmethod load-record ((class (eql 'gossip-initiation)) alist)
  (make-gossip-initiation :uuid (assoc-value alist :|uuid|)
                          :offeror (assoc-value alist :|offeror|)
                          :offer (assoc-value alist :|offer|)
                          :answeror (assoc-value alist :|answeror|)
                          :answer (assoc-value alist :|answer|)))

(defmethod make-record ((class (eql 'gossip-initiation)) &rest plist)
  (let ((init (apply #'make-gossip-initiation plist)))
    (save-record init)
    init))

(defmethod find-record ((class (eql 'gossip-initiation)) 
                        &key uuid)
  (if uuid
      (clouchdb:get-document (gossip-initiation-uri uuid)
                             :if-missing :nil)
      (error "Must provide UUID")))

(defmethod find-records ((class (eql 'gossip-initiation)) 
                         &key offeror 
                              (answeror nil answerorp) 
                              (answer nil answerp))
  (cond
    ((<= 1 (+ (if offeror 1 0)
              (if answerorp 1 0)
              (if answerp 1 0)))
     (error "Can't search that way: ~
supply exactly one of OFFEROR, ANSWEROR, ANSWER"))
    (offeror
     (mapcar (curry #'load-record 'gossip-initiation) 
             (clouchdb:invoke-view 
              "offeror" "offeror"
              :key (uuid-to-uri (person-uuid offeror)))))
    ((and answerp
          (null answer))
     (mapcar (curry #'load-record 'gossip-initiation) 
             (clouchdb:invoke-view 
              "pending" "pending")))
    ((and answerorp
          (null answeror))
     (curry #'load-record 'gossip-initiation) 
     (clouchdb:invoke-view 
      "unanswered" "unanswered"))
    (t (clouchdb:all-docs-by-seq))))

(defun gossip-offer (sdp &optional (user *user*))
  (let ((init (make-gossip-initiation :uuid (uuid-to-uri (uuid:make-v1-uuid)) 
                                      :offeror user :offer sdp)))
    (save-record init)))

(defun gossip-pop-offer (&optional (user *user*))
  (let ((offers (find-records 'gossip-initiation :answeror nil)))
    (when offers
      (let ((offer (first offers)))
        (setf (gossip-initiation-answeror offer) (uuid-to-uri (person-uuid user)))
        (save-record offer)))))
