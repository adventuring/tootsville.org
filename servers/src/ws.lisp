(in-package :Tootsville)

(defclass gossip-channel (hunchensocket:websocket-resource)
  ((offer-id :initarg :offer-id
             :initform (uuid:make-v4-uuid)
             :reader gossip-offer-id)))

(defvar *gossip-channels* 
  (queues:make-queue :simple-cqueue :minimum-size 16))

(defun ws-get-gossip-channel (request)
  (queues:qpop *gossip-channels*))

(defun ws-broadcast (channel sender message)
  (dolist (peer (hunchensocket:clients channel))
    (unless (eql peer sender)
      (hunchensocket:send-text-message channel message))))

(defmethod hunchensocket:text-message-received ((channel gossip-channel) 
                                                sender message)
  (ws-broadcast channel sender message))

(defparameter *ws-server*
  (make-instance 'hunchensocket:websocket-acceptor :port :2774))
