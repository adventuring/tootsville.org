;;; Utilities and globals
(defmacro -then-> (first &rest then)
  `(funcall (oget (funcall ,first) "then") (lambda () ,then)))

(defmacro callback ((function &rest args) &body (&rest l-list) callback)
  `(,function (lambda (,@l-list) ,@callback) ,@args))

(defmacro callback* ((function &rest args) callback-name)
  `(,function (lambda (&rest callback-args) (apply ',callback-name callback-args))
              ,@args))

(defvar *game-state*
  `( :google-user nil
                  :facebook-user nil
                  :oauth-user nil
                  :location-info nil
                  :player-info nil
                  :overlay-active "game-welcome"
                  :server-info ( :url ,(concatenate 'string
                                                    #j:document:location:protocol
                                                    "//"
                                                    #j:document:location:host
                                                    "/tootsville/action"))))

(defun log (format &rest args)
  (#j:console:log (apply #'format nil format args)))

(defun keywordify (word)
  (make-keyword (string-upcase word)))

(defun init ()
  (log "—~—
Romance Ⅱ Game System
Copyright © 2008-2017 Bruce-Robert Pocock
—~—"))
