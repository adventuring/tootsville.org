;;; JSCLBindings.lisp - JSCL Lisp bindings for Tootsville client
;;; Provides native Lisp interface to JavaScript client objects
;;;
;;; Copyright © 2025 Interworldly Adventuring, LLC.
;;; This program is Free Software; Refer to COPYING.AGPL for details.
;;; Author: Interworldly Adventuring, LLC
;;; Version: 1.0.0

(in-package :cl-user)

;;; @section JSCL Bindings Overview
;;; 
;;; This module provides native Common Lisp bindings to the Tootsville
;;; client-side JavaScript objects. It allows Lisp code to interact
;;; with the game world, avatars, items, and communication system
;;; using familiar Lisp syntax and conventions.
;;;
;;; @subsection Usage Examples
;;; 
;;; @example
;;; ;; Initialize bindings
;;; (defvar *tootsville* (make-tootsville-bindings))
;;; 
;;; ;; Move avatar
;;; (tootsville-move-avatar *tootsville* 100 50 200)
;;; 
;;; ;; Send message
;;; (tootsville-send-message *tootsville* "Hello, world!" :public)
;;; @end example

(defclass tootsville-bindings ()
  ((js-bindings :accessor js-bindings
                :initform nil
                :documentation "Reference to JavaScript ClientBindings object")
   (event-handlers :accessor event-handlers
                  :initform (make-hash-table :test 'equal)
                  :documentation "Registered event handlers"))
  (:documentation "Lisp wrapper for Tootsville client bindings"))

(defun make-tootsville-bindings (&optional js-bindings)
  "Create a new Tootsville bindings instance.
   Creates a Lisp wrapper around the JavaScript ClientBindings object.
   If no JavaScript bindings are provided, attempts to access the
   global TootsvilleBindings object.
   @param js-bindings - Optional JavaScript bindings object
   @return tootsville-bindings - The created bindings instance"
  (let ((bindings (make-instance 'tootsville-bindings)))
    (setf (js-bindings bindings)
          (or js-bindings
              (jscl::oget (jscl::oget window "TootsvilleBindings") "default")))
    bindings))

(defun tootsville-connected-p (bindings)
  "Check if connected to server.
   @param bindings tootsville-bindings - The bindings instance
   @return boolean - True if connected, false otherwise"
  (jscl::oget (js-bindings bindings) "communication" "isConnected"))

(defun tootsville-connect (bindings &optional (cluster "test"))
  "Connect to Tootsville server.
   @param bindings tootsville-bindings - The bindings instance
   @param cluster string - The server cluster to connect to (default: 'test')"
  (jscl::funcall (jscl::oget (js-bindings bindings) "communication" "connect") cluster))

(defun tootsville-disconnect (bindings)
  "Disconnect from server.
   @param bindings tootsville-bindings - The bindings instance"
  (jscl::funcall (jscl::oget (js-bindings bindings) "communication" "disconnect")))

(defun tootsville-move-avatar (bindings x y z)
  "Move avatar to specified coordinates.
   @param bindings tootsville-bindings - The bindings instance
   @param x number - X coordinate
   @param y number - Y coordinate
   @param z number - Z coordinate"
  (jscl::funcall (jscl::oget (js-bindings bindings) "game" "moveAvatar") x y z))

(defun tootsville-teleport (bindings world x y z)
  "Teleport avatar to different world and coordinates.
   @param bindings tootsville-bindings - The bindings instance
   @param world string - Target world name
   @param x number - X coordinate
   @param y number - Y coordinate
   @param z number - Z coordinate"
  (jscl::funcall (jscl::oget (js-bindings bindings) "game" "teleport") world x y z))

(defun tootsville-send-message (bindings message &optional (type "public"))
  "Send chat message.
   @param bindings tootsville-bindings - The bindings instance
   @param message string - The message to send
   @param type string - Message type (default: 'public')"
  (jscl::funcall (jscl::oget (js-bindings bindings) "game" "sendMessage") message type))

(defun tootsville-use-item (bindings item-id target)
  "Use item from inventory.
   @param bindings tootsville-bindings - The bindings instance
   @param item-id string - The item ID to use
   @param target object - The target object or position"
  (jscl::funcall (jscl::oget (js-bindings bindings) "game" "useItem") item-id target))

(defun tootsville-get-inventory (bindings)
  "Get current inventory contents.
   @param bindings tootsville-bindings - The bindings instance
   @return list - Inventory items"
  (jscl::funcall (jscl::oget (js-bindings bindings) "game" "getInventory")))

(defun tootsville-get-position (bindings)
  "Get current avatar position.
   @param bindings tootsville-bindings - The bindings instance
   @return list - Position as (:x :y :z :world)"
  (let ((pos (jscl::funcall (jscl::oget (js-bindings bindings) "game" "getPosition"))))
    (list :x (jscl::oget pos "x")
          :y (jscl::oget pos "y")
          :z (jscl::oget pos "z")
          :world (jscl::oget pos "world"))))

(defun tootsville-get-nearby-avatars (bindings)
  "Get list of nearby avatars.
   @param bindings tootsville-bindings - The bindings instance
   @return list - List of nearby avatar objects"
  (jscl::funcall (jscl::oget (js-bindings bindings) "game" "getNearbyAvatars")))

(defun tootsville-calculate-distance (bindings pos1 pos2)
  "Calculate distance between two positions.
   @param bindings tootsville-bindings - The bindings instance
   @param pos1 list - First position (:x :y :z)
   @param pos2 list - Second position (:x :y :z)
   @return number - Distance between positions"
  (jscl::funcall (jscl::oget (js-bindings bindings) "utils" "calculateDistance") pos1 pos2))

(defun tootsville-world-to-screen (bindings world-pos)
  "Convert world coordinates to screen coordinates.
   @param bindings tootsville-bindings - The bindings instance
   @param world-pos list - World position (:x :y :z)
   @return list - Screen coordinates (:x :y)"
  (jscl::funcall (jscl::oget (js-bindings bindings) "utils" "worldToScreen") world-pos))

(defun tootsville-screen-to-world (bindings screen-pos)
  "Convert screen coordinates to world coordinates.
   @param bindings tootsville-bindings - The bindings instance
   @param screen-pos list - Screen position (:x :y)
   @return list - World coordinates (:x :y :z)"
  (jscl::funcall (jscl::oget (js-bindings bindings) "utils" "screenToWorld") screen-pos))

(defun tootsville-format-time (bindings timestamp)
  "Format timestamp for display.
   @param bindings tootsville-bindings - The bindings instance
   @param timestamp number - Unix timestamp
   @return string - Formatted time string"
  (jscl::funcall (jscl::oget (js-bindings bindings) "utils" "formatTime") timestamp))

(defun tootsville-on (bindings event handler)
  "Register event handler.
   @param bindings tootsville-bindings - The bindings instance
   @param event string - Event name to listen for
   @param handler function - Function to call when event occurs"
  (setf (gethash event (event-handlers bindings)) handler)
  (jscl::funcall (jscl::oget (js-bindings bindings) "communication" "on") 
                 event 
                 (lambda (data)
                   (funcall handler data))))

(defun tootsville-off (bindings event)
  "Remove event handler.
   @param bindings tootsville-bindings - The bindings instance
   @param event string - Event name to stop listening for"
  (remhash event (event-handlers bindings))
  (jscl::funcall (jscl::oget (js-bindings bindings) "communication" "off") event))

(defun tootsville-send-command (bindings command data)
  "Send raw command to server.
   @param bindings tootsville-bindings - The bindings instance
   @param command string - Command name
   @param data object - Command data payload"
  (jscl::funcall (jscl::oget (js-bindings bindings) "communication" "send") command data))

(defun tootsville-get-game-world (bindings)
  "Get game world component.
   @param bindings tootsville-bindings - The bindings instance
   @return object - Game world component"
  (jscl::funcall (jscl::oget (js-bindings bindings) "components" "getGameWorld")))

(defun tootsville-get-avatar-manager (bindings)
  "Get avatar manager component.
   @param bindings tootsville-bindings - The bindings instance
   @return object - Avatar manager component"
  (jscl::funcall (jscl::oget (js-bindings bindings) "components" "getAvatarManager")))

(defun tootsville-get-world-terrain (bindings)
  "Get world terrain component.
   @param bindings tootsville-bindings - The bindings instance
   @return object - World terrain component"
  (jscl::funcall (jscl::oget (js-bindings bindings) "components" "getWorldTerrain")))

(defun tootsville-get-item-manager (bindings)
  "Get item manager component.
   @param bindings tootsville-bindings - The bindings instance
   @return object - Item manager component"
  (jscl::funcall (jscl::oget (js-bindings bindings) "components" "getItemManager")))

(defun tootsville-get-weather-system (bindings)
  "Get weather system component.
   @param bindings tootsville-bindings - The bindings instance
   @return object - Weather system component"
  (jscl::funcall (jscl::oget (js-bindings bindings) "components" "getWeatherSystem")))

(defun tootsville-destroy (bindings)
  "Clean up bindings and event handlers.
   @param bindings tootsville-bindings - The bindings instance"
  (clrhash (event-handlers bindings))
  (jscl::funcall (jscl::oget (js-bindings bindings) "destroy")))

;;; @section Convenience Macros
;;;
;;; These macros provide convenient ways to interact with the Tootsville
;;; bindings using more idiomatic Lisp syntax.

(defmacro with-tootsville-bindings ((var &optional js-bindings) &body body)
  "Execute body with Tootsville bindings, automatically cleaning up.
   @param var symbol - Variable to bind the Tootsville bindings to
   @param js-bindings - Optional JavaScript bindings object
   @param body - Forms to execute with the bindings"
  `(let ((,var (make-tootsville-bindings ,js-bindings)))
     (unwind-protect
         (progn ,@body)
       (tootsville-destroy ,var))))

(defmacro tootsville-with-connection ((bindings cluster) &body body)
  "Execute body with active server connection.
   @param bindings symbol - Variable containing the Tootsville bindings
   @param cluster string - Server cluster to connect to
   @param body - Forms to execute while connected"
  `(progn
     (tootsville-connect ,bindings ,cluster)
     (unwind-protect
         (progn ,@body)
       (tootsville-disconnect ,bindings))))

;;; @section Event Handling Examples
;;;
;;; @example
;;; ;; Set up event handlers
;;; (tootsville-on *tootsville* "avatarUpdate"
;;;   (lambda (data)
;;;     (format t "Avatar updated: ~A~%" data)))
;;; 
;;; (tootsville-on *tootsville* "message"
;;;   (lambda (data)
;;;     (format t "~A: ~A~%" 
;;;             (gethash "sender" data)
;;;             (gethash "message" data))))
;;; @end example

;;; @section Advanced Usage
;;;
;;; For advanced usage, you can access the underlying JavaScript objects
;;; directly through the js-bindings slot:
;;;
;;; @example
;;; ;; Access JavaScript object directly
;;; (let ((js-obj (js-bindings *tootsville*)))
;;;   (jscl::funcall (jscl::oget js-obj "game" "moveAvatar") 100 50 200))
;;; @end example

;;; Export all public functions
(export '(make-tootsville-bindings
          tootsville-connected-p
          tootsville-connect
          tootsville-disconnect
          tootsville-move-avatar
          tootsville-teleport
          tootsville-send-message
          tootsville-use-item
          tootsville-get-inventory
          tootsville-get-position
          tootsville-get-nearby-avatars
          tootsville-calculate-distance
          tootsville-world-to-screen
          tootsville-screen-to-world
          tootsville-format-time
          tootsville-on
          tootsville-off
          tootsville-send-command
          tootsville-get-game-world
          tootsville-get-avatar-manager
          tootsville-get-world-terrain
          tootsville-get-item-manager
          tootsville-get-weather-system
          tootsville-destroy
          with-tootsville-bindings
          tootsville-with-connection))
