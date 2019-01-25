;;;; -*- lisp -*-
;;;
;;;; ./servers/src/items.lisp is part of Tootsville
;;;
;;;; Copyright  ©   2016,2017  Bruce-Robert  Pocock;  ©   2018,2019  The
;;;; Corporation for Inter-World Tourism and Adventuring (ciwta.org).
;;;
;;;; This  program is  Free  Software: you  can  redistribute it  and/or
;;;; modify it under the terms of  the GNU Affero General Public License
;;;; as published by  the Free Software Foundation; either  version 3 of
;;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the  hope that it will be useful, but
;;; WITHOUT  ANY   WARRANTY;  without  even  the   implied  warranty  of
;;; MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE. See  the GNU
;;; Affero General Public License for more details.
;;;
;;; You should  have received a  copy of  the GNU Affero  General Public
;;; License    along     with    this     program.    If     not,    see
;;; <https://www.gnu.org/licenses/>.
;;;
;;; You can reach CIWTA at https://ciwta.org/, or write to us at:
;;;
;;; PO Box 23095
;;;; Oakland Park, FL 33307-3095
;;; USA

(in-package :Tootsville)




(defun create-item (template-id)
  "Create an item as an instance of the given TEMPLATE-ID."
  (let ((template (find-record 'item-template :id template-id)))
    (make-record 'item
                 :template template-id
                 :avatar-scale-x (item-template-avatar-scale-x template)
                 :avatar-scale-y (item-template-avatar-scale-y template)
                 :avatar-scale-z (item-template-avatar-scale-z template)
                 :x 0 :y 0 :z 0
                 :latitude 0 :longitude 0 :altitude -1000
                 :world :chor)))

(defun grant-item (template-id recipient)
  "Create a new instance of TEMPLATE-ID and give it to RECIPIENT."
  (let ((item (create-item template-id))
        (player-uuid (etypecase recipient
                       (Toot (Toot-player recipient))
                       (string (Toot-player (find-record 'Toot :name recipient)))
                       (person recipient)))
        (Toot  (etypecase recipient
                 (Toot recipient)
                 (string (find-record 'Toot :name recipient))
                 (person nil))))
    (player-alert player-uuid :inventory :get item)
    (make-record 'inventory-item
                 :item (item-uuid item)
                 :person player-uuid
                 :Toot (Toot-uuid Toot)
                 :equipped :N)))

(defun gift-item (item giver recipient)
  "Transfer the ownership of ITEM from GIVER to RECIPIENT."
  (when *user*
    (unless (eql *user* giver)
      (error 'not-allowed)))
  (let ((giver-player (etypecase recipient
                        (Toot (Toot-player recipient))
                        (string (Toot-player (find-record 'Toot :name recipient)))
                        (person recipient)))
        (giver-Toot  (etypecase recipient
                       (Toot recipient)
                       (string (find-record 'Toot :name recipient))
                       (person nil)))
        (recipient-player (etypecase recipient
                            (Toot (Toot-player recipient))
                            (string (Toot-player (find-record 'Toot :name recipient)))
                            (person recipient)))
        (recipient-Toot  (etypecase recipient
                           (Toot recipient)
                           (string (find-record 'Toot :name recipient))
                           (person nil))))
    (player-alert recipient-player :inventory :get item)
    (player-alert giver-player :inventory :drop item)
    (let ((inventory (find-record 'inventory
                                  :item (item-uuid item)
                                  :person (person-uuid giver-player)
                                  :Toot (Toot-uuid giver-Toot))))
      (setf (inventory-item-equipped inventory) :N
            (inventory-item-person inventory) (person-uuid recipient-player)
            (inventory-item-Toot inventory) (toot-uuid recipient-Toot))
      inventory)))

(defun vanish-item (item)
  "ITEM ceases to exist."
  (v:info '(:vanish :item) "VANISH item ~a" item)
  (destroy-record item))



(defun item-lose-energy (item amount)
  "Decrease the energy of ITEM by AMOUNT (stopping at zero).

If the item's  energy reaches zero, the effect of  its :On-Zero flag will
occur; either it will remain :EMPTY, or :VANISH.

If ITEM's Energy-Kind is :COUNTABLE, then AMOUNT must be an integer."
  (when (eql :countable (item-template-energy-kind (item-template item)))
    (assert (integerp amount)))
  (cond
    ((> (item-energy item) amount)
     (decf (item-energy item) amount))
    (t 
     (setf (item-energy item) 0)
     (ecase (item-template-on-zero (item-template item))
       (:vanish (progn 
                  (vanish-item item)
                  (return-from item-lose-energy)))
       ;; TODO ... on-zero cases
       )))
  (save-record item))

(defun item-gain-energy (item amount)
  "Increate the energy of ITEM by AMOUNT (stopping at its :Energy-Max).

If ITEM's Energy-Kind is :COUNTABLE, then AMOUNT must be an integer."
  (when (eql :countable (item-template-energy-kind (item-template item)))
    (assert (integerp amount)))
  (incf (item-energy item) amount)
  (save-record item))



(defun don-item (item slot)
  "Equip ITEM on its owning Toot in SLOT.

If this conflicts with any other equipped items, remove them.")

(defun doff-item (item)
  "Un-equip ITEM.")

(defun drop-item (item)
  "Drop ITEM and cease to own it.")

(defun take-item (item recipient)
  "RECIPIENT becomes the new owner of ITEM.

The RECIPIENT Toot must  be close enough to pick up  ITEM, and ITEM must
be in the world, and not owned by any other player."
  (when *user*
    (unless (eql *user* recipient)
      (error 'not-allowed))))
