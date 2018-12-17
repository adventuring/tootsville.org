(in-package :Tootsville)




(defun create-item (template-id)
  "Create an item as an instance of the given TEMPLATE-ID."
  (let ((template (find-record 'db.item-template :id template-id)))
    (make-record 'db.item 
                 :template template-id
                 :avatar-scale-x (db.item-template-avatar-scale-x template)
                 :avatar-scale-y (db.item-template-avatar-scale-y template)
                 :avatar-scale-z (db.item-template-avatar-scale-z template)
                 :x 0 :y 0 :z 0
                 :latitude 0 :longitude 0 :altitude -1000
                 :world :chor)))

(defun grant-item (template-id recipient)
  "Create a new instance of TEMPLATE-ID and give it to RECIPIENT."
  (let ((item (create-item template-id))
        (player-uuid (etypecase recipient
                       (db.Toot (db.Toot-player recipient))
                       (string (db.Toot-player (find-record 'db.Toot :name recipient)))
                       (db.person recipient)))
        (Toot  (etypecase recipient
                 (db.Toot recipient)
                 (string (find-record 'db.Toot :name recipient))
                 (db.person nil))))
    (make-record 'db.inventory-item
                 :item (db.item-uuid item)
                 :person player-uuid
                 :Toot (db.Toot-uuid Toot)
                 :equipped :N)))

(defun gift-item (item giver recipient)
  "Transfer the ownership of ITEM from GIVER to RECIPIENT."
  (when *user*
    (unless (eql *user* giver)
      (error 'not-allowed)))
  (let ((giver-player (etypecase recipient
                        (db.Toot (db.Toot-player recipient))
                        (string (db.Toot-player (find-record 'db.Toot :name recipient)))
                        (db.person recipient)))
        (giver-Toot  (etypecase recipient
                       (db.Toot recipient)
                       (string (find-record 'db.Toot :name recipient))
                       (db.person nil)))
        (recipient-player (etypecase recipient
                            (db.Toot (db.Toot-player recipient))
                            (string (db.Toot-player (find-record 'db.Toot :name recipient)))
                            (db.person recipient)))
        (recipient-Toot  (etypecase recipient
                           (db.Toot recipient)
                           (string (find-record 'db.Toot :name recipient))
                           (db.person nil))))
    (let ((inventory (find-record 'db.inventory
                                  :item (db.item-uuid item)
                                  :person (db.person-uuid giver-player)
                                  :Toot (db.Toot-uuid giver-Toot))))
      (setf (db.inventory-item-equipped inventory) :N
            (db.inventory-item-person inventory) (db.person-uuid recipient-player)
            (db.inventory-item-Toot inventory) (db.toot-uuid recipient-Toot))
      inventory)))

(defun vanish-item (item)
  "ITEM ceases to exist.")



(defun item-lose-energy (item amount)
  "Decrease the energy of ITEM by AMOUNT (stopping at zero).

If the item's  energy reaches zero, the effect of  its :On-Zero flag will
occur; either it will remain :EMPTY, or :VANISH.

If ITEM's Energy-Kind is :COUNTABLE, then AMOUNT must be an integer.")

(defun item-gain-energy (item amount)
  "Increate the energy of ITEM by AMOUNT (stopping at its :Energy-Max).

If ITEM's Energy-Kind is :COUNTABLE, then AMOUNT must be an integer.")


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
