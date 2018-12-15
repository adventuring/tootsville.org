(in-package :Tootsville)

;;; Toot character data.

(defun find-Toot-by-name (Toot-name)
  (check-type Toot-name Toot-name)
  (find-record 'db.Toot :name Toot-name))


(defun Toot-childp (Toot)
  (player-childp (find-reference Toot :player)))

(defun Toot-item-info (inv)
  (let* ((item (find-reference inv :item))
         (template (find-reference item :template))
         (avatar (find-reference item :avatar)))
    (list :|equipped| (db.inventory-item-equipped inv)
          :|uuid| (db.item-uuid item)
          :|baseColor| (db.item-base-color item)
          :|energy| (db.item-energy item)
          :|template| (db.item-template-id template)
          :|name| (db.item-template-name template)
          :|defaultBaseColor| (db.item-template-default-base-color template)
          :|avatar| (db.avatar-moniker avatar)
          :|energyKind| (db.item-template-energy-kind template)
          :|onZero| (db.item-template-on-zero template)
          :|wearSlot| (db.item-template-wear-slot template)
          :|weight| (db.item-template-weight template))))

(defun Toot-inventory (Toot)
  (find-records 'db.inventory-item :Toot (db.Toot-uuid Toot)))

(defun Toot-info (Toot)
  (list :|name| (db.Toot-name Toot)
        :|note| (or (db.Toot-note Toot) "")
        :|avatar| (db.avatar-moniker (find-reference Toot :avatar))
        :|baseColor| (color24-name (db.Toot-base-color Toot))
        :|pattern| (string-downcase (db.pattern-name 
                                     (find-reference Toot :pattern)))
        :|patternColor| (color24-name (db.Toot-pattern-color Toot))
        :|padColor| (color24-name (db.Toot-pad-color Toot))
        :|childP| (if (Toot-childp Toot) :true :false)
        :|sensitiveP| (if (or (Toot-childp Toot)
                              (db.person-sensitivep 
                               (find-reference Toot :player)))
                          :true :false)
        :|lastSeen| (db.Toot-last-active Toot)
        :|equip| (apply #'vector
                        (mapcar #'Toot-item-info
                                (Toot-inventory Toot)))))
