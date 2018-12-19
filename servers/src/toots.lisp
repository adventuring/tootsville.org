(in-package :Tootsville)

(defvar *Toot* nil
  "The Toot that the active user, is currectly using.")

;;; Toot character data.

(defun find-Toot-by-name (Toot-name)
  (check-type Toot-name Toot-name)
  (find-record 'Toot :name Toot-name))


(defun Toot-childp (Toot)
  (player-childp (find-reference Toot :player)))

(defun Toot-item-info (inv)
  (let* ((item (find-reference inv :item))
         (template (find-reference item :template))
         (avatar (find-reference item :avatar)))
    (list :|equipped| (inventory-item-equipped inv)
          :|uuid| (item-uuid item)
          :|baseColor| (item-base-color item)
          :|energy| (item-energy item)
          :|template| (item-template-id template)
          :|name| (item-template-name template)
          :|defaultBaseColor| (item-template-default-base-color template)
          :|avatar| (avatar-moniker avatar)
          :|energyKind| (item-template-energy-kind template)
          :|onZero| (item-template-on-zero template)
          :|wearSlot| (item-template-wear-slot template)
          :|weight| (item-template-weight template))))

(defun Toot-inventory (Toot)
  (find-records 'inventory-item :Toot (Toot-uuid Toot)))

(defun Toot-info (Toot)
  (list :|name| (Toot-name Toot)
        :|note| (or (Toot-note Toot) "")
        :|avatar| (avatar-moniker (find-reference Toot :avatar))
        :|baseColor| (color24-name (Toot-base-color Toot))
        :|pattern| (string-downcase (pattern-name
                                     (find-reference Toot :pattern)))
        :|patternColor| (color24-name (Toot-pattern-color Toot))
        :|padColor| (color24-name (Toot-pad-color Toot))
        :|childP| (if (Toot-childp Toot) :true :false)
        :|sensitiveP| (if (or (Toot-childp Toot)
                              (person-sensitivep
                               (find-reference Toot :player)))
                          :true :false)
        :|lastSeen| (Toot-last-active Toot)
        :|equip| (apply #'vector
                        (mapcar #'Toot-item-info
                                (Toot-inventory Toot)))))
