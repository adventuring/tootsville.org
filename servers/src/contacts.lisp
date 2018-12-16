(in-package :Tootsville)

(defun add-contact (owner contact)
  (make-record 'db.contact 
               :owner (db.Toot-uuid owner)
               :contact (db.Toot-uuid contact)
               :added (now)
               :last-used (now)
               :starredp nil))

(defun bool-sort (a b) (declare (ignore b)) a)

(defun Toot-contacts (Toot)
  (find-records 'db.contact :owner (db.Toot-uuid owner)))

(defun delete-contact (owner contact)
  (destroy-record
   (find-record 'db.contact
                :owner (db.Toot-uuid owner)
                :contact (db.Toot-uuid contact))))

