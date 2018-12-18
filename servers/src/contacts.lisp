(in-package :Tootsville)

(defun add-contact (owner contact)
  (make-record 'contact 
               :owner (Toot-uuid owner)
               :contact (Toot-uuid contact)
               :added (now)
               :last-used (now)
               :starredp nil))

(defun bool-sort (a b) (declare (ignore b)) a)

(defun Toot-contacts (Toot)
  (find-records 'contact :owner (Toot-uuid owner)))

(defun delete-contact (owner contact)
  (destroy-record
   (find-record 'contact
                :owner (Toot-uuid owner)
                :contact (Toot-uuid contact))))

