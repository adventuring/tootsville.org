(defpackage org.star-hope.machine
  (:use :cl :uiop :org.tfeb.hax.memoize)
  (:export #:processor-count
           #:load-average))

(in-package #:org.star-hope.machine)

(defun range-size (numeric-range-string)
  (destructuring-bind (start end) (uiop:split-string numeric-range-string
                                                     :separator "-")
    (1+ (- (parse-integer end) (parse-integer start)))))

(def-memoized-function processor-count ()
  "Number of processor (cores) available."
  #+linux
  (progn
    (with-open-file (online "/sys/devices/system/cpu/online"
                            :direction :input
                            :if-does-not-exist :error)
      (let ((count 0))
        (loop for set = (read-line online nil nil)
              while set
              do (incf count (range-size set)))
        (the (integer 1 2000) count))))
  #-linux
  (error "I don't have code to check this on non-Linux hosts"))

(defun load-average ()
  "Load averages return as multiple-values.

Values are: load  averages over the past 1, 5,  and 10 minutes, followed
by  the  number of  actively-running  processes,  and the  total  number
of processes.

eg:

\(multiple-value-bind (load-average-1-minute
 load-average-5-minutes
 load-average-10-minutes
 number-of-processes-running
 total-number-of-processes)
 \(load-average))

… although commonly, only the primary value (load average over 1 minute)
will be of interest.

"
  (with-open-file (loadavg "/proc/loadavg"
                           :direction :input
                           :if-does-not-exist :error)
    (destructuring-bind (a1 a5 a10 ratio &rest _) 
        (uiop:split-string (read-line loadavg) :separator " ")
      (declare (ignore _))
      (destructuring-bind (running total)
          (uiop:split-string ratio :separator "/")
        (values (the (real 0 *) (parse-number:parse-real-number a1))
                (the (real 0 *) (parse-number:parse-real-number a5))
                (the (real 0 *) (parse-number:parse-real-number a10))
                (the (integer 0 #.(expt 2 32)) (parse-number:parse-real-number running))
                (the (integer 0 #.(expt 2 32)) (parse-number:parse-real-number total)))))))
