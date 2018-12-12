(in-package :Tootsville)


(define-constant +Toot-base-color-names+
    '(Cyan Indigo Orange Pink Red Turquoise Violet White Yellow)
  :test #'equalp
  :documentation "Named colors allowed as Toot base colors")

(define-memo-function Toot-base-color-name-p (string-designator)
  (check-type string-designator string-designator)
  (member string-designator +Toot-base-color-names+
          :test #'string-equal))

(deftype Toot-base-color-name ()
  '(and string-designator
    (satisfies Toot-base-color-name-p)))

(define-constant +Toot-pads-color-names+
    '(Cyan Indigo Pink Red "Spring Green" Violet White Yellow)
  :test #'equalp
  :documentation "Named colors allowed as Toot pads colors")

(define-memo-function Toot-pads-color-name-p (string-designator)
  "Is STRING-DESIGNATOR a color that can be used for foot pads and trunk tips"
  (check-type string-designator string-designator)
  (member string-designator +Toot-pads-color-names+
          :test #'string-equal))

(deftype Toot-pads-color-name ()
  "A color name that can be used for Toot foot pads and nose tip"
  '(and string-designator
    (satisfies Toot-pads-color-name-p)))

(define-constant +Toot-pattern-color-names+
    '(Black Cyan Indigo Orange Pink Rainbow Turquoise Violet White Yellow)
  :test #'equalp
  :documentation "Named colors allowed as Toot pattern colors")

(define-memo-function Toot-pattern-color-name-p (string-designator)
  "Is STRING-DESIGNATOR the name of a color that can be used for a pattern?"
  (check-type string-designator string-designator)
  (member string-designator
          +Toot-pattern-color-names+
          :test #'string-equal))

(deftype Toot-pattern-color-name ()
  "The name of a color that can be used for a pattern"
  '(and string-designator
    (satisfies Toot-pattern-color-name-p)))



(define-constant +Toot-basic-pattern-names+
    '(Flowers Horseshoes Lightning Patches "Polka Dots"
      Notes Sparkles Spots Stars Swirls)
  :test #'equalp
  :documentation "Basic patterns available to any Toot")

(define-constant +Toot-extended-pattern-names+
    '(Circuitboard Peace)
  :test #'equalp
  :documentation "Extended patterns that require special effort to obtain")

(define-memo-function Toot-pattern-name-p (string-designator)
  "Is STRING-DESIGNATOR the name of a Toot pattern?"
  (check-type string-designator string-designator)
  (member string-designator (concatenate 'list
                                         +Toot-basic-pattern-names+
                                         +Toot-extended-pattern-names+)
          :test #'string-equal))

(deftype Toot-pattern-name ()
  "The name of a Toot pattern"
  '(and string-designator
    (satisfies Toot-pattern-name-p)))

(define-memo-function allowed-base-colors-under-pattern (pattern-color)
  (remove-if (curry #'equalp pattern-color) +Toot-base-color-names+))

(define-memo-function  allowed-pattern-colors-on-base (base-color)
  (remove-if (curry #'equalp base-color) +Toot-pattern-color-names+))



(defun check-pattern-on-base-color (pattern-color base-color
                                    &key Toot-name pads-color pattern address)
  (tagbody do-over
     (restart-case
         (progn
           (check-type base-color Toot-base-color-name "The name ofa Toot base color")
           (check-type pattern-color Toot-pattern-color-name "The name of a Toot pattern color")
           (when (equal pattern-color base-color)
             (error "A Toot may not have the same base and pattern color; currently, both are ~:(~a~)~
~@[, with a pattern of ~:(~a~)~]~
~@[, and pads color of ~:(~a~)~]~
~@[ for the Toot named ~a~]~
~@[ (player with e-mail ~a)~]."
                    base-color pattern pads-color Toot-name address))
           (list pattern-color base-color))
       (change-pattern-color (color)
         :report (lambda (s)
                   (format s "Change the pattern color to one of: ~:(~{~a~^, ~}~)"
                           (allowed-pattern-colors-on-base base-color)))
         (setf pattern-color color)
         (check-type pattern-color Toot-pattern-color-name)
         (go do-over))
       (change-base-color (color)
         :report (lambda (s)
                   (format s "Change the base color to one of: ~:(~{~a~^, ~}~)"
                           (allowed-base-colors-under-pattern pattern-color)))
         :interactive-function (lambda (s)
                                 (let ((bases (allowed-base-colors-under-pattern pattern-color)))
                                   (format s "~&Choose a new base color by name or number:")
                                   (loop for color in bases
                                      for i from 1
                                      do (format s "~% [ ~d ] ~:(~a~)" i color))
                                   (format s "~% Base Color 🢩 ")
                                   (finish-output s)
                                   (let ((in (read s)))
                                     (typecase in
                                       (number
                                        (assert (<= 1 in (length bases)) (in)
                                                "The number ~d is not ~
the index from 1 to ~d of a new base color in the list where 1=~{~a~^, ~}"
                                                in (length bases) bases)
                                        (elt bases in))
                                       (t (princ-to-string in))))))
         (setf base-color color)
         (check-type base-color Toot-pattern-color-name)
         (go do-over)))))



(defstruct color24 red green blue)

(defun color24-hsv (color)
  (declare (optimize (speed 1) (safety 2)))
  (let* ((red (the (real 0 1)
                   (/ (the (unsigned-byte 8) (color24-red color))
                      255.0d0)))
         (green (the (real 0 1)
                     (/ (the (unsigned-byte 8) (color24-green color))
                        255.0d0)))
         (blue (the (real 0 1)
                    (/ (the (unsigned-byte 8) (color24-blue color))
                       255.0d0)))
         (c-max (the (real 0 1) (max red green blue)))
         (c-min (the (real 0 1) (min red green blue)))
         (delta (the (real 0 1) (- (the (real 0 1) c-max)
                                   (the (real 0 1) c-min)))))
    (if (< 0 delta)
        (list
         ;; hue
         (mod (* (/ (* 60.0d0
                       (cond
                         ((= c-max red) (mod (/ (- green blue) delta) 6))
                         ((= c-max green) (+ (/ (- blue red) delta) 2))
                         ((= c-max blue) (+ (/ (- red green) delta) 4))
                         (t (error "unreachable"))))
                    360.0d0)
                 2 pi)
              (* 2 pi))
         ;; saturation
         (if (< 0 c-max)
             (/ delta c-max)
             0)
         ;; value
         c-max)
        ;; else
        (list 0 0 c-max))))

(defun color24-hue (color)
  (first (color24-hsv color)))

(defun color24-saturation (color)
  (second (color24-hsv color)))

(defun color24-value (color)
  (third (color24-hsv color)))

(defun integer-to-color24 (number)
  (make-color24 :red (ldb (byte 8 16) number)
	      :green (ldb (byte 8 8) number)
	      :blue (ldb (byte 8 0) number)))

(defun color24-to-integer (color)
  (+ (ash (color24-red color) 16)
     (ash (color24-green color) 8)
     (color24-blue color)))

(defun parse-color24 (color)
  "Parse COLOR as a name for a color, or a hex 24-bit color value"
  (integer-to-color24 
   (etypecase color
     (string (cond
               ((equalp color "silver") #xdddddd)
               ((equalp color "charcoal") #x333333)
               ((equalp color "white") #xffffff)
               ((equalp color "black") #x000000)
               ((equalp color "deep-purple") #xb117ff)
               ((equalp color "yellow") #xff216f)
               ((equalp color "pink") #xe73e97)
               ((equalp color "turquoise") #x00a290)
               ((equalp color "periwinkle") #x96b4de)
               ((equalp color "violet") #x9669ad)
               ((equalp color "gold") #xf7d023)
               ((equalp color "burgundy") #x9c0059)
               ((equalp color "green") #x7ac142)
               ((equalp color "blue") #x0082c8)
               ((equalp color "lavender") #xba9dca)
               ((equalp color "tan") #xffd2a0)
               ((equalp color "red") #xe51b24)
               ((equalp color "spring-green") #xc4d82d)
               ((equalp color "indigo") #x0028ff)
               ((equalp color "orange") #xff7b26)
               (t (parse-number color :radix 16))))
     (number color))))
