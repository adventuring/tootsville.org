(dolist (asd-path (directory #p"~/Private/ciwta/src/lib/alexandria/*.asd"))
  (format *trace-output* "~&;;* ASDF file: ~a~%" asd-path)
  (with-open-file (asd asd-path :direction :input)
    (handler-case
        (asdf:load-asd asd-path)
      (error (error)
        (warn "Error reading system definition file ~a: ~a"
              asd-path error)))
    (let ((systems
            (loop
              for index from 1

              for form =
                       (handler-case
                           (read asd nil nil)
                         (error (error)
                           (warn "Error reading ~:r form in ~a: ~a"
                                 index asd-path error)))

              while form
              when (and (listp form)
                        (string-equal (first form) "DEFSYSTEM"))
                collect (second form))))
      (cond ((null systems)
             (warn "No DEFSYSTEM forms found in ~a" asd-path))
            (t
             (format *trace-output*
                     "~&;;* Testing ~r systems from ~a (~{~a~^, ~})~%"
                     (length systems)
                     asd-path
                     systems)
             (dolist (system systems)
               (format *trace-output* "~&;;* System: ~a~%" system)
               (handler-case
                   (ql:quickload system)
                 (error (error)
                   (warn "Error loading system ~a: ~a"
                         system error)))
               (handler-case
                   (asdf:test-system (asdf:find-system system))
                 (error (error)
                   (warn "Can't test system ~a: ~a"
                         system error)))))))))
