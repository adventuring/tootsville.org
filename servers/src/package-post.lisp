(in-package :Tootsville)

(defpost ensure-package-imports-from-oliphaunt-are-available ()
  (macro-function 'Tootsville::define-memo-function) ()
  "Did not import symbols correctly; `DEFINE-MEMO-FUNCTION' should ~
 be a macro, but it is not.

Got: ~a"
  (with-output-to-string (*standard-output*)
    (describe 'Tootsville::define-memo-function)))
