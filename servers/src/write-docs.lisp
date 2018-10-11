(in-package :tootsville)

(defun inform-declt-of-agplv3 ()
  "Adds the AGPLv3 to the list of licenses for DECLT."
  (let ((licenses (intern "*LICENSES*" (find-package :net.didierverna.declt))))
    (set licenses
         (cons (eval licenses)
               '((:agplv3
                  "The GNU Affero General Public License"
                  "This  program is  free  software; you  can redistribute  it
and/or  modify it  under  the terms  of the  GNU  Affero General  Public
License as  published by  the Free  Software Foundation;  either version
3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program; if not,  write to the Free Software Foundation,
Inc., 675 Mass Ave, Cambridge, MA 02139, USA."))))))

(defun write-docs ()
  "Write out the documentation in TeΧinfo format using DECLT.

Note that DECLT  is not usually compiled into the  binary by default, so
this  may  have  to  download  DECLT  and/or  its  dependencies  through
Quicklisp when called."
  (format *trace-output* "~& Writing documentation…")

  (ql:quickload :net.didierverna.declt)
  (let ((source-dir (asdf:component-pathname (asdf:find-system :tootsville))))
    (inform-declt-of-agplv3)
    (ensure-directories-exist (merge-pathnames #p"doc/" source-dir))
    (funcall (intern "DECLT" (find-package :net.didierverna.declt))
             :tootsville
             :library "Tootsville Ⅴ (Romance Ⅱ)"
             :texi-file (merge-pathnames #p"doc/Tootsville.texi"
                                         source-dir)
             :info-file (merge-pathnames #p "doc/Tootsville"
                                         source-dir)
             :license :agplv3
             :declt-notice :short
             :hyperlinks nil
             :introduction (alexandria:read-file-into-string
                            (merge-pathnames #p"src/doc/introduction"
                                             source-dir))
             :conclusion (alexandria:read-file-into-string
                          (merge-pathnames #p"src/doc/conclusion"
                                           source-dir)))))
