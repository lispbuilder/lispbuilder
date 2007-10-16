(in-package #:lispbuilder-net-cffi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-library-path (suffix)
    (merge-pathnames (concatenate 'string "../bin-" suffix)
                     (directory-namestring (or *load-truename* *default-pathname-defaults*))))
  #+darwin(pushnew (get-library-path "macosx/") cffi:*foreign-library-directories* :test #'equal)
  #+win32(pushnew (get-library-path "win32/") cffi:*foreign-library-directories* :test #'equal)
  #+unix(pushnew (get-library-path "linux/") cffi:*foreign-library-directories* :test #'equal))

(cffi:define-foreign-library net
  (:darwin "libnet.dylib")
  (:windows "net.dll")
  (:unix "libnet.so"))

(cffi:use-foreign-library net)
