;;; -*- lisp -*-

(in-package #:lispbuilder-cal3d)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew (merge-pathnames "../bin/" (directory-namestring (or *load-truename* *default-pathname-defaults*)))
	   cffi:*foreign-library-directories*
	   :test #'equal))

(cffi:define-foreign-library cal3d
  (:darwin (:framework "cal3d"))
  (:windows "cal3d.dll")
  (:unix (:or "cal3d" "cal3d.so")))

(cffi:use-foreign-library cal3d)
