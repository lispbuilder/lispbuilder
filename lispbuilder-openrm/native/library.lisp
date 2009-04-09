;;; -*- lisp -*-

(in-package #:lispbuilder-openrm-cffi)


#+win32(eval-when (:compile-toplevel :load-toplevel :execute)
		  (pushnew (merge-pathnames "../bin/" (directory-namestring (or *load-truename* *default-pathname-defaults*)))
			   cffi:*foreign-library-directories*
			   :test #'equal))

;; (cffi:define-foreign-library GL
;;   (:darwin (:framework "OpenGL"))
;;   (:windows "OPENGL32.dll")
;;   (:unix (:or "libGL")))

;; (cffi:use-foreign-library GL)

#+win32(cffi:define-foreign-library opengl32
  (:windows "opengl32.dll"))


#+win32(cffi:use-foreign-library opengl32)
