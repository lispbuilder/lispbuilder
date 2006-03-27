;;; -*- lisp -*-

(in-package #:lispbuilder-sdl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew (merge-pathnames "../bin/" (directory-namestring (or *load-truename* *default-pathname-defaults*)))
	   cffi:*foreign-library-directories*
	   :test #'equal))

(cffi:define-foreign-library sdl
  (:darwin (:framework "SDL"))
  (:windows "SDL.dll")
  (:unix (:or "libSDL" "libSDL.so")))

(cffi:use-foreign-library sdl)
