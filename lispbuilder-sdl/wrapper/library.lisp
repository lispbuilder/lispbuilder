;;; -*- lisp -*-

(in-package #:lispbuilder-sdl-base)

#+win32(eval-when (:compile-toplevel :load-toplevel :execute)
		  (pushnew (merge-pathnames "../bin/" (directory-namestring (or *load-truename* *default-pathname-defaults*)))
			   cffi:*foreign-library-directories*
			   :test #'equal))

(cffi:define-foreign-library sdl
  (:darwin (:framework "SDL"))
  (:windows "SDL.dll")
  (:unix (:or "libSDL" "libSDL.so" "libSDL-1.2.so" "libSDL-1.2.so.0.7.2")))

(cffi:use-foreign-library sdl)
