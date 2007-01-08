;;; -*- lisp -*-

(in-package #:lispbuilder-sdl-image-cffi) 

#+win32(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew (merge-pathnames "../bin/" (directory-namestring (or *load-truename* *default-pathname-defaults*)))
	   cffi:*foreign-library-directories*
	   :test #'equal))

(cffi:define-foreign-library sdl-image
  (:darwin (:framework "libSDL_image-1.2"))
  (:windows (:or "SDL_image.dll" "SDL_image1.2.dll"))
  (:unix (:or "libSDL_image1.2" "libSDL_image-1.2.so.0")))

(cffi:use-foreign-library sdl-image)
