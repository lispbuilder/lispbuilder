;;; -*- lisp -*-

(in-package #:lispbuilder-sdl-image-cffi) 

;;#+win32(eval-when (:compile-toplevel :load-toplevel :execute)
;;  (pushnew (merge-pathnames "../bin/" (directory-namestring (or *load-truename* *default-pathname-defaults*)))
;;	   cffi:*foreign-library-directories*
;;	   :test #'equal))

#+win32(eval-when (:compile-toplevel :load-toplevel :execute)
         (pushnew sdl-image-bin:*dll-path*
                  cffi:*foreign-library-directories*
                  :test #'equal))

(cffi:define-foreign-library sdl-image
  (:darwin (:framework "libSDL_image-1.2"))
  (:windows (:or "SDL_image.dll" "SDL_image1.2.dll"))
  (:unix (:or "libSDL_image-1.2.so.0"
	      "libSDL_image1.2" 
	      "libSDL_image.so")))

(cffi:define-foreign-library zlib
  (:windows (:or "zlib1.dll")))

(cffi:define-foreign-library libpng
  (:windows (:or "libpng12-0.dll")))

(cffi:use-foreign-library sdl-image)
(cffi:use-foreign-library zlib)
(cffi:use-foreign-library libpng)

