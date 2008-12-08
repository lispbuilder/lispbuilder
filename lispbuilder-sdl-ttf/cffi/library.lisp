;;; -*- lisp -*-

(in-package #:lispbuilder-sdl-ttf-cffi) 

;;#+win32(eval-when (:compile-toplevel :load-toplevel :execute)
;;  (pushnew (merge-pathnames "../bin/" (directory-namestring (or *load-truename* *default-pathname-defaults*)))
;;	   cffi:*foreign-library-directories*
;;	   :test #'equal))

#+win32(eval-when (:compile-toplevel :load-toplevel :execute)
         (pushnew sdl-ttf-bin:*dll-path*
                  cffi:*foreign-library-directories*
                  :test #'equal))

(cffi:define-foreign-library zlib1
  (:windows "zlib1.dll")
  ;; (:unix "libz.so")
  )

(cffi:define-foreign-library libfreetype-6
  (:windows "libfreetype-6.dll")
  ;; (:unix "libfreetype.so")
  )

(cffi:define-foreign-library sdl-ttf
  (:darwin (:or (:framework "SDL_ttf")
		(:framework "libSDL_ttf-2.0")))
  (:windows (:or "SDL_ttf.dll"))
  (:unix (:or "libSDL_ttf" "libSDL_ttf2.0" "libSDL_ttf-2.0.so.0")))

(cffi:define-foreign-library sdl-ttf-glue
  (:darwin (:or "liblispbuilder-sdl-ttf-glue.dylib"
		"lispbuilder-sdl-ttf-glue.dylib"))
  (:windows (:or "liblispbuilder-sdl-ttf-glue.dll"
		 "lispbuilder-sdl-ttf-glue.dll"))
  (:unix "liblispbuilder-sdl-ttf-glue.so"))

;; Load the win32 required libraries.
(cffi:use-foreign-library zlib1)
(cffi:use-foreign-library libfreetype-6)

;; Load the general sdl libraries.
(cffi:use-foreign-library sdl-ttf)
(cffi:use-foreign-library sdl-ttf-glue)
