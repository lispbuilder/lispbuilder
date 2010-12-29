;;; -*- lisp -*-

(in-package #:lispbuilder-sdl-cffi) 

;; cffi:foreign-library-loaded-p is not yet in the released version of CFFI
;;(defparameter *image-loaded-p* (cffi:foreign-library-loaded-p 'sdl-image))
(defparameter *gfx-loaded-p* nil)

(cffi:define-foreign-library sdl-gfx
  (:darwin (:or "libSDL_gfx.dylib" (:framework "SDL_gfx")))
  (:windows "SDL_gfx.dll")
  (:unix (:or "libSDL_gfx"
	      "libSDL_gfx.so"
	      "libSDL_gfx.so.4"
	      "libSDL_gfx.so.13"
	      "libSDL_gfx.so.13.0.0")))

(defun load-gfx-library ()
  (setf *gfx-loaded-p* nil)
  (when (handler-case (cffi:use-foreign-library sdl-gfx)
          (load-foreign-library-error () nil))
    (setf *gfx-loaded-p* t)
    (pushnew :lispbuilder-sdl-gfx *features*)))

(eval-when (:load-toplevel :execute)
  (load-gfx-library))
