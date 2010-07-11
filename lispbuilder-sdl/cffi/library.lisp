;;; -*- lisp -*-

(in-package #:lispbuilder-sdl-cffi)

;; cffi:foreign-library-loaded-p is not yet in the released version of CFFI
;;(defparameter *glue-loaded-p* (cffi:foreign-library-loaded-p 'sdl-glue))
(defparameter *glue-loaded-p* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew sdl-bin:*dll-path*
           cffi:*foreign-library-directories*
           :test #'equal))

#+darwin
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew sdl-bin:*dll-path*
           cffi:*darwin-framework-directories*
           :test #'equal))

;; This is where FINK installs SDL.
#+darwin(pushnew #P"/sw/lib/" cffi:*foreign-library-directories*
	             :test #'equal)
;; This is where MacPorts installs SDL.
#+darwin(pushnew #P"/opt/local/lib/" cffi:*foreign-library-directories*
                 :test #'equal)

(cffi:define-foreign-library sdl
  (:darwin (:or (:framework "SDL")
                (:default "libSDL")))
  (:windows "SDL.dll")
  (:unix (:or "libSDL-1.2.so.0.7.2"
	      "libSDL-1.2.so.0"
	      "libSDL-1.2.so"
	      "libSDL.so"
	      "libSDL")))

(cffi:define-foreign-library sdl-glue
  (:windows "liblispbuilder-sdl-glue.dll")
  (:darwin (:or (:framework "liblispbuilder-sdl-glue")
                (:default "liblispbuilder-sdl-glue")))
  (:unix (:or "liblispbuilder-sdl-glue"
	      "liblispbuilder-sdl-glue.so"))
  (t (:or)))

(defun load-sdl-library ()
  (cffi:use-foreign-library sdl)

  (when (handler-case (cffi:use-foreign-library sdl-glue)
          (load-foreign-library-error () nil))
    (progn
      (setf *glue-loaded-p* t)
      (pushnew :lispbuilder-sdl-audio *features*))))

(eval-when (:load-toplevel :execute)
  (load-sdl-library))
