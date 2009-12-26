;;; -*- lisp -*-

(in-package #:lispbuilder-sdl-cffi)

;;#+windows(eval-when (:compile-toplevel :load-toplevel :execute)
;;         (pushnew (merge-pathnames "../bin/"
;;                                   (directory-namestring (or *load-truename* *default-pathname-defaults*)))
;;                  cffi:*foreign-library-directories*
;;                  :test #'equal))

#+windows(eval-when (:compile-toplevel :load-toplevel :execute)
                        (pushnew sdl-bin:*dll-path*
                                 cffi:*foreign-library-directories*
                                 :test #'equal))

;; This is where FINK installs SDL.
#+darwin(pushnew #P"/sw/lib/" cffi:*foreign-library-directories*
	             :test #'equal)
+;; This is where MacPorts installs SDL.
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
	      "lispbuilder-sdl-glue.so"))
  (t (:or)))

(cffi:use-foreign-library sdl)

(when (handler-case (cffi:use-foreign-library sdl-glue)
        (load-foreign-library-error () nil))
  (pushnew :lispbuilder-sdl-audio *features*))


;;(defparameter *glue-loaded-p* (foreign-library-loaded-p 'sdl-glue))
