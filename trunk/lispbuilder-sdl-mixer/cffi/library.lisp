;;; -*- lisp -*-

(in-package #:lispbuilder-sdl-mixer-cffi)

;;#+win32(eval-when (:compile-toplevel :load-toplevel :execute)
;;		  (pushnew (merge-pathnames "../bin/" (directory-namestring (or *load-truename* *default-pathname-defaults*)))
;;			   cffi:*foreign-library-directories*
;;			   :test #'equal))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew sdl-mixer-bin:*dll-path*
	   cffi:*foreign-library-directories*
	   :test #'equal))

(cffi:define-foreign-library sdl-mixer
  (:darwin (:framework "SDL_mixer"))
  (:windows "SDL_mixer.dll")
  (:unix (:or "libSDL_mixer"
	      "libSDL_mixer.so"
	      "libSDL_mixer-1.2.so"
	      "libSDL_mixer-1.2.so.0")))

(cffi:define-foreign-library mikmod
  (:windows "mikmod.dll"))

(cffi:define-foreign-library ogg
  (:windows "libogg-0.dll"))

(cffi:define-foreign-library smpeg
  (:windows "smpeg.dll"))

(cffi:define-foreign-library vorbis
  (:windows "libvorbis-0.dll"))

(cffi:define-foreign-library vorbisfile
  (:windows "libvorbisfile-3.dll"))

(defun load-library ()
  (cffi:use-foreign-library sdl-mixer)
  
  (when (handler-case (cffi:use-foreign-library mikmod)
          (load-foreign-library-error () nil)))
  (when (handler-case (cffi:use-foreign-library ogg)
          (load-foreign-library-error () nil)))
  (when (handler-case (cffi:use-foreign-library vorbis)
          (load-foreign-library-error () nil)))
  (when (handler-case (cffi:use-foreign-library vorbisfile)
          (load-foreign-library-error () nil)))
  (when (handler-case (cffi:use-foreign-library smpeg)
          (load-foreign-library-error () nil))))

(eval-when (:load-toplevel :execute)
  (load-library))
