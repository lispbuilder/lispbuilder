;;; -*- lisp -*-

(in-package #:lispbuilder-sdl-mixer-cffi)

;;#+win32(eval-when (:compile-toplevel :load-toplevel :execute)
;;		  (pushnew (merge-pathnames "../bin/" (directory-namestring (or *load-truename* *default-pathname-defaults*)))
;;			   cffi:*foreign-library-directories*
;;			   :test #'equal))

#+win32(eval-when (:compile-toplevel :load-toplevel :execute)
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

(cffi:define-foreign-library ogg
  (:windows "libogg-0.dll"))

(cffi:define-foreign-library smpeg
  (:windows "smpeg.dll"))

(cffi:define-foreign-library vorbis
  (:windows "libvorbis-0.dll"))

(cffi:define-foreign-library vorbisfile
  (:windows "libvorbisfile-3.dll"))

(cffi:use-foreign-library ogg)
(cffi:use-foreign-library vorbis)
(cffi:use-foreign-library vorbisfile)
(cffi:use-foreign-library smpeg)
(cffi:use-foreign-library sdl-mixer)

