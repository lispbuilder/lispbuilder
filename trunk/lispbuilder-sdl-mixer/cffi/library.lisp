;;; -*- lisp -*-

(in-package #:lispbuilder-sdl-mixer-cffi)

#+win32(eval-when (:compile-toplevel :load-toplevel :execute)
		  (pushnew (merge-pathnames "../bin/" (directory-namestring (or *load-truename* *default-pathname-defaults*)))
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
  (:windows "ogg.dll"))

(cffi:define-foreign-library smpeg
  (:windows "smpeg.dll"))

(cffi:define-foreign-library vorbis
  (:windows "vorbis.dll"))

(cffi:define-foreign-library vorbisfile
  (:windows "vorbisfile.dll"))

(cffi:use-foreign-library sdl-mixer)
(cffi:use-foreign-library ogg)
(cffi:use-foreign-library smpeg)
(cffi:use-foreign-library vorbis)
(cffi:use-foreign-library vorbisfile)
