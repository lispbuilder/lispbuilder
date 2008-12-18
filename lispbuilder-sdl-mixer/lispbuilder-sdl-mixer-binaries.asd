;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-mixer-binaries-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-mixer-binaries-system)

(defsystem lispbuilder-sdl-mixer-binaries
  :description "lispbuilder-sdl-mixer-binaries: The Win32 binary for the SDL_mixer v1.2.7 library"
  :version "1.2.8"
  :author " Sam Lantinga <slopuken@libsdl.org>"
  :maintainer "Application Builder <application-builder@lispniks.com>"
  :licence "MIT"
  :components
  ((:module "bin"
    :components
    ((:file "package")
     (:file "globals")
               ;(:static-file "SDL_mixer.dll")
	       ;(:static-file "ogg.dll")
	       ;(:static-file "smpeg.dll")
	       ;(:static-file "vorbis.dll")
	       ;(:static-file "vorbisfile.dll")
     ))
   ;;     (:module "documentation"
   ;;	      :components
   ;;	      ((:doc-file "bin_README")))
   ))
