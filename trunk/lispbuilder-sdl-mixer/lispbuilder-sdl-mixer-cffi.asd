;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-mixer-cffi-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-mixer-cffi-system)

(defsystem lispbuilder-sdl-mixer-cffi
    :description "lispbuilder-sdl-mixer-cffi: SDL_mixer 1.2.8 library wrapper and tools"
    :long-description
    "lispbuilder-sdl-mixer is a wrapper for the SDL_mixer 1.2.8 library."
    :version "0.3"
    :author "Luke Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "MIT"
    :depends-on (cffi lispbuilder-sdl
                      #+(or mswindows win32)lispbuilder-sdl-mixer-binaries)
    :components
    ((:module "cffi"
	      :components
	      ((:file "package")
	       (:file "library" :depends-on ("package"))
;; 	       (:file "base" :depends-on ("package"))
 	       (:file "mixer" :depends-on ("package"))
	       (:file "sdl-mixer" :depends-on ("package" "library"))
	       (:file "translate" :depends-on ("sdl-mixer" "mixer"))))))
