;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-cffi-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-cffi-system)

(defsystem lispbuilder-sdl-cffi
    :description "lispbuilder-sdl-cffi: Basic Lisp wrapper for the SDL library."
    :long-description
    "The lispbuilder-sdl-cffi package uses CFFI to provide a very low-level set of Lisp bindings to the SDL library."
    :version "0.9.8"
    :author "Justin Heyes-Jones <justinhj@gmail.com>, Luke J Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "MIT"
    :depends-on (cffi #+(or mswindows win32)lispbuilder-sdl-binaries)
    :components
    ((:module "cffi"
	      :components
	      ((:file "package")
	       (:file "library")
	       (:file "util")
	       (:file "cffi-util")
	       (:file "cffi-translate")
	       (:file "endian")
	       (:file "version")
	       (:file "stdinc")
               (:file "mutex")
	       (:file "timer")
	       (:file "error")
	       (:file "rwops")
	       (:file "audio")
	       (:file "cdrom")
	       (:file "joystick")
	       (:file "active")
	       (:file "keysym")
	       (:file "mouse")
	       (:file "events")
	       (:file "syswm")
	       (:file "video")
	       (:file "sdl")
               (:file "glue")
	       (:file "documentation"))
	      :serial t)
     (:module "build"
	      :components
	      ((:static-file "sdlswig.i")))))
