;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-wrapper-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-wrapper-system)

(defsystem lispbuilder-sdl-wrapper
    :description "lispbuilder-sdl-wrapper: Basic Lisp wrapper for the SDL library."
    :long-description
    "The lispbuilder-sdl-wrapper package uses CFFI to provide a very low-level set of Lisp bindings to the SDL library."
    :version "0.8.0"
    :author "Justin Heyes-Jones <justinhj@gmail.com>, Luke J Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "MIT"
    :depends-on (cffi)
    :components
    ((:module "wrapper"
	      :components
	      ((:file "package")
	       (:file "library")
	       (:file "util")
	       (:file "cffi-util")
	       (:file "cffi-translate")
	       (:file "endian")
	       (:file "version")
	       (:file "stdinc")
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
	       (:file "sdl"))
	      :serial t)
     (:module "documentation"
	      :components
	      ((:html-file "index")
	       (:static-file "sdl1.png")
	       (:static-file "groovy1.png")
	       (:doc-file "README")
	       (:doc-file "COPYING")
	       (:doc-file "CONTRIBUTORS")
             (:doc-file "cl-sdl_LICENSE")))
     (:module "build"
	      :components
	      ((:static-file "sdlswig.i")))))
