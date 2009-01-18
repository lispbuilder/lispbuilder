;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-base-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-base-system)

(defsystem lispbuilder-sdl-base
    :description "lispbuilder-sdl-base: SDL library wrapper providing a base set of functionality."
    :long-description
    "The lispbuilder-sdl-base prackage provides a base set of functionality on top of the CFFI bndings of lispbuilder-sdl-wrapper."
    :version "0.9.8"
    :author "Justin Heyes-Jones <justinhj@gmail.com>, Luke J Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "MIT"
    :depends-on (cffi lispbuilder-sdl-cffi)
    :components
    ((:module "base"
	      :components
	      ((:file "package")
	       (:file "globals")
	       (:file "util")
	       (:file "fps")
               ;;(:file "events")
	       (:file "pixel")
	       (:file "rectangle")
	       (:file "surfaces")
	       (:file "video")
	       (:file "rwops")
	       (:file "sdl-util"))
	      :serial t)))
