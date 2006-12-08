;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-base-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-base-system)

(defsystem lispbuilder-sdl-base
    :description "lispbuilder-sdl-base: SDL library wrapper providing a base set of functionality."
    :long-description
    "The lispbuilder-sdl-base prackage provides a base set of functionality on top of the CFFI bndings of lispbuilder-sdl-wrapper."
    :version "0.8.0"
    :author "Justin Heyes-Jones <justinhj@gmail.com>, Luke J Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "MIT"
    :depends-on (cffi lispbuilder-sdl-wrapper)
    :components
    ((:module "base"
	      :components
	      ((:file "package")
	       (:file "util")
	       (:file "events")
	       (:file "rectangle")
	       (:file "surfaces")
	       (:file "video")
	       (:file "rwops"))
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
