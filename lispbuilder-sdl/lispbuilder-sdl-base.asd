;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-base-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-base-system)

(defsystem lispbuilder-sdl-base
    :description "lispbuilder-sdl-base: The base lispified SDL library code"
    :long-description
    "lispbuilder-sdl-base provides a lispified layer above lispbuilder-sdl-wrapper (however foreign pointers are still used)."
    :version "0.8.0"
    :author "Justin Heyes-Jones <justinhj@gmail.com>, Luke J Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "MIT"
    :depends-on (cffi)
    :components
    ((:module "base"
	      :components
	      ((:file "package")
	       (:file "util")
	       (:file "events")
	       (:file "primitives")
	       (:file "surfaces"))
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
