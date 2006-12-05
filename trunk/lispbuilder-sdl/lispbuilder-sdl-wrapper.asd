;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-wrapper-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-wrapper-system)

(defsystem lispbuilder-sdl-wrapper
    :description "lispbuilder-sdl-wrapper: The basic SDL library wrapper"
    :long-description
    "lispbuilder-sdl-wrapper uses CFFI to provide a very simple and low-level wrapper to libSDL."
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
	       (:file "sdl")
	       (:file "post-swig")
	       (:file "translate"))
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
