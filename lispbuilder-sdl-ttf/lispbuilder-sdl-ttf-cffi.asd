;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-ttf-cffi-system
  (:use #:cl #:asdf))

(in-package #:lispbuilder-sdl-ttf-cffi-system)

(defsystem lispbuilder-sdl-ttf-cffi
    :description "lispbuilder-sdl-ttf: SDL_ttf 2.0.9 library wrapper and tools"
    :long-description
    "lispbuilder-sdl-ttf is a wrapper for the SDL_ttf 2.0.9 library."
    :version "0.1"
    :author "Luke J Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "MIT"
    :depends-on (cffi lispbuilder-sdl-cffi)
    :components
    ((:module "cffi"
	      :components
	      ((:file "package")
	       (:file "library" :depends-on ("package"))
	       (:file "ttf" :depends-on ("package" "library"))
	       (:file "translate" :depends-on ("ttf"))))
     (:module "glue"
	      :components
	      ((:static-file "Makefile")
	       (:static-file "lispbuilder-sdl-ttf-glue.c")))))
