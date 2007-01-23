;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-ttf-system
  (:use #:cl #:asdf))

(in-package #:lispbuilder-sdl-ttf-system)

(defsystem lispbuilder-sdl-ttf
    :description "lispbuilder-sdl-ttf: SDL_ttf 2.0.8 library wrapper and tools"
    :long-description
    "lispbuilder-sdl-ttf is a wrapper for the SDL_ttf 2.0.8 library."
    :version "0.1"
    :author "Luke J Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "MIT"
    :depends-on (cffi lispbuilder-sdl lispbuilder-sdl-ttf-cffi)
    :components
    ((:module "sdl-ttf"
	      :components
	      ((:file "globals")
	       (:file "font")
	       (:file "sdl-util-ttf" :depends-on ("globals"))
	       (:file "cffi-finalizers" :depends-on "sdl-util-ttf")))
     (:module "documentation"
	      :components
	      (:html-file "header.html")
	      (:html-file "footer.html")
	      (:html-file "lispbuilder-sdl-ttf"))))
