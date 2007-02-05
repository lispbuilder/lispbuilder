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
    :depends-on (cffi lispbuilder-sdl-ttf-cffi lispbuilder-sdl)
    :components
    ((:module "sdl-ttf"
	      :components
	      ((:file "package")
	       (:file "generics" :depends-on ("package"))
	       (:file "globals" :depends-on ("package"))
	       (:file "font" :depends-on ("package" "generics" "globals"))
	       #-clisp(:file "cffi-finalizers" :depends-on ("font" "package" "globals"))
	       (:file "sdl-util-ttf" :depends-on ("font" "globals" "package"))
	       (:static-file "bitstream-vera-copyright")
	       (:static-file "Vera.ttf")))
     (:module "documentation"
	      :components
	      ((:html-file "header")
	       (:html-file "footer")
	       (:html-file "lispbuilder-sdl-ttf")))))
