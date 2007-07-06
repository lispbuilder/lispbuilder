;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-image-system
  (:use #:cl #:asdf))

(in-package #:lispbuilder-sdl-image-system)

(defsystem lispbuilder-sdl-image
    :description "lispbuilder-sdl-image: SDL_image 1.2.5 library wrapper and tools"
    :long-description
    "lispbuilder-sdl-image is a wrapper for the SDL_image 1.2.5 library."
    :version "0.4"
    :author "Luke Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "MIT"
    :depends-on (cffi lispbuilder-sdl-image-cffi lispbuilder-sdl)
    :components
    ((:module "sdl-image"
	      :components
	      ((:file "package")
	       (:file "generics" :depends-on ("package"))
	       (:file "sdl-image-util" :depends-on ("package" "generics"))))
     (:module "documentation"
	      :components
	      ((:html-file "header")
	       (:html-file "footer")
	       (:html-file "lispbuilder-sdl-image")
	       (:static-file "alien.png")))))
