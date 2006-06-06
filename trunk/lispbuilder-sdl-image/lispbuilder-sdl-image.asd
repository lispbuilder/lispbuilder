;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-image-system
  (:use #:cl #:asdf))

(in-package #:lispbuilder-sdl-image-system)

(defsystem lispbuilder-sdl-image
    :description "lispbuilder-sdl-image: SDL_image 1.2.5 library wrapper and tools"
    :long-description
    "lispbuilder-sdl-image is a wrapper for the SDL_image 1.2.5 library."
    :version "0.1"
    :author "Luke Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "LGPL"
    :depends-on (cffi lispbuilder-sdl)
    :components
    ((:module "sdl-image"
	      :components
	      ((:file "package")
	       (:file "library" :depends-on ("package"))
	       (:file "sdl_image" :depends-on ("package" "library"))
	       (:file "translate" :depends-on ("sdl_image"))
	       (:file "util-sdl_image" :depends-on ("sdl_image" "translate"))))
     (:module "documentation"
	      :components
	      ((:doc-file "COPYING")
	       (:doc-file "CONTRIBUTORS")))))
