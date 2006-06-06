;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-image-examples-system
  (:use #:cl #:asdf))
(in-package :lispbuilder-sdl-image-examples-system)

(defsystem lispbuilder-sdl-image-examples
    :description "Examples describing the use of lispbuilder-sdl-image."
    :version "0.1"
    :author "Luke Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "BSD"
    :depends-on (cffi lispbuilder-sdl lispbuilder-sdl-image)
    :components
    ((:module "examples"
	      :components
	      ((:file "package")
	       (:file "image-example" :depends-on ("package"))))))
