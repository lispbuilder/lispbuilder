;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-ttf-examples-system
  (:use #:cl #:asdf))
(in-package :lispbuilder-sdl-ttf-examples-system)

(defsystem lispbuilder-sdl-ttf-examples
    :description "Examples describing the use of lispbuilder-sdl-ttf."
    :version "0.2.2"
    :author "Luke Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>, Luke Crook <luke@balooga.com>"
    :licence "BSD"
    :depends-on (cffi lispbuilder-sdl lispbuilder-sdl-ttf)
    :components
    ((:module "examples"
	      :components
	      ((:file "package")
	       (:file "font-example" :depends-on ("package"))))))
