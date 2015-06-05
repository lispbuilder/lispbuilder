;;; -*- lisp -*-

(defpackage #:lispbuilder-opengl-examples-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-opengl-examples-system)

(defsystem lispbuilder-opengl-examples
    :description "Examples for the lispbuilder-opengl package."
    :author "Lispbuilder Mailing List <lispbuilder@googlegroups.com>"
    :maintainer "Lispbuilder Mailing List <lispbuilder@googlegroups.com>"
    :licence "BSD"
    :depends-on (cffi lispbuilder-sdl lispbuilder-opengl-1-1)
    :components
    ((:module "examples"
	      :components
	      ((:file "package")
	       (:file "SDL_examples_1_4 (gears)" :depends-on ("package"))))))
    
