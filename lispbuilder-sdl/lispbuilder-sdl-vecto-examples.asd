;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-vecto-examples-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-vecto-examples-system)

(defsystem lispbuilder-sdl-vecto-examples
    :description "Examples for the LISPBUILDER-SDL-VECTO package."
    :version "0.0.1"
    :author "Lispbuilder Mailing List <lispbuilder@googlegroups.com>"
    :maintainer "Lispbuilder Mailing List <lispbuilder@googlegroups.com>"
    :license "MIT"
    :depends-on (lispbuilder-sdl-vecto)
    :components
    ((:module "examples"
	      :components
	      ((:file "package")
	       (:file "vecto" :depends-on ("package"))
	       (:file "roots" :depends-on ("package"))))))
