;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-gfx-examples-system
  (:use #:cl #:asdf))
(in-package :lispbuilder-sdl-gfx-examples-system)

(defsystem lispbuilder-sdl-gfx-examples
    :description "Examples describing the use of lispbuilder-sdl-gfx."
    :version "0.6.2"
    :author "Luke Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "MIT"
    :depends-on (lispbuilder-sdl-gfx)
    :components
    ((:module "examples"
	      :components
	      ((:file "package")
	       (:file "globals" :depends-on ("package"))
	       (:file "font" :depends-on ("package"))
	       (:file "utils" :depends-on ("package"))
	       (:file "random-circles" :depends-on ("package"))
	       (:file "setup-and-draw" :depends-on ("package"))
	       (:file "width-height" :depends-on ("package"))
	       (:file "recursion" :depends-on ("package"))
	       (:file "functions" :depends-on ("package"))
	       (:file "objects" :depends-on ("package"))
	       (:file "points-and-lines" :depends-on ("package"))
	       (:file "shape-primitives" :depends-on ("package"))
	       (:file "bezier" :depends-on ("package"))
	       (:file "distance-2D" :depends-on ("package"))
	       (:file "vertices" :depends-on ("package"))
	       (:file "rotozoom" :depends-on ("package" "globals"))
	       (:file "fireworks" :depends-on ("package" "globals"))
	       #-sbcl(:file "metaballs" :depends-on ("package"))
	       ))))
