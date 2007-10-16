;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-image-examples-system
  (:use #:cl #:asdf))
(in-package :lispbuilder-sdl-image-examples-system)

(defsystem lispbuilder-sdl-image-examples
    :description "Examples describing the use of lispbuilder-sdl-image."
    :version "0.4.1"
    :author "Luke Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "MIT"
    :depends-on (cffi lispbuilder-sdl lispbuilder-sdl-image)
    :components
    ((:module "examples"
	      :components
	      ((:file "package")
	       (:file "globals" :depends-on ("package"))
	       (:file "image-example" :depends-on ("globals" "package"))
	       (:static-file "lisp.bmp")
	       (:static-file "lisp.gif")
	       (:static-file "lisp.jpg")
	       (:static-file "lisp.lbm")
	       (:static-file "lisp.pcx")
	       (:static-file "lisp.png")
	       (:static-file "lisp.pbm")
	       (:static-file "lisp.ppm")
	       (:static-file "lisp.pgm")
	       (:static-file "lisp.tga")
	       (:static-file "lisp.tif")))))
