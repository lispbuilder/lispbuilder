;;; -*- lisp -*-

(defpackage #:lispbuilder-opengl-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-opengl-system)

(defsystem lispbuilder-opengl
    :description "lispbuilder-opengl: gl Mesa v6.4.2 library wrapper and tools"
    :long-description
    "lispbuilder-opengl is a wrapper for the Mesa OpenGL \"gl.h\" library, v 6.4.2. 
    The wrapper is implemeted using CFFI to be highly 
    compatible across lisp implementations."
    :version "0.1"
    :author "Luke Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "BSD"
    :depends-on (cffi)
    :components
    ((:module "opengl"
	      :components
	      ((:file "package")
	       (:file "library")
	       (:file "gl" :depends-on ("package" "library"))))
     (:module "documentation"
	      :components
	      ((:doc-file "COPYING")
	       (:doc-file "README")))
     (:module "build"
	      :components
	      ((:static-file "openglswig.i")))))

