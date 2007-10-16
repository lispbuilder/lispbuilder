;;; -*- lisp -*-

(defpackage #:lispbuilder-opengl-1-2-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-opengl-1-2-system)

(defsystem lispbuilder-opengl-1-2
    :description "lispbuilder-opengl: gl Mesa v6.4.2 library wrapper and tools"
    :long-description
    "lispbuilder-opengl is a wrapper for the Mesa OpenGL \"gl.h\" library, v 6.4.2. 
    The wrapper is implemeted using CFFI to be highly 
    compatible across lisp implementations."
    :version "0.1"
    :author "Luke Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "BSD"
    :depends-on (cffi lispbuilder-opengl-1-2)
    :components
    ((:module "opengl"
	      :components
	      ((:file "gl_1_2")))
     (:module "build"
	      :components
	      ((:static-file "opengl_1_2_swig.i")))))

