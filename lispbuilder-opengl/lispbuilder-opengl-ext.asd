;;; -*- lisp -*-

(defpackage #:lispbuilder-opengl-ext-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-opengl-ext-system)

(defsystem lispbuilder-opengl-ext
    :description "lispbuilder-opengl: gl Mesa v6.4.2 library wrapper and tools"
    :long-description
    "lispbuilder-opengl is a wrapper for the Mesa OpenGL \"glext.h\" library, v 6.4.2. 
    The wrapper is implemeted using CFFI to be highly 
    compatible across lisp implementations."
    :version "0.1"
    :author "Luke Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "BSD"
    :depends-on (cffi lispbuilder-opengl)
    :components
    ((:module "opengl"
	      :components
	      ((:file "package")
	       (:file "gl_ext" :depends-on ("package"))))
     (:module "build"
	      :components
	      ((:static-file "openglextswig.i")))))

