;;; -*- lisp -*-

(defpackage lispbuilder-opengl-system
  (:use :common-lisp :asdf :cffi))
(in-package lispbuilder-opengl-system)

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
    :components
    ((:module "opengl"
	      :components
	      ((:file "package")
	       (:file "gl")))))
    
