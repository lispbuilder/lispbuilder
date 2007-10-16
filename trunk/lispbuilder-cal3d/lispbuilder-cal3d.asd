;;; -*- lisp -*-

(defpackage #:lispbuilder-cal3d-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-cal3d-system)

(defsystem lispbuilder-cal3d
    :description "lispbuilder-cal3d: Cal3D library wrapper and tools"
    :long-description
    "lispbuilder-cal3d uses CFFI to be highly compatible across lisp 
    implementations. It includes a selection of utilities to assist  
    game programming in Common Lisp."
    :version "0.1"
    :author "Luke Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "BSD"
    :depends-on (cffi)
    :components
    ((:module "cal3d"
	      :components
	      ((:file "package")
	       (:file "library" :depends-on ("package"))
	       (:file "cal3d" :depends-on ("package" "library"))
	       (:file "translate" :depends-on ("cal3d"))
	       (:file "util-cal3d" :depends-on ("cal3d" "translate"))
	       ))
     (:module "build"
	      :components
	      ((:static-file "cal3dswig.i")))))
