;;; -*- lisp -*-

(defpackage #:lispbuilder-operm-cffi-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-operm-cffi-system)

(defsystem lispbuilder-openrm-cffi
    :version "0.1"
    :author "Luke Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "BSD"
    :depends-on (cffi)
    :components
    ((:module "cffi"
	      :components
	      ((:file "package")
	       (:file "library" :depends-on ("package"))
 	       (:file "openrm" :depends-on ("package" "library"))
 	       (:file "translate" :depends-on ("openrm"))))
     (:module "build"
	      :components
	      ((:static-file "openrmswig.i")))))
