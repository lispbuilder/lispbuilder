;;; -*- lisp -*-

(defpackage #:lispbuilder-openrm-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-openrm-system)

(defsystem lispbuilder-openrm
    :description "lispbuilder-openrm: OpenRM library wrapper and tools"
    :long-description
    "lispbuilder-openrm uses CFFI to be highly compatible across lisp 
    implementations. It includes a selection of utilities to assist  
    game programming in Common Lisp."
    :version "0.1"
    :author "Luke Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "BSD"
    :depends-on (cffi)
    :components
    ((:module "openrm"
	      :components
	      ((:file "package")
	       (:file "library" :depends-on ("package"))
	       (:file "openrm" :depends-on ("package" "library"))
	       (:file "util-rm" :depends-on ("openrm"))
	       ))
     (:module "documentation"
	      :components
	      ((:html-file "index")
	       (:static-file "sdl1.png")
	       (:static-file "groovy1.png")
	       (:doc-file "README")
	       (:doc-file "COPYING")
	       (:doc-file "CONTRIBUTORS")))
     (:module "build"
	      :components
	      ((:static-file "openrmswig.i")))))
