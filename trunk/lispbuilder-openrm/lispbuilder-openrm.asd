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
    :depends-on (cffi trivial-garbage lispbuilder-openrm-cffi)
    :components
    ((:module "openrm"
	      :components
	      ((:file "package")
	       (:file "library" :depends-on ("package"))
	       (:file "openrm" :depends-on ("package" "library"))
	       (:file "translate" :depends-on ("openrm"))
	       (:file "util-rm" :depends-on ("openrm" "translate"))
	       ))
     (:module "documentation"
	      :components
	      ((:html-file "index")
	       (:doc-file "README")
	       (:doc-file "COPYING")
	       (:doc-file "CONTRIBUTORS")))))
