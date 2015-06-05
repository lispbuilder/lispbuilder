;;; -*- lisp -*-

(defpackage #:lispbuilder-openrm-base-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-openrm-base-system)

(defsystem lispbuilder-openrm-base
    :version "0.1"
    :author "Luke Crook <luke@balooga.com>"
    :maintainer "Lispbuilder Mailing List <lispbuilder@googlegroups.com>"
    :licence "BSD"
    :depends-on (cffi lispbuilder-openrm-cffi)
    :components
    ((:module "base"
	      :components
	      ((:file "package")
	       (:file "utils"))	
	      :serial t)))
