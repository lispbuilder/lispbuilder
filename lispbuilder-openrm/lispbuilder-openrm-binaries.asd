;;; -*- lisp -*-

(defpackage #:lispbuilder-openrm-binaries-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-openrm-binaries-system)

(defsystem lispbuilder-openrm-binaries
  :description "lispbuilder-openrm-binaries: The windows binary for the OPENRM v1.8.0 library"
  :version "1.8.0"
  :author "Lispbuilder Mailing List <lispbuilder@googlegroups.com>"
  :maintainer "Lispbuilder Mailing List <lispbuilder@googlegroups.com>"
  :licence "BSD"
  :components
  ((:module "bin"
    :components
    ((:file "package")
     (:file "globals"))
    :serial t)))
