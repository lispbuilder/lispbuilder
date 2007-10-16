;;; -*-Lisp-*-

(defpackage :lispbuilder-windows-examples
  (:use :common-lisp :asdf :cffi :lispbuilder-windows))
(in-package :lispbuilder-windows-examples)

(defsystem lispbuilder-windows-examples
  :description "Examples for the lispbuilder-windows package."
  :depends-on (cffi lispbuilder-windows)
  :components
  ((:module "examples"
    :components
    ((:file "gui-eval")))))
