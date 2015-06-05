;;; -*-Lisp-*-

(defpackage lispbuilder-windows-system
  (:use :common-lisp :asdf))
(in-package :lispbuilder-windows-system)

(defsystem lispbuilder-windows
  :description "lispbuilder-windows: Windows library wrapper and tools"
  :version "0.1"
  :author "Lispbuilder Mailing List <lispbuilder@googlegroups.com>"
  :maintainer "Lispbuilder Mailing List <lispbuilder@googlegroups.com>"
  :licence "BSD"
  :depends-on (cffi)
  :components
  ((:module "windows"
	    :components
	    ((:file "package")
	     (:file "library")
	     (:file "windows")
	     (:file "user32")
	     (:file "winuser")
	     (:file "wingdi"))
	    :serial t)))
