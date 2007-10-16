;;; -*-Lisp-*-

(defpackage lispbuilder-windows-system
  (:use :common-lisp :asdf))
(in-package :lispbuilder-windows-system)

(defsystem lispbuilder-windows
  :description "lispbuilder-windows: Windows library wrapper and tools"
  :version "0.1"
  :author "Frank Buss <fb@frank-buss.de>"
  :maintainer "Common Lisp Application Builder http://www.lispbuilder.org"
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
