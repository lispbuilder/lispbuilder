;;; -*- lisp -*-

(defpackage #:lispbuilder-openrm-native-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-openrm-native-system)

(defsystem lispbuilder-openrm-native
    :description "lispbuilder-openrm-window: Native OS support"
    :version "0.1"
    :author "Luke Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "BSD"
    :depends-on (cffi lispbuilder-openrm lispbuilder-windows)
    :components
    ((:module "native"
	      :components
	      ((:file "package")
	       #+win32(:file "library" :depends-on ("package"))
 	       #+win32(:file "opengl32" :depends-on ("library"))
	       #+win32(:file "globals" :depends-on ("package"))
	       #+win32(:file "generics" :depends-on ("package"))
	       #+win32(:file "win32" :depends-on ("package" "library" "opengl32" "globals" "generics"))))))

