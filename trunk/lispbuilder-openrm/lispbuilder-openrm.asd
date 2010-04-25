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
  :depends-on (cffi lispbuilder-openrm-cffi lispbuilder-openrm-base trivial-garbage log5 simple-finalizer)
  :perform (load-op :after (op lispbuilder-openrm)
		    (pushnew :lispbuilder-openrm *features*))
  :components
  ((:module "openrm"
	    :components
	    ((:file "package")
	     (:file "log5")
	     (:file "globals")
	     (:file "generics")
	     (:file "util")
             ;;(:file "base")
	     (:file "openrm")
	     (:file "events")
	     (:file "color")
	     (:file "vertex")
	     (:file "camera")
	     (:file "pipe")
	     (:file "primitive")
	     (:file "procedural-primitives")
	     (:file "quad-mesh-primitive")
	     (:file "text-primitive")
	     (:file "light")
	     (:file "node")
	     ;; (:file "meta-node")
	     ;; (:file "scene-proxy")
	     (:file "scene")
	     (:file "rm-aux")
	     (:file "window")
	     (:file "rendering")
	     (:file "image")
	     (:file "mouse")
	     ;; (:file "pick")
             (:file "init")
             (:file "objects")
             (:file "gl")
	     ;(:file "cffi-finalizers")
             )
	    :serial t)
   (:module "documentation"
	    :components
	    ((:html-file "index")
	     (:doc-file "README")
	     (:doc-file "COPYING")
	     (:doc-file "CONTRIBUTORS")))))
