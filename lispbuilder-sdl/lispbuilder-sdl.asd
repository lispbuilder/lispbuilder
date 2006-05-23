;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-system)

(defsystem lispbuilder-sdl
    :description "lispbuilder-sdl: SDL library wrapper and tools"
    :long-description
    "lispbuilder-sdl uses CFFI to be highly compatible across lisp 
    implementations. It includes a selection of utilities to assist  
    game programming in Common Lisp."
    :version "0.7.1"
    :author "Justin Heyes-Jones <justinhj@gmail.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "BSD"
    :depends-on (cffi)
    :components
    ((:module "sdl"
	      :components
	      ((:file "package")
	       (:file "library" :depends-on ("package"))
	       (:file "sdl" :depends-on ("package" "library"))
	       (:file "post-swig" :depends-on ("sdl"))
	       (:file "util-sdl" :depends-on ("sdl" "post-swig"))))
     (:module "documentation"
	      :components
	      ((:html-file "index")
	       (:static-file "sdl1.png")
	       (:static-file "groovy1.png")
	       (:doc-file "README")
	       (:doc-file "COPYING")
	       (:doc-file "CONTRIBUTORS")
             (:doc-file "cl-sdl_LICENSE")))
     (:module "build"
	      :components
	      ((:static-file "sdlswig.i")))))
