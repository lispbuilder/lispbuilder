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
    :version "0.8.0"
    :author "Justin Heyes-Jones <justinhj@gmail.com>, Luke J Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "MIT"
    :depends-on (cffi lispbuilder-sdl-cffi lispbuilder-sdl-base)
    :components
    ((:module "sdl"
	      :components
	      ((:file "package")
	       (:file "globals")
	       (:file "generics")
	       (:file "classes")
	       (:file "util")
;	       (:file "events")
	       (:file "primitives")
	       (:file "surfaces")
	       (:file "rectangle")
	       (:file "color")
	       (:file "pixel")
;	       (:file "drawing-primitives")
	       (:file "video")
	       (:file "image")
	       (:file "sdl-util")
;	       (:file "sdl_simplefont")
	       #-clisp(:file "cffi-finalizers")
	       )
	      :serial t)
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
