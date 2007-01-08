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
    :version "0.8.1"
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
	       (:file "init")
	       (:file "events")
	       (:file "primitives")
	       (:file "rectangle")
	       (:file "surfaces")
	       (:file "color")
	       (:file "pixel")
	       (:file "point")
	       (:file "video")
	       (:file "image")
	       (:file "rwops")
	       (:file "drawing-primitives")
	       (:file "simple-font")
	       (:file "sdl-util")
	       #-clisp(:file "cffi-finalizers")

	       (:static-file "font.bmp")
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
