;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-system)

(defsystem lispbuilder-sdl
    :description "lispbuilder-sdl: Wrapper and tools for SDL 1.2.13"
    :long-description
    "lispbuilder-sdl uses CFFI to be highly compatible across lisp 
    implementations. It includes a selection of utilities to assist  
    game programming in Common Lisp."
    :version "0.9.5"
    :author "Justin Heyes-Jones <justinhj@gmail.com>, Luke J Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>, Luke J Crook <luke@balooga.com>"
    :licence "MIT"
    :depends-on (cffi trivial-garbage lispbuilder-sdl-cffi lispbuilder-sdl-base)
    :components
    ((:module "sdl"
	      :components
	      ((:file "package")
	       (:file "globals")
	       (:file "generics")
	       (:file "classes")
	       (:file "util")
	       (:file "bitmap-font-data")
	       (:file "init")
	       (:file "fps")
	       (:file "events")
	       (:file "primitives")
	       (:file "color")
	       (:file "point")
	       (:file "rectangle")
	       (:file "surfaces")
	       (:file "pixel")	       
	       (:file "video")
	       (:file "image")
	       (:file "rwops")
	       (:file "drawing-primitives")
	       (:file "font")
;; 	       (:file "simple-font")
	       (:file "bitmap-font")	       
	       (:file "string-solid")
	       (:file "string-shaded")
	       (:file "keys")
	       (:file "sdl-util")
	       (:file "cffi-finalizers")
	       (:file "default-colors")
;;	       (:static-file "font.bmp")
	       )
	      :serial t)
     (:module "documentation"
	      :components
	      ((:html-file "lispbuilder-sdl")
	       (:html-file "footer")
	       (:html-file "header")
	       (:static-file "sdl-alien.png")
	       (:doc-file "COPYING")))))
