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
  :version "0.9.6"
  :author "Justin Heyes-Jones <justinhj@gmail.com>, Luke J Crook <luke@balooga.com>"
  :maintainer "Application Builder <application-builder@lispniks.com>, Luke J Crook <luke@balooga.com>"
  :licence "MIT"
  :depends-on (cffi trivial-garbage lispbuilder-sdl-cffi lispbuilder-sdl-base)
  :perform (load-op :after (op lispbuilder-sdl)
		    (pushnew :lispbuilder-sdl *features*))
  :components
  ((:module "sdl"
	    :components
	    ((:file "package")
	     (:file "globals")
	     (:file "generics")
	     (:file "base")
	     (:file "util")
	     (:file "bitmap-font-data")
	     (:file "init")
	     (:file "input-util")
	     (:file "fps")
	     (:file "events")
	     (:file "primitives")
	     (:file "color")
	     (:file "point")
	     (:file "rectangle")
	     (:file "surfaces")
	     (:file "pixel")	       
	     (:file "video")	     
	     (:file "rwops")
	     (:file "image")
	     (:file "drawing-primitives")
	     (:file "font")
             ;; (:file "simple-font")
	     (:file "bitmap-font")	       
	     (:file "string-solid")
             (:file "string-shaded")
             (:file "string-blended")
	     (:file "keys")
	     (:file "sdl-util")
	     (:file "default-colors"))
	    :serial t)
   (:module "assets"
	      :components
	      ((:file "globals")
	       (:static-file "bitstream-vera-copyright")
	       (:static-file "Vera.ttf")))
   (:module "documentation"
	    :components
	    ((:html-file "lispbuilder-sdl")
	     (:html-file "footer")
	     (:html-file "header")
	     (:static-file "sdl-alien.png")
	     (:doc-file "COPYING")))))
