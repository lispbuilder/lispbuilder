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
	       (:file "sdl-util")
	       (:file "cffi-finalizers")
;;	       (:static-file "font.bmp")
	       )
	      :serial t)
     (:module "documentation"
	      :components
	      ((:html-file "index")
	       (:html-file "lispbuilder-sdl")
	       (:html-file "footer")
	       (:html-file "header")
	       (:static-file "sdl1.png")
	       (:static-file "groovy1.png")
	       (:static-file "sdl-gfx-examples_bezier.png")
	       (:static-file "sdl-gfx-examples_distance-2d.png")
	       (:static-file "sdl-gfx-examples_functions.png")
	       (:static-file "sdl-gfx-examples_objects.png")
	       (:static-file "sdl-gfx-examples_points-and-lines.png")
	       (:static-file "sdl-gfx-examples_random-circles.png")
	       (:static-file "sdl-gfx-examples_recursion.png")
	       (:static-file "sdl-gfx-examples_shape-primitives.png")
	       (:static-file "sdl-gfx-examples_vertices.png")
	       (:static-file "sdl-gfx-examples_width-height.png")
	       (:doc-file "COPYING")))))
