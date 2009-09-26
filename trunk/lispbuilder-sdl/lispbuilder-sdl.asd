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
  :version "0.9.8"
  :author "Justin Heyes-Jones <justinhj@gmail.com>, Luke J Crook <luke@balooga.com>"
  :maintainer "Application Builder <application-builder@lispniks.com>, Luke J Crook <luke@balooga.com>"
  :licence "MIT"
  :depends-on (cffi trivial-garbage lispbuilder-sdl-base lispbuilder-sdl-assets)
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
     (:file "init")
     (:file "mouse")
     (:file "input-util")
     (:file "fps")
     (:file "events")
     (:file "primitives")
     (:file "color")
     (:file "point")
     (:file "rectangle")
     (:file "pixel")
     (:file "surfaces")
     (:file "video")
     (:file "rwops")
     (:file "image")
     (:file "drawing-primitives")
     (:file "font-definition")
     (:file "bitmap-font-definition")
     (:file "simple-font-definition")
     (:file "font")
     (:file "bitmap-font-data-5x7")
     (:file "bitmap-font-data-5x8")
     (:file "bitmap-font-data-6x9")
     (:file "bitmap-font-data-6x10")
     (:file "bitmap-font-data-6x12")
     (:file "bitmap-font-data-6x13")
     (:file "bitmap-font-data-6x13b")
     (:file "bitmap-font-data-6x13o")
     (:file "bitmap-font-data-7x13")
     (:file "bitmap-font-data-7x13b")
     (:file "bitmap-font-data-7x13o")
     (:file "bitmap-font-data-7x14")
     (:file "bitmap-font-data-7x14b")
     (:file "bitmap-font-data-8x8")
     (:file "bitmap-font-data-8x13")
     (:file "bitmap-font-data-8x13b")
     (:file "bitmap-font-data-8x13o")
     (:file "bitmap-font-data-9x15")
     (:file "bitmap-font-data-9x15b")
     (:file "bitmap-font-data-9x18")
     (:file "bitmap-font-data-9x18b")
     (:file "bitmap-font-data-10x20")
     (:file "simple-font-data")
     (:file "bitmap-font")
     (:file "simple-font")
     (:file "string-solid")
     (:file "string-shaded")
     (:file "string-blended")
     (:file "keys")
     (:file "sdl-util")
     (:file "default-colors")
     (:file "audio")
     (:file "mixer")
     (:file "active")
     (:static-file "bitstream-vera-copyright")
     (:static-file "Vera.ttf"))
    :serial t)
   (:module "documentation"
    :components
    ((:html-file "lispbuilder-sdl")
     (:html-file "footer")
     (:html-file "header")
     (:static-file "sdl-alien.png")
     (:doc-file "COPYING")))))
