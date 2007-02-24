;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-gfx-cffi-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-gfx-cffi-system)

(defsystem lispbuilder-sdl-gfx-cffi
    :description "lispbuilder-sdl-gfx-cffi: SDL_gfx v2.0.13 library wrapper and tools"
    :long-description
    "lispbuilder-sdl-gfx is a wrapper for the SDL_gfx v2.0.13 library. 
    The SDL_gfx library extends the base functionality implemented by SDL
    and provides Graphic Primitives, Rotozoomimg, Framerate control, and 
    MMX image filters. The wrapper is implemeted using CFFI to be highly 
    compatible across lisp implementations."
    :version "0.5.0"
    :author "Luke Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "MIT"
    :depends-on (cffi lispbuilder-sdl-cffi)
    :components
    ((:module "cffi"
	      :components
	      ((:file "package")
	       (:file "library" :depends-on ("package"))
	       (:file "gfx" :depends-on ("package" "library")))
	      :serial t)
     (:module "documentation"
	      :components
	      ((:html-file "index")
	       (:static-file "sdl_gfx1.jpg")
	       (:doc-file "README")
	       (:doc-file "COPYING")
	       (:doc-file "CONTRIBUTORS")))
     (:module "build"
	      :components
	      ((:static-file "sdlgfxswig.i")))))
