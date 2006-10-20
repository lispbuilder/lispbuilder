;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-gfx-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-gfx-system)

(defsystem lispbuilder-sdl-gfx
    :description "lispbuilder-sdl-gfx: SDL_gfx v2.0.13 library wrapper and tools"
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
    :depends-on (cffi lispbuilder-sdl)
    :components
    ((:module "sdl-gfx"
	      :components
	      ((:file "package")
	       (:file "library" :depends-on ("package"))
	       (:file "sdl_gfx" :depends-on ("package" "library"))
	       (:file "util-sdl_gfx" :depends-on ("sdl_gfx")))
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

