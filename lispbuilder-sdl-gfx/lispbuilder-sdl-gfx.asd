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
    :version "0.1"
    :author "Luke Crook <luke@balooga.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "BSD"
    :depends-on (cffi lispbuilder-sdl)
    :components
    ((:module "sdl-gfx"
	      :components
	      ((:doc-file "README")
	       (:file "package")
	       (:file "sdl_gfx")
	       (:file "util-sdl_gfx" :depends-on ("sdl_gfx"))))))
