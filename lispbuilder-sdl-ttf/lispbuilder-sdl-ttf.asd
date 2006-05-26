;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-ttf-system
  (:use #:cl #:asdf))

(in-package #:lispbuilder-sdl-ttf-system)

(defsystem lispbuilder-sdl-ttf
    :description "lispbuilder-sdl-ttf: SDL_ttf 2.0 library wrapper and tools"
    :long-description
    "lispbuilder-sdl-ttf is a wrapper for the SDL_ttf 2.0 library."
    :version "0.0.1"
    :author "Rune Nesheim <rune.nesheim@gmail.com>"
    :maintainer "Rune Nesheim <rune.nesheim@gmail.com>"
    :licence "BSD"
    :depends-on (cffi lispbuilder-sdl)
    :components
    ((:module "sdl-ttf"
	      :components
	      ((:file "package")
	       (:file "library" :depends-on ("package"))
	       (:file "sdl_ttf" :depends-on ("package" "library"))
	       (:file "translate" :depends-on ("sdl_ttf"))
	       (:file "util-sdl_ttf" :depends-on ("sdl_ttf" "translate"))))))
;     (:module "documentation"
;	      :components
;	      ((:html-file "index")
;            (:static-file "sdl_gfx1.jpg")
;		 (:doc-file "README")
;	       (:doc-file "COPYING")
;	       (:doc-file "CONTRIBUTORS")))
;     (:module "build"
;	      :components
;	      ((:static-file "sdlgfxswig.i")))))

