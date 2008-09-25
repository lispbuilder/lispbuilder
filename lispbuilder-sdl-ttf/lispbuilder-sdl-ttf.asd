;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-ttf-system
  (:use #:cl #:asdf))

(in-package #:lispbuilder-sdl-ttf-system)

(defsystem lispbuilder-sdl-ttf
  :description "lispbuilder-sdl-ttf: SDL_ttf 2.0.9 library wrapper and tools"
  :long-description
  "lispbuilder-sdl-ttf is a wrapper for the SDL_ttf 2.0.9 library."
  :version "0.2.4"
  :author "Luke J Crook <luke@balooga.com>"
  :maintainer "Application Builder <application-builder@lispniks.com>, Luke J Crook <luke@balooga.com>"
  :licence "BSD"
  :depends-on (cffi lispbuilder-sdl lispbuilder-sdl-ttf-cffi)
  :perform (load-op :after (op lispbuilder-sdl-ttf)
		    (pushnew :lispbuilder-sdl-ttf *features*))
  :components
  ((:module "sdl-ttf"
	    :components
	    ((:file "package")
	     (:file "generics" :depends-on ("package"))
	     (:file "globals" :depends-on ("package"))
	     (:file "font" :depends-on ("package" "generics" "globals"))
	     (:file "sdl-util-ttf" :depends-on ("font" "globals" "package"))
	     (:file "string-solid" :depends-on ("sdl-util-ttf"))
	     (:file "string-shaded" :depends-on ("sdl-util-ttf"))
	     (:file "string-blended" :depends-on ("sdl-util-ttf"))
	     (:static-file "bitstream-vera-copyright")
	     (:static-file "Vera.ttf")))
   (:module "documentation"
	    :components
	    ((:html-file "header")
	     (:html-file "footer")
	     (:html-file "lispbuilder-sdl-ttf")))))
