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
	     (:file "generics")
	     (:file "globals")
             (:file "ttf-font-definition")
             (:file "ttf-font-data")
	     (:file "font")
	     (:file "sdl-util-ttf")
	     (:file "string-solid")
	     (:file "string-shaded")
	     (:file "string-blended"))
            :serial t)
   (:module "documentation"
	    :components
	    ((:html-file "header")
	     (:html-file "footer")
	     (:html-file "lispbuilder-sdl-ttf")))))
