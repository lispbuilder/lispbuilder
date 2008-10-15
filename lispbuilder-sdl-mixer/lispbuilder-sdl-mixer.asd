;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-mixer-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-mixer-system)

(defsystem lispbuilder-sdl-mixer
  :description "lispbuilder-sdl-mixer: SDL_mixer v1.2.8 library wrapper and tools"
  :long-description
  "lispbuilder-sdl-mixer uses CFFI to be highly compatible across lisp 
    implementations. It includes a selection of utilities to assist  
    game programming in Common Lisp."
  :version "0.3"
  :author "Luke Crook <luke@balooga.com>"
  :maintainer "Application Builder <application-builder@lispniks.com>"
  :licence "MIT"
  :depends-on (cffi lispbuilder-sdl-mixer-cffi)
  :perform (load-op :after (op lispbuilder-sdl-mixer)
		    (pushnew :lispbuilder-sdl-mixer *features*))
  :components
  ((:module "sdl-mixer"
	    :components
	    ((:file "package")
	     (:file "globals" :depends-on ("package"))
	     (:file "mixer" :depends-on ("globals"))))
   (:module "documentation"
	    :components
	    ((:doc-file "README")
	     (:doc-file "COPYING")
	     (:doc-file "CONTRIBUTORS")
	     (:html-file "lispbuilder-sdl-mixer")
	     (:html-file "footer")
	     (:html-file "header")))
   (:module "build"
	    :components
	    ((:static-file "sdlmixerswig.i")))))
