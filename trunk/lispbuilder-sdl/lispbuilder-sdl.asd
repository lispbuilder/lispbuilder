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
    :version "0.1"
    :author "Justin Heyes-Jones <justinhj@gmail.com.com>"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "BSD"
    :depends-on (cffi)
    :components
;    ((:doc-file "README")
;     (:doc-file "COPYING")
;     (:doc-file "CONTRIBUTORS")
;     (:static-file "sdlswig.i")
;     (:html-file "index")
    ((:module "sdl"
	      :components
	      ((:file "package")
	       (:file "sdl")
	       (:file "util-sdl" :depends-on ("sdl"))))))

;)

