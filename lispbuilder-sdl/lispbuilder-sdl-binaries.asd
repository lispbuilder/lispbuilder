;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-binaries-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-binaries-system)

(defsystem lispbuilder-sdl-binaries
  :description "lispbuilder-sdl-binaries: The windows binary for the SDL v1.2.12 library"
  :version "1.2.12"
  :author "Sam Lantinga <slouken@libsdl.org>"
  :maintainer "Application Builder <application-builder@lispniks.com>"
  :licence "LGPL"
  ;;    :perform (load-op :after (op lispbuilder-sdl-binaries)
  ;;		    (pushnew :sdl-bin *features*))
  :components
  ((:module "bin"
    :components
    ((:file "package")
     (:file "globals")
     ;;(:doc-file "README-SDL.txt")
     ;;(:static-file "SDL.dll")
     ))
   ;;(:module "documentation"
   ;;	      :components
   ;;	      ((:doc-file "bin_README")
   ;;	       (:doc-file "bin_COPYING")))
   ))
