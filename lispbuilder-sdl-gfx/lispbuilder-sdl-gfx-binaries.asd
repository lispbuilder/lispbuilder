;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-gfx-binaries-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-gfx-binaries-system)

(defsystem lispbuilder-sdl-gfx-binaries
  :description "lispbuilder-sdl-gfx-binaries: The windows binary for the SDL_gfx v2.0.13 library"
  :version "2.0.13"
  :author "Andreas Schiffler <aschiffler@appwares.com>"
  :maintainer "Application Builder <application-builder@lispniks.com>"
  :licence "LGPL"
  :components
  ((:module "bin"
    :components
    ((:file "package")
     (:file "globals")
     ;;(:static-file "SDL_gfx.dll")
     ))
   ;;(:module "documentation"
   ;;	      :components
   ;;	      ((:doc-file "bin_README")
   ;;	       (:doc-file "bin_LICENSE")))
   ))
