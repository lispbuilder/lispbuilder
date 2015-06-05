;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-assets-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-assets-system)

(defsystem lispbuilder-sdl-assets
  :description "lispbuilder-sdl-assets: Assets"
  :version "0.9.8"
  :author "Lispbuilder Mailing List <lispbuilder@googlegroups.com>"
  :maintainer "Lispbuilder Mailing List <lispbuilder@googlegroups.com>"
  :licence "MIT"
  :components
  ((:module "assets"
    :components
    ((:file "package")
     (:file "globals")
     (:static-file "bitstream-vera-copyright")
     (:static-file "Vera.ttf")
     (:static-file "lisp.bmp")
     (:static-file "sdl.bmp")
     (:static-file "particle.bmp")
     (:static-file "particle-alpha.bmp")
     (:static-file "blood.bmp")
     (:static-file "bug.bmp")
     (:static-file "racket.bmp")
     (:static-file "squash.bmp"))
    :serial t)))
