
;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-cocoahelper-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-cocoahelper-system)

(defsystem cocoahelper;; lispbuilder-sdl-cocoahelper
  :description "cocoahelper system for SDL"
  :version "0.1"
  :author "Lispbuilder Mailing List <lispbuilder@googlegroups.com>"
  :maintainer "Lispbuilder Mailing List <lispbuilder@googlegroups.com>"
  :licence "BSD"
  :depends-on (cffi lispbuilder-sdl-binaries)
  :components ((:module "cocoahelper"
                :components ((:file "cocoahelper")))))
