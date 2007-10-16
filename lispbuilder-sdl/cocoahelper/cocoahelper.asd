;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-cocoahelper-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-cocoahelper-system)

(defsystem cocoahelper;; lispbuilder-sdl-cocoahelper
    :description "cocoahelper system for SDL"
    :version "0.1"
    :author "Brad Beveridge"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "BSD"
    :depends-on (cffi)
    :components
    ((:file "cocoahelper")))
