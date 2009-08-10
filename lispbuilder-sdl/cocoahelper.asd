
;;; -*- lisp -*-

#+(or darwin macos macosx)
(defpackage #:lispbuilder-sdl-cocoahelper-system
  (:use #:cl #:asdf))
#+(or darwin macos macosx)
(in-package #:lispbuilder-sdl-cocoahelper-system)

#+(or darwin macos macosx)
(defsystem cocoahelper;; lispbuilder-sdl-cocoahelper
  :description "cocoahelper system for SDL"
  :version "0.1"
  :author "Brad Beveridge"
  :maintainer "Application Builder <application-builder@lispniks.com>"
  :licence "BSD"
  :depends-on (cffi)
  :components ((:module "cocoahelper"
                :components ((:file "cocoahelper")))))
