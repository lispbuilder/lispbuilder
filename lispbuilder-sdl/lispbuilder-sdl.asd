;;; -*- lisp -*-

(defpackage lispbuilder-sdl-system
  (:use :common-lisp :asdf :cffi))

(in-package lispbuilder-sdl-system)

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
    :components
    ((:doc-file "README")
     (:file "packages")
     (:file "sdl")
     (:file "util")))

