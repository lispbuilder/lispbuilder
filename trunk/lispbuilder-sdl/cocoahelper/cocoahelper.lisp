;;(require 'cffi)

(defpackage :lispbuilder-sdl-cocoahelper
 (:use :common-lisp :cffi))

(in-package :lispbuilder-sdl-cocoahelper)
(push (merge-pathnames (pathname "cocoahelper/")
                       (asdf:component-pathname (asdf:find-system :cocoahelper)))
      *foreign-library-directories*)
(push (merge-pathnames (pathname "cocoahelper/")
                       (asdf:component-pathname (asdf:find-system :cocoahelper)))
      *darwin-framework-directories*)

(define-foreign-library cocoahelper
  (:darwin (:or (:framework "cocoahelper")
                "cocoahelper.dylib")))
(use-foreign-library cocoahelper)

(defcfun "cocoahelper_init" :void)
(cocoahelper-init)
