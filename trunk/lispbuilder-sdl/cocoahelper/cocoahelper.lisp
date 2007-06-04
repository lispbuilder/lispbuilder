;;(require 'cffi)

(defpackage :lispbuilder-sdl-cocoahelper
 (:use :common-lisp :cffi))

(in-package :lispbuilder-sdl-cocoahelper)
(push (asdf:component-pathname (asdf:find-system :cocoahelper)) *foreign-library-directories*)

(define-foreign-library cocoahelper
  (t (:default "cocoahelper")))
(use-foreign-library cocoahelper)

(defcfun "cocoahelper_init" :void)
(cocoahelper-init)
