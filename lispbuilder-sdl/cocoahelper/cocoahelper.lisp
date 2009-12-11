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
                (:default "cocoahelper"))))
(use-foreign-library cocoahelper)


#+darwin(defcfun "cocoahelper_init" :void)
#+darwin(cocoahelper-init)

