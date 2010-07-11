;;(require 'cffi)

(defpackage :lispbuilder-sdl-cocoahelper
 (:use :common-lisp :cffi))

(in-package :lispbuilder-sdl-cocoahelper)
(pushnew
 (merge-pathnames (pathname "cocoahelper/")
                  (asdf:component-pathname (asdf:find-system :cocoahelper)))
 *foreign-library-directories*)
(pushnew
 (merge-pathnames (pathname "cocoahelper/")
                  (asdf:component-pathname (asdf:find-system :cocoahelper)))
 *darwin-framework-directories*)
(pushnew sdl-bin:*dll-path* *foreign-library-directories*)
(pushnew sdl-bin:*dll-path* *darwin-framework-directories*)

(define-foreign-library cocoahelper
  (:darwin (:or (:framework "cocoahelper")
                (:default "cocoahelper"))))
(use-foreign-library cocoahelper)


#+darwin(defcfun "cocoahelper_init" :void)
#+darwin(cocoahelper-init)

