;;; -*-Lisp-*-

(defpackage lispbuilder-windows
  (:use :common-lisp :asdf :cffi))
(in-package :lispbuilder-windows)

(defsystem lispbuilder-windows
  :description "lispbuilder-sdl: Windows library wrapper and tools"
  :version "0.1"
  :author "Frank Buss <fb@frank-buss.de>"
  :maintainer "Common Lisp Application Builder http://www.lispbuilder.org"
  :licence "BSD"
  :depends-on (cffi)
  :components
  ((:module "windows"
    :components
    ((:file "package")
     (:file "windows" :depends-on ("package"))))))
