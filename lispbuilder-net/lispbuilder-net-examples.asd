(defpackage #:lispbuilder-net-examples-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-net-examples-system)

(defsystem lispbuilder-net-examples
  :description "Examples for the lispbuilder-net package."
  :author "Lispbuilder Mailing List <lispbuilder@googlegroups.com>"
  :maintainer "Lispbuilder Mailing List <lispbuilder@googlegroups.com>"
  :licence "MIT"
  :depends-on (cffi lispbuilder-net)
  :components
  ((:module "examples"
    :components
    ((:file "package")
     (:file "wget" :depends-on ("package"))
     (:file "webserver" :depends-on ("package"))))))
