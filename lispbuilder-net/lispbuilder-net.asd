(defpackage #:lispbuilder-net-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-net-system)

(defsystem lispbuilder-net
  :description "lispbuilder-net: asynchronous sockets library"
  :author "Lispbuilder Mailing List <lispbuilder@googlegroups.com>"
  :maintainer "Lispbuilder Mailing List <lispbuilder@googlegroups.com>"
  :licence "MIT"
  :depends-on (cffi lispbuilder-net-cffi)
  :components
  ((:module "net"
    :components
    ((:file "package")))))
