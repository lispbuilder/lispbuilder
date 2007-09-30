;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; trivial-garbage.asd --- ASDF system definition for trivial-garbage.
;;;
;;; This software is placed in the public domain by Luis Oliveira
;;; <loliveira@common-lisp.net> and is provided with absolutely no
;;; warranty.

#-(or :cmu :sbcl :allegro :clisp :openmcl :corman :lispworks)
(error "Sorry, your Lisp is not supported by trivial-garbage.")

(defpackage #:trivial-garbage-system
  (:use #:cl #:asdf))
(in-package #:trivial-garbage-system)

(defsystem trivial-garbage
  :description "Portable finalizers, weak hash-tables and weak pointers."
  :author "Luis Oliveira <loliveira@common-lisp.net>"
  :version "0.15"
  :licence "Public Domain"
  :components
  ((:module "trivial-garbage"
            :components
            ((:file "trivial-garbage")))))

(defmethod perform ((op test-op) (sys (eql (find-system :trivial-garbage))))
  (operate 'load-op :trivial-garbage-tests)
  (operate 'test-op :trivial-garbage-tests))

(defsystem trivial-garbage-tests
  :description "Unit tests for TRIVIAL-GARBAGE."
  :depends-on (trivial-garbage rt)
  :components
  ((:module "trivial-garbage"
            :components
            ((:file "tests")))))

(defmethod perform ((op test-op)
                    (sys (eql (find-system :trivial-garbage-tests))))
  (funcall (find-symbol (symbol-name '#:do-tests) '#:regression-test)))

;; vim: ft=lisp et