;;; -*-Lisp-*-

(asdf:defsystem #:lispbuilder-lexer
  :name "lispbuilder-lexer"
  :author "Michael Parker <mparker762@hotmail.com>"
  :licence "BSD"
  :description "A lexical-analyzer-generator called DEFLEXER, which is built on top of both REGEX and CLAWK"
  
  :components
  ((:file "packages")
   (:file "lexer" :depends-on ("packages"))))
