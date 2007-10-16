;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10 -*-

(defpackage "LISPBUILDER-LEXER"
#+:Genera  (:use COMMON-LISP CLOS LISPBUILDER-REGEX)
#-:Genera  (:use COMMON-LISP LISPBUILDER-REGEX)
  (:export "TOKENIZE" "DEFLEXER"
   "%N" "%0" "%1" "%2" "%3" "%4" "%5" "%6" "%7" "%8" "%9" "%10"
   "NUM" "INT" "NEXTCH" "UNGETCH"
   ))

(defun delete-lexer ()
  (delete-package :LISPBUILDER-LEXER))

