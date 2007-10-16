;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10 -*-

(defpackage LISPBUILDER-REGEX
 #+:Genera (:use COMMON-LISP CLOS)
 #-:Genera (:use COMMON-LISP)
  (:export
    ;; compiler
    "PARSE-STR"
    "COMPILE-STR" "COMPILE-EXPR"
    "MACROEXPAND-REGEX-STR" "MACROEXPAND-REGEX-EXPR"
    "DEFREGEX"
    ;; match/scan
    "MATCH-STR" "SCAN-STR"
    ;; matcher structure
    "MATCHER" "MATCHER-P" "MATCHER-MATCHFN" "MATCHER-NUMREGS"
    ;; Accessors into match registers.  Overloaded on string, symbol,
    ;; matcher, and register array.
    "REGISTER-MATCHED-P" "REGISTER-START" "REGISTER-END" "MAKE-REGS"
    ;; housekeeping
    "CLEAR-PATTERN-CACHE"
    ;; tweak the syntax, speed, etc.
    "COMPILE-VERBOSE"
    "MATCH-SIMPLE-STRINGS-ONLY"
    "ESCAPE-SPECIAL-CHARS"
    "FORCE-SAFE-MATCH"
    "DOT-MATCHES-NEWLINE"
    "ALLOW-BACKMATCH"
    "ALLOW-RANGEMATCH"
    "ALLOW-NONGREEDY-QUANTIFIERS"
    "ALLOW-NONREGISTER-GROUPS"
    "REGISTERS-MATCH-RIGHTMOST"
    ;; regex as sexpr
    "MAKE-CHAR-NODE" "CHAR-NODE-P"
                     "CHAR-NODE-CHAR"
    "MAKE-STRING-NODE" "STRING-NODE-P"
                     "STRING-NODE-STRING"
    "MAKE-BACKMATCH-NODE" "BACKMATCH-NODE-P"
                     "BACKMATCH-NODE-REGNUM"
    "MAKE-SEQ-NODE-LIST" "MAKE-SEQ-NODE-ARGS" "SEQ-NODE-P"
                     "SEQ-NODE-CHILDREN"
                     "SEQ-NODE-NUMCHILDREN"
                     "SEQ-NODE-CHILD"
    "MAKE-KLEENE-NODE" "KLEENE-NODE-P"
                     "KLEENE-NODE-GREEDY-P" "KLEENE-NODE-NONGREEDY-P"
                     "KLEENE-NODE-CHILD"
    "MAKE-PKLEENE-NODE" "PKLEENE-NODE-P"
                     "PKLEENE-NODE-GREEDY-P" "PKLEENE-NODE-NONGREEDY-P"
                     "PKLEENE-NODE-CHILD"
    "MAKE-OPTIONAL-NODE" "OPTIONAL-NODE-P"
                     "OPTIONAL-NODE-GREEDY-P" "OPTIONAL-NODE-NONGREEDY-P"
                     "OPTIONAL-NODE-CHILD"
    "MAKE-RANGE-NODE" "RANGE-NODE-P"
                     "RANGE-NODE-GREEDY-P" "RANGE-NODE-NONGREEDY-P"
                     "RANGE-NODE-MIN" "RANGE-NODE-MAX"
                     "RANGE-NODE-CHILD"
    "MAKE-ALT-NODE-LIST" "MAKE-ALT-NODE-ARGS" "ALT-NODE-P"
                     "ALT-NODE-CHILDREN"
                     "ALT-NODE-NUMCHILDREN"
                     "ALT-NODE-FIRST"
                     "ALT-NODE-SECOND"
                     "ALT-NODE-CHILD"
    "MAKE-REGISTER-NODE" "REGISTER-NODE-P"
                    "REGISTER-NODE-REGNUM"
                    "REGISTER-NODE-CHILD"
    "MAKE-REGSTART-NODE" "REGSTART-NODE-P" "REGSTART-NODE-REGNUM"
    "MAKE-REGEND-NODE" "REGEND-NODE-P" "REGEND-NODE-REGNUM"
    "MAKE-CHARCLASS-NODE" "CHARCLASS-NODE-P" "CHARCLASS-NODE-NEGATED-P"
                    "CHARCLASS-NODE-CHARS"
    "MAKE-SPECCLASS-NODE" "SPECCLASS-NODE-P" "SPECCLASS-NODE-NEGATED-P"
                    "SPECCLASS-NODE-CLASS"
    "MAKE-START-ANCHOR-NODE" "START-ANCHOR-NODE-P"
    "MAKE-END-ANCHOR-NODE" "END-ANCHOR-NODE-P"
    "MAKE-ANY" "ANY-P"
    "MAKE-HOOK-NODE" "HOOK-NODE-P"
                    "HOOK-NODE-FUNCTION"
                    "HOOK-NODE-SYMBOL-P" "HOOK-NODE-FUNCTION-P" "HOOK-NODE-INDEX-P"
    "MAKE-SUCCESS-NODE" "SUCCESS-NODE-P"
                    "SUCCESS-NODE-RC"
    ;; special char classes
    "ALPHA" "UPPER" "LOWER" "DIGIT" "ALNUM" "XDIGIT" "ODIGIT" "PUNCT" "SPACE"
   ))

(defpackage LISPBUILDER-REGEX-TEST
  (:use COMMON-LISP LISPBUILDER-REGEX))

(defun delete-lispbuilder-regex ()
  (delete-package :LISPBUILDER-REGEX-TEST)
  (delete-package :LISPBUILDER-REGEX))









