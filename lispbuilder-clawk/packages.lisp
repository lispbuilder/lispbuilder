;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10 -*-

(defpackage LISPBUILDER-CLAWK
#+:Genera (:use COMMON-LISP CLOS LISPBUILDER-REGEX)
#-:Genera (:use COMMON-LISP LISPBUILDER-REGEX)
#+:Genera (:import-from "SCL" DEFINE-SYMBOL-MACRO)
  (:export
    ;; install the #/../ reader
    "INSTALL-REGEX-SYNTAX"
    ;; convert any accepted representation of a pattern into a compiled pattern
    "GET-MATCHER-FOR-PATTERN"
    ;; Specials
    "*CURFILE*" "*CURLINE*" "*FS*" "*RSTART*" "*RLENGTH*" "*REND*"
    "*REGS*" "*FIELDS*"
    "*NR*" "*FNR*" "*NF*" "*SUBSEP*"
    "*LAST-MATCH*" "*LAST-SUCCESSFUL-MATCH*"
    ;; AWK-like functions
    "SUB" "GSUB" "SPLIT" "INDEX" "MATCH" "SUBSTR"
    "$SUB" "$GSUB" "$SPLIT" "$INDEX" "$MATCH" "$SUBSTR"
    "$SUBSTR" "~" "/~" "!~"
    ;; Handy macros
    "WITH-PATTERNS" "WITH-FIELDS" "WITH-REGS" "WITH-SUBMATCHES"
    "IF-MATCH" "WITH-MATCH" "MATCH-CASE" "MATCH-WHEN"
    "TOKENIZE"
    ;; iterate across a stream or file, evaluating the body for each
    ;; line
    "FOR-STREAM-LINES" "FOR-FILE-LINES"
    ;; iterate across a stream or file, splitting the lines and
    ;; evaluating the body for each line
    "FOR-STREAM-FIELDS" "FOR-FILE-FIELDS"
    ;; iterate across the lines in a stream or file, splitting the lines
    ;; and evaluating the AWK-like clauses for each line.
    "WHEN-STREAM-FIELDS" "WHEN-FILE-FIELDS"
    ;; define a function on a set of files that closely mimics the
    ;; structure of an AWK program.
    "DEFAWK"
    ;; handy generic functions
    "$+" "$-" "$*" "$/" "$REM" "$++" "$==" "$<" "$>" "$<=" "$>=" "$/="
    "$MIN" "$MAX" "$ZEROP" "$LENGTH"
    ;; arithmetic functions
    "$ATAN2" "$COS" "$SIN" "$EXP" "$EXPT" "$INT" "$LOG" "$SQRT" "$SRAND" "$RAND"
    ;; AWK-like I/O
    "$PRINT" "$FPRINT"
    ;; hashtable-based "arrays"
    "$ARRAY" "$AREF" "$FOR" "$IN" "$DELETE"
    ;; Fields access. These don't follow the *..* convention, but they
    ;; still stand out visually, so I think the goal of that convention
    ;; is still met
    "$N" "$0" "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" "$9" "$10"
    "$11" "$12" "$13" "$14" "$15" "$16" "$17" "$18" "$19" "$20"
    ;; Fields access, as numbers
    "$#N" "$#0" "$#1" "$#2" "$#3" "$#4" "$#5" "$#6" "$#7" "$#8" "$#9" "$#10"
    "$#11" "$#12" "$#13" "$#14" "$#15" "$#16" "$#17" "$#18" "$#19" "$#20"
    ;; Coercion routines, although the generic functions above reduce
    ;; the need for them.
    "STR" "NUM" "INT"
    ;; Register access.
    "%N" "%0" "%1" "%2" "%3" "%4" "%5" "%6" "%7" "%8" "%9"
    "%10" "%11" "%12" "%13" "%14" "%15" "%16" "%17" "%18" "%19" "%20"
    ;; Register access, as numbers
    "%#N" "%#0" "%#1" "%#2" "%#3" "%#4" "%#5" "%#6" "%#7" "%#8" "%#9"
    "%#10" "%#11" "%#12" "%#13" "%#14" "%#15" "%#16" "%#17" "%#18" "%#19" "%#20"
    ))

(defpackage LISPBUILDER-CLAWK-USER
  (:use COMMON-LISP LISPBUILDER-CLAWK))

(defpackage LISPBUILDER-CLAWK-TEST
  (:use COMMON-LISP LISPBUILDER-CLAWK))



(defun delete-clawk ()
  (delete-package :LISPBUILDER-CLAWK-TEST)
  (delete-package :LISPBUILDER-CLAWK))
