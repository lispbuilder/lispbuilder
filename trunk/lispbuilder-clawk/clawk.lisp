;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LISPBUILDER-CLAWK; Base: 10 -*-

(in-package :LISPBUILDER-CLAWK)


;;;
;;; change the #/.../ readmacro to generate a regex-matcher object,
;;; and use the make-load-form function to define the loader and initializer
;;;

;;;
;;; reader macro for #/.../ regex symbols
;;;

(defun |#/-reader| (strm subchar arg)
  (declare (ignore subchar arg))
  (let ((symname ""))
    (loop 
     (let ((c (read-char strm)))
       (cond ((char= c #\/)
              (return-from |#/-reader|
                (list 'quote (intern symname *package*))))
             ((char= c #\\)
              (let ((c2 (read-char strm)))
                (cond ((char= c2 #\/)
                       (setq symname (concatenate 'string symname (string #\/))))
                      ((or (char= c2 #\n) (char= c2 #\N)
                           (char= c2 #\r) (char= c2 #\R)
                           (char= c2 #\t) (char= c2 #\T))
                       (setq symname (concatenate 'string symname (string c) (string c2))))
                      ((digit-char-p c2)
                       (setq symname (concatenate 'string symname (string c) (string c2))))
                      (t (setq symname (concatenate 'string symname (string c2)))))))
             ((or (char= c #\Return) (char= c #\Newline))
              (error "Unterminated regular expression: " symname))
             (t (setq symname (concatenate 'string symname (string c)))))))))

(defun install-regex-syntax ()
  (set-dispatch-macro-character #\# #\/ #'|#/-reader|)
  t)

#-:Symbolics
(progn

(defun |#`-reader| (strm subchar arg)
  (declare (ignore subchar arg))
  (let ((cmdname-strm (make-string-output-stream)))
    (loop
     (let ((c (read-char strm)))
       (cond ((char= c #\`)
              (return-from |#`-reader| `(call-system-catching-output ,(get-output-stream-string cmdname-strm))))
             ((char= c #\\)
              (let ((c2 (read-char strm)))
                (cond ((char= c2 #\`)
                       (princ c2 cmdname-strm))
                      (t (princ c cmdname-strm)
                         (princ c2 cmdname-strm)))))
             ((or (char= c #\Return) (char= c #\Newline))
              (error "Unterminated shell command: " (get-output-stream-string cmdname-strm)))
             (t (princ c cmdname-strm)))))))

(defun install-cmd-syntax ()
  (set-dispatch-macro-character #\# #\` #'|#`-reader|)
  t)

)

;(defun test ()
;  (for-stream-lines (#`command.com /c dir`)
;    ($print *FNR* " " $0)))



#+:Lispworks
(progn
  
(defun call-system-catching-output (cmd)
  (let ((cmd-output (make-string-output-stream)))
    (sys:call-system-showing-output cmd
                                    :prefix ""
                                    :output-stream cmd-output
                                    :wait t
                                    :show-cmd nil)
    (make-string-input-stream (get-output-stream-string cmd-output))))

(defun get-shell-pgm ()
  (or (lispworks:environment-variable "COMSPEC")
      (lispworks:environment-variable "SHELL")
      (lispworks:environment-variable "shell")))

)



(defgeneric get-matcher-for-pattern (pattern))
(defmethod get-matcher-for-pattern ((pattern symbol))
  (let ((matcher (get pattern 'regex-matcher)))
    (if matcher
        matcher
      (let ((matcher (compile-str (symbol-name pattern))))
        (if matcher
            (setf (get pattern 'regex-matcher) matcher))
        matcher))))
(defmethod get-matcher-for-pattern ((pattern string))
  (compile-str pattern))
(defmethod get-matcher-for-pattern ((pattern matcher))
  pattern)




(defvar *CURFILE*)
(defvar *CURLINE* "")
(defvar *FS-IS-WS* t)
(defconstant +WS-FIELDSEP-PAT+ "[ \\t]+")
(defvar *FS* +WS-FIELDSEP-PAT+)
(defvar *RSTART* 0)
(defvar *RLENGTH* 0)
(defvar *REND* 0)
(defvar *REGS* (make-array 0))
(defvar *FIELDS* (make-array 0))
(defvar *NR* 0)                         ; total num recs read
(defvar *FNR* 0)                        ; num recs read in current file
(defvar *NF* 0)                         ; number of fields in current record
(defvar *SUBSEP* (string (code-char #o34)))
(defvar *OFS* " ")
(defvar *ORS* "\n")
(defvar *LAST-SUCCESSFUL-MATCH*)

(define-symbol-macro $0 *curline*)
(define-symbol-macro $1 ($n 1))
(define-symbol-macro $2 ($n 2))
(define-symbol-macro $3 ($n 3))
(define-symbol-macro $4 ($n 4))
(define-symbol-macro $5 ($n 5))
(define-symbol-macro $6 ($n 6))
(define-symbol-macro $7 ($n 7))
(define-symbol-macro $8 ($n 8))
(define-symbol-macro $9 ($n 9))
(define-symbol-macro $10 ($n 10))
(define-symbol-macro $11 ($n 11))
(define-symbol-macro $12 ($n 12))
(define-symbol-macro $13 ($n 13))
(define-symbol-macro $14 ($n 14))
(define-symbol-macro $15 ($n 15))
(define-symbol-macro $16 ($n 16))
(define-symbol-macro $17 ($n 17))
(define-symbol-macro $18 ($n 18))
(define-symbol-macro $19 ($n 19))
(define-symbol-macro $20 ($n 20))

(define-symbol-macro $#0 ($#n 0))
(define-symbol-macro $#1 ($#n 1))
(define-symbol-macro $#2 ($#n 2))
(define-symbol-macro $#3 ($#n 3))
(define-symbol-macro $#4 ($#n 4))
(define-symbol-macro $#5 ($#n 5))
(define-symbol-macro $#6 ($#n 6))
(define-symbol-macro $#7 ($#n 7))
(define-symbol-macro $#8 ($#n 8))
(define-symbol-macro $#9 ($#n 9))
(define-symbol-macro $#10 ($#n 10))
(define-symbol-macro $#11 ($#n 11))
(define-symbol-macro $#12 ($#n 12))
(define-symbol-macro $#13 ($#n 13))
(define-symbol-macro $#14 ($#n 14))
(define-symbol-macro $#15 ($#n 15))
(define-symbol-macro $#16 ($#n 16))
(define-symbol-macro $#17 ($#n 17))
(define-symbol-macro $#18 ($#n 18))
(define-symbol-macro $#19 ($#n 19))
(define-symbol-macro $#20 ($#n 20))



(define-symbol-macro %0 (%n 0))
(define-symbol-macro %1 (%n 1))
(define-symbol-macro %2 (%n 2))
(define-symbol-macro %3 (%n 3))
(define-symbol-macro %4 (%n 4))
(define-symbol-macro %5 (%n 5))
(define-symbol-macro %6 (%n 6))
(define-symbol-macro %7 (%n 7))
(define-symbol-macro %8 (%n 8))
(define-symbol-macro %9 (%n 9))
(define-symbol-macro %10 (%n 10))
(define-symbol-macro %11 (%n 11))
(define-symbol-macro %12 (%n 12))
(define-symbol-macro %13 (%n 13))
(define-symbol-macro %14 (%n 14))
(define-symbol-macro %15 (%n 15))
(define-symbol-macro %16 (%n 16))
(define-symbol-macro %17 (%n 17))
(define-symbol-macro %18 (%n 18))
(define-symbol-macro %19 (%n 19))
(define-symbol-macro %20 (%n 20))


(define-symbol-macro %#0 (%#n 0))
(define-symbol-macro %#1 (%#n 1))
(define-symbol-macro %#2 (%#n 2))
(define-symbol-macro %#3 (%#n 3))
(define-symbol-macro %#4 (%#n 4))
(define-symbol-macro %#5 (%#n 5))
(define-symbol-macro %#6 (%#n 6))
(define-symbol-macro %#7 (%#n 7))
(define-symbol-macro %#8 (%#n 8))
(define-symbol-macro %#9 (%#n 9))
(define-symbol-macro %#10 (%#n 10))
(define-symbol-macro %#11 (%#n 11))
(define-symbol-macro %#12 (%#n 12))
(define-symbol-macro %#13 (%#n 13))
(define-symbol-macro %#14 (%#n 14))
(define-symbol-macro %#15 (%#n 15))
(define-symbol-macro %#16 (%#n 16))
(define-symbol-macro %#17 (%#n 17))
(define-symbol-macro %#18 (%#n 18))
(define-symbol-macro %#19 (%#n 19))
(define-symbol-macro %#20 (%#n 20))

(defun FS ()
  (when (stringp *FS*)
    (if (string= *FS* +WS-FIELDSEP-PAT+)
        (setq *FS-IS-WS* t)
      (setq *FS-IS-WS* nil))
    (setq *FS* (get-matcher-for-pattern *FS*)))
  *FS*)




;;;
;;; STR
;;;
(defgeneric str (x))
(defmethod str ((x string))
  x)
(defmethod str ((x (eql nil)))
  "")
(defmethod str ((x number))
  (format nil "~D" x))



;;;
;;; NUM
;;;
(defgeneric num (x))
(defmethod num ((x number))
  x)
(defmethod num ((x (eql nil)))
  0)
(defmethod num ((x string))
  (let ((val (read (make-string-input-stream x))))
    (if (numberp val)
	val
	0)))

;;;
;;; INT
;;;
(defgeneric int (x))
(defmethod int ((x integer))
  x)
(defmethod int ((x number))
  (round x))
(defmethod int ((x (eql nil)))
  0)
(defmethod int ((x string))
  (let ((val (read (make-string-input-stream x) nil 0)))
    (if (numberp val)
	(round val)
	0)))

;;;
;;; SUB
;;;
;;; Substitute the first occurrence of pattern.
;;;
(defun sub (pattern replacement &optional (source *CURLINE*))
  "Replace the first occurrence of pattern in the source string."
  (let ((matcher (get-matcher-for-pattern pattern))
        (srclen (length source)))
    (multiple-value-bind (matchedp matchstart matchlen)
        (lispbuilder-regex:scan-str matcher source :start 0 :length srclen)
      (if matchedp
          (concatenate 'string (subseq source 0 matchstart)
                       replacement
                       (subseq source (+ matchstart matchlen) srclen))
        source))))

(defun $sub (pattern replacement &optional (source *CURLINE*))
  "Replace the first occurrence of pattern in the source string.
   Coerces its arguments to the appropriate type."
  (sub pattern (str replacement) (str source)))


;;;
;;; GSUB
;;;
;;; Globally substitute all occurrences of pattern.
;;;
(defun gsub (pattern replacement &optional (source *CURLINE*))
  "Replace all occurrences of pattern in the source string."
  (let* ((matcher (get-matcher-for-pattern pattern))
         (total-len (length source))
         (len-remaining total-len)
         (new-string "")
         (matchedp t)
         (prior-end 0)
         (match-start 0)
         (match-len 0))
    (loop
      (multiple-value-setq (matchedp match-start match-len)
        (lispbuilder-regex:scan-str matcher source :start match-start :length len-remaining))
      (if matchedp
          (setf new-string (concatenate 'string new-string
                                        (subseq source prior-end match-start)
                                        replacement)
                prior-end (+ match-start match-len)
                match-start prior-end
                len-remaining (- total-len match-start))
        (return-from gsub
          (concatenate 'string
                       new-string
                       (subseq source prior-end total-len)))))))

(defun $gsub (pattern replacement &optional (source *CURLINE*))
  "Replaces all occurrences of pattern in the source string.
   Coerces its arguments to the appropriate type."
  (gsub pattern (str replacement) (str source)))


;;;
;;; SPLIT
;;;
;;; Splits a string up based on an optional field separator pattern
;;; (uses *FS* as a default).  If no string is supplied, it will split
;;; *CURLINE* and set *FIELDS* and the various $n variables.
;;;
(defun split (&optional (source nil not-splitting-curline)
                        (fieldsep-pattern (FS) fieldsep-pattern-p))
  "Split a string up into a list of multiple fields based on
   the field-separator pattern."
  (let ((push-empty-leading-and-trailing-segments
         (or fieldsep-pattern-p
             (not *fs-is-ws*)
             (and (stringp fieldsep-pattern) (string= fieldsep-pattern +WS-FIELDSEP-PAT+)))))
    (when (or (null source) (eq source *CURLINE*))
      (setf source *CURLINE*
            not-splitting-curline nil))
    (let* ((fieldsep-matcher (get-matcher-for-pattern fieldsep-pattern))
           (fields nil)
           (total-len (length source))
           (len-remaining (length source))
           (matchedp t)
           (prior-end 0)
           (match-start 0)
           (match-len 0))
      (loop
       (multiple-value-setq (matchedp match-start match-len)
           (lispbuilder-regex:scan-str fieldsep-matcher source :start match-start :length len-remaining))
       (cond (matchedp
              ;; don't push an empty string if the first thing we
              ;; find is a separator
              (when (or push-empty-leading-and-trailing-segments (> match-start 0))
                (push (subseq source prior-end match-start)
                      fields))
              ;; now step over this match and continue splitting
              (setf prior-end (+ match-start match-len)
                    match-start prior-end
                    len-remaining (- total-len match-start)) )
             (t
              ;; only push an string if we're not at the end.
              (when (or push-empty-leading-and-trailing-segments (< prior-end total-len))
                (push (subseq source prior-end match-len) fields))
              (setf fields (nreverse fields))
              (unless not-splitting-curline
                (setf *FIELDS* (make-array (length fields) :initial-contents fields)))
              (return-from split fields)))))) )
  

(defun $split (&optional (source nil source-supplied-p) (fieldsep-pattern (FS)))
  "Split a string up into a list of multiple fields based on
   the field-separator pattern.  Coerces its arguments to the appropriate type."
  (split (if (and source source-supplied-p) (str source) source)
	 fieldsep-pattern))


;;;
;;; MATCH
;;;
;;; Searches for the first occurrence of a pattern in a string.
;;; Takes an optional offset.  If successful, sets *RSTART*,
;;; *RLENGTH*, *REND*, and *REGS*.
;;;
(defun match (source pattern &optional (start 0))
  "Scan for first occurrence of a pattern within the source string."
  (let ((matcher (get-matcher-for-pattern pattern)))
    (multiple-value-bind (matchedp start len regs)
        (lispbuilder-regex:scan-str matcher source :start start :length (- (length source) start)
                  :start-is-anchor t :end-is-anchor t)
      (when matchedp
        (setf *RSTART* start
              *RLENGTH* len
              *REND* (+ start len)
              *REGS* regs
	      *LAST-SUCCESSFUL-MATCH* source))
      matchedp)))

(defun $match (source pattern &optional (start 0))
  "Scan for first occurrence of a pattern within the source string.
   Coerces its arguments to the appropriate type."
  (match (str source) pattern (int start)))


;;;
;;; INDEX
;;;
;;; Searches for the first occurrence of a substring within a string.
;;; Takes an optional offset.
;;;
(declaim (inline index))
(defun index (pat str &optional (start 0))
  (search pat str :start2 start))

(defun $index (pat str &optional (start 0))
  (index (str pat) (str str) (int start)))


;;;
;;; SUBSTR
;;;
;;; Extract a substring.  Like subseq, but takes length instead of end.
;;;
(declaim (inline substr))
(defun substr (str start len)
  (subseq str start (+ start len)))

(defun $substr (str start len)
  (substr (str str) (int start) (int len)))


;;;
;;; ~ !~ /~
;;;
;;; test if string contains the pattern
;;;
(defgeneric ~ (str pat)
  (:documentation "Test if pattern matches the string."))

(defmethod ~ ((str string) (matcher matcher))
  (multiple-value-bind (matchedp)
      (lispbuilder-regex:scan-str matcher str
                      :start 0 :length (length str)
                      :start-is-anchor t :end-is-anchor t)
    matchedp))

(defmethod ~ ((str string) pat)
  (~ str (get-matcher-for-pattern pat)))

(defmethod ~ ((matcher matcher) (str string))
  (~ str matcher))

(defmethod ~ ((pat symbol) (str string))
  (~ str (get-matcher-for-pattern pat)))


(declaim (inline /~))
(defun /~ (str pat)
  "Test if pattern isn't present in the string."
  (not (~ str pat)))

(declaim (inline !~))
(defun !~ (str pat)
  "Test if pattern isn't present in the string."
  (/~ pat str))



;;;
;;; WITH-PATTERNS
;;;
(defmacro with-patterns (pats &rest body)
  "Execute the body in an environment that includes the compiled patterns
   bound to their respective variables."
  #+:Symbolics (declare (zwei:indentation 1 1))
  (expand-with-patterns pats `(progn ,@body)))
#+:Lispworks (editor:setup-indent "with-patterns" 1 2 6)

(defun expand-with-pattern (var strpat body)
  `(let ((,var (get-matcher-for-pattern ,strpat)))
     (when ,var
       (locally
        (declare (type matcher ,var))
        ,body))) )

(defun expand-with-patterns (pats body)
  (if pats
      (expand-with-pattern (caar pats) (cadar pats)
                           (expand-with-patterns (cdr pats) body))
    body))



;;;
;;; WITH-FIELDS
;;;
;;; Allows stuff like
;;;   (with-fields ((a b c d &rest rest) str)
;;;     ($print "Fields:" a b c d "Rest: " rest))
;;;
;;; as well as the slightly more awk-ish
;;;   (with-fields (nil str)
;;;     ($print "Fields:" $1 $2 $3 $4))
;;; which splits STR into the $ vars,
;;; 
;;; or even
;;;   (with-fields ()
;;;     ($print "Fields:" $1 $2 $3 $4))
;;; which will split the current line into the $ vars.
;;;
(defmacro with-fields ((&optional fields sourcestr (fieldsep-pattern '(FS))) &rest body)
  "Split the source string into fields based on the field separator,
   bind the field array to the fields variable."
  #+:Symbolics (declare (zwei:indentation 1 1))
  (expand-with-fields fields sourcestr fieldsep-pattern body))
#+:Lispworks (editor:setup-indent "with-fields" 1 2 6)

(defun expand-with-fields (fields sourcestr fieldsep-pattern body)
  (let ((tmp-splits (gensym)))
    (if (null fields)
        `(let (*FIELDS*)
           (let* ((,tmp-splits (split ,sourcestr ,fieldsep-pattern))
                  (*NF* (length ,tmp-splits)))
             ,@body))
      `(let* ((,tmp-splits (split ,sourcestr ,fieldsep-pattern))
              (*NF* (length ,tmp-splits)))
         (declare (special *NF*))
         (destructuring-bind ,fields ,tmp-splits
           ,@body)))))



;;;
;;; $
;;;
;;; Field access to a computed field
;;;
(defun $n (n)
  "Access a field."
  (let ((n (int n)))
    (cond ((zerop n) *CURLINE*)
          ((and (>= n 0) (<= n (array-dimension *FIELDS* 0)))
           (aref *FIELDS* (1- n))))))

(defsetf $n (n) (val)
  (let ((tmpvar (gensym)))
    `(let ((,tmpvar (int ,n)))
       (cond ((zerop ,tmpvar) (setf *CURLINE* ,val))
             ((and (>= ,tmpvar 0) (<= ,tmpvar (array-dimension *FIELDS* 0)))
              (setf (aref *FIELDS* (1- ,tmpvar)) ,val))))))


(declaim (inline $#n))
(defun $#n (n)
  "Access a field, as a number."
  (num ($n n)))

(defsetf $#n (n) (val)
  `(setf ($n ,n) (str ,val)))



;;;
;;; WITH-SUBMATCHES
;;;
;;; Allows stuff like
;;;   (with-submatches (a b c d)
;;;     ($print "Regs:" a b c d))
;;;
;;; which is handy in match-case clauses
;;;
(defmacro with-submatches (&optional fields &rest body)
  "Bind the submatch variables to the corresponding strings from the registers array."
  #+:Symbolics (declare (zwei:indentation 1 1))
  (expand-with-submatches fields body))
#+:Lispworks (editor:setup-indent "with-submatches" 1 2 6)

(defun expand-with-submatches (fields body)
  (if fields
      `(destructuring-bind ,fields
           (make-register-list *LAST-SUCCESSFUL-MATCH* *REGS*)
	 ,@body)
      `(progn ,@body)))



;;;
;;; IF-MATCH
;;;
;;; Allows stuff like
;;;   (if-match ("a*b" str)
;;;     ($print "Regs:" %0 %1 %2 %3 %4)
;;;     ($print "No match")
;;;
(defmacro if-match ((pat str &optional (pos 0)) consequent alternative)
"Match the pattern to the string, and if it matches, bind the
*RSTART*, *RLENGTH*, and *REGS* and evaluate the consequent,
otherwise evaluate the alternative."
  #+:Symbolics (declare (zwei:indentation 1 1))
  (expand-if-match pat str pos consequent alternative))
#+:Lispworks (editor:setup-indent "if-match" 2 2 4)


#-:Symbolics
(progn
  (defmacro once-only (variables &rest body)
    "Returns the code built by BODY.  If any of VARIABLES
might have side effects, they are evaluated once and stored
in temporary variables that are then passed to BODY."
    (assert (every #'symbolp variables))
    (let ((temps (loop repeat (length variables) collect (gensym))))
      `(if (every #'side-effect-free-p (list ,@variables))
           (progn ,@body)
         (list 'let
               ,`(list ,@(mapcar #'(lambda (tmp var)
                                     `(list ',tmp ,var))
                                 temps variables))
               (let ,(mapcar #'(lambda (var tmp) `(,var ',tmp))
                             variables temps)
                 ,@body)))))

  (defun side-effect-free-p (exp)
    "Is EXP a constant, variable, or function,
or of the form (THE type x) where x is side-effect-free."
    (or (constantp exp) (atom exp)
      (and (starts-with exp 'the)
           (side-effect-free-p (third exp)))))

  (defun starts-with (lst x)
    (and (consp lst) (eq (first lst) x)))
  )


(defun expand-if-match (pat str pos consequent alternative)
  (once-only (str pos)
    `(multiple-value-bind (matchedp *RSTART* *RLENGTH* *REGS*)
         (lispbuilder-regex:match-str (get-matcher-for-pattern ,pat)
                          ,str :start ,pos)
       (if matchedp
           (let ((*last-successful-match* ,str))
             ,consequent)
         ,alternative))))


;;;
;;; WITH-MATCH
;;;
;;; Allows stuff like
;;;   (with-match ((a b c d &rest rest) pat str)
;;;     ($print "Regs:" a b c d "Rest: " rest))
;;;
;;; as well as the slightly less readable
;;;   (with-match (nil pat str)
;;;     ($print "Regs:" %1 %2 %3 %4))
;;; which matches pat against str, and loads the submatches into the % vars,
;;; 
;;; or even
;;;   (with-match (nil pat)
;;;     ($print "Fields:" %1 %2 %3 %4))
;;; which will split the current line into the % vars.

(defmacro with-match ((&optional fields pat sourcestr) &rest body)
  "Split the source string into registers based on the pattern,
   bind the register variables to the registers array."
 #+:Symbolics (declare (zwei:indentation 1 1))
  (expand-with-match pat fields sourcestr body))
#+:Lispworks (editor:setup-indent "with-match" 1 2 6)

(defun expand-with-match (pat fields sourcestr body)
  (cond ((and pat fields)
	 `(when (match (or ,sourcestr *CURLINE*) ,pat)
	    (destructuring-bind ,fields (make-register-list *LAST-SUCCESSFUL-MATCH* *REGS*)
	      ,@body)))
	((and pat (null fields))
	 `(let (*LAST-SUCCESSFUL-MATCH* *REGS*)
	    (declare (special *LAST-SUCCESSFUL-MATCH* *REGS*))
	    (when (match (or ,sourcestr *CURLINE*) ,pat)
	      ,@body)))
	((and (null pat) fields (null sourcestr))
	 `(destructuring-bind ,fields
	      (make-register-list *LAST-SUCCESSFUL-MATCH* *REGS*)
	    ,@body))
	((and (null pat) (null fields) sourcestr)
	 (error "WITH-MATCH requires a pattern to match the string ~A against" sourcestr))
	((and (null pat) (null fields) (null sourcestr))
	 `(progn ,@body))))

(defun make-register-list (str regs)
  (let ((nregs (array-dimension regs 0)))
    (loop for i from 0 below nregs
	  collect (get-reg-str str regs i))))


;;;
;;; %
;;;
;;; Register access
;;;

(defun get-reg-str (str regs n)
  "Access a register."
  (let ((rstart (register-start regs n))
	(rend (register-end regs n)))
    (if (and (numberp rstart) (numberp rend))
	(subseq str rstart rend))))

(defun %n (n)
  "Access a register."
  (let ((n (int n)))
    (if (and (stringp *LAST-SUCCESSFUL-MATCH*) (< n (array-dimension *REGS* 0)) (>= n 0))
	(get-reg-str *LAST-SUCCESSFUL-MATCH* *REGS* n))))



;;;
;;; MATCH-CASE
;;;
;;; Allows stuff like:
;;; (match-case str
;;;   (#/foo/ (format t "foo"))
;;;   (#/bar/ (format t "bar"))
;;;   ((#/baz/ #/qux/) (format t "either baz or qux"))
;;;   (else (format t "unknown")))
;;;
;;; The matches are done by MATCH, so the various special variables are set
;;; appropriately
;;;
(defmacro match-case (strexpr &rest clauses)
  #+:Symbolics (declare (zwei:indentation 1 1))
  (expand-match-case strexpr clauses))
#+:Lispworks (editor:setup-indent "match-case" 1 2 6)

(defun expand-match-case (strexpr clauses)
  (let ((tmpstrsym (gensym)))
    `(let ((,tmpstrsym ,strexpr))
       ,(expand-match-case-clauses tmpstrsym clauses))))

(defun expand-match-case-clauses (strexpr clauses)
  (if clauses
      (let ((clause (first clauses))
            (other-clauses (rest clauses)))
        (cond ((member (car clause) '(t else otherwise))
               `(progn ,@(rest clause)))
              ((atom (car clause))
               `(if (match ,strexpr ,(car clause))
                    (progn ,@(cdr clause))
                  ,(expand-match-case-clauses strexpr other-clauses)))
              (t `(if (or ,@(mapcar #'(lambda (x) `(,strexpr match ,x)) clause))
                      (progn ,@(cdr clause))
                    ,(expand-match-case-clauses strexpr other-clauses)))))
    `()))




;;;
;;; MATCH-WHEN
;;;
;;; Takes a set of clauses that correspond to the AWK toplevel forms,
;;; but just evaluates the clauses (BEGIN clause first, then pattern
;;; clauses, then END clause), without any looping.
;;;
(defmacro match-when (&rest clauses)
  #+:Symbolics (declare (zwei:indentation 1 1))
  (let ((docs-and-decs (extract-docs-and-decs clauses))
        (begin-clauses (extract-begin-clauses clauses))
        (end-clauses (extract-end-clauses clauses))
        (pattern-clauses (extract-pattern-clauses clauses)))
    `(locally ,@docs-and-decs
              (progn ,@(mapcan #'rest begin-clauses)
                     ,@(mapcar #'expand-match-when-clause pattern-clauses)
                     ,@(mapcar #'rest end-clauses)))))
#+:Lispworks (editor:setup-indent "match-when" 0 2)

(defun is-special-clause (clause type)
  (and (listp clause)
       (symbolp (first clause))
       (string-equal (symbol-name (first clause)) type)))


(defun extract-docs-and-decs (clauses)
  (loop for clause in clauses
        while (or (stringp clause) (is-special-clause clause "DECLARE"))
        collect clause))

(defun extract-begin-clauses (clauses)
  (loop for clause in clauses
        when (is-special-clause clause "BEGIN")
             collect clause))

(defun extract-end-clauses (clauses)
  (loop for clause in clauses
        when (is-special-clause clause "END")
             collect clause))

(defun extract-pattern-clauses (clauses)
  (loop for clause in clauses
        unless (or (not (listp clause))
                   (is-special-clause clause "BEGIN")
                   (is-special-clause clause "END")
                   (is-special-clause clause "DECLARE"))
             collect clause))

;
; (t . body) --> (progn . body)
; (nil . body) --> (progn . body)
; ((quote sym) . body) --> (when (match *CURLINE* (quote sym)) . body)
; (stringlit . body) --> (when (match *CURLINE* stringlit) . body)
; (form . body) --> (when form . body)
;
(defun expand-match-when-clause (clause)
  (cond ((null clause) nil)
        ((stringp (first clause))
         `(when (match *CURLINE* ,(first clause))
            ,@(expand-match-when-consequent (rest clause))))
        ((member (first clause) '("t" "always") :test #'symbol-name-eq)
         (rest clause))
        ((atom (first clause))
         `(when ,(first clause)
            ,@(expand-match-when-consequent (rest clause))))
        (t (let ((condition (first clause))
                 (consequents (rest clause)))
             (if (eq (first condition) 'quote)
                 `(when (match *CURLINE* ,condition)
                    ,@(expand-match-when-consequent consequents))
               `(when ,condition
                  ,@(expand-match-when-consequent consequents)))))))

; provide the default action if one isn't present
(defun expand-match-when-consequent (consequent)
  (if consequent
      consequent
    '(($print *CURLINE*))))

; sym-name-eq
(defun symbol-name-eq (x y)
  (if (symbolp x)
      (if (symbolp y)
          (string= (symbol-name x) (symbol-name y))
        (if (stringp y)
            (string= (symbol-name x) y)))
    (if (stringp x)
        (if (symbolp y)
            (string= x (symbol-name y))
          (if (stringp y)
              (string= x y))))))

;;;
;;; FOR-STREAM-LINES
;;;
;;; Iterate the body over the lines of the stream.  Don't split the
;;; lines, but keep the current line in both *CURLINE* and $0.
;;;
(defmacro for-stream-lines ((stream &optional (strmvar (gensym))
                                              (linevar (gensym)))
                                    &rest body)
  #+:Symbolics (declare (zwei:indentation 1 1))
  (expand-for-stream-lines strmvar linevar stream body))
#+:Lispworks (editor:setup-indent "for-stream-lines" 1 2 6)

(defun expand-for-stream-lines (streamvar linevar stream body
                                          &aux (nextlbl (gensym)))
  `(let ((,streamvar ,stream))
     (when (eq ,streamvar 't)
       (setq ,streamvar *standard-input*))
     (unless (null ,streamvar)
       (let ((*CURFILE* nil)
             (*CURLINE* "")
             (*FNR* -1))
         (macrolet ((next () (list 'throw ',nextlbl nil)))
           (prog ,(if (eq linevar '*CURLINE*) nil (list linevar))
             ,nextlbl
                 (setq ,linevar (read-line ,streamvar nil :eof))
                 (unless (eq ,linevar :eof)
                    (setq *CURLINE* ,linevar
                          $0 ,linevar)
                    (incf *NR*)
                    (incf *FNR*)
                    (catch ',nextlbl
                      ,@body)
                    (go ,nextlbl))))))))



;;;
;;; FOR-FILE-LINES
;;;
;;; Iterate the body over the lines of the file.  Don't split the
;;; lines, but keep the current line in both *CURLINE* and $0.
;;;
;;; Need to do this with prog or labels, and macrolet (next) to jump to
;;; the read-next-line logic.
;;;
(defmacro for-file-lines ((path &optional (streamvar (gensym))
                                          (linevar (gensym)))
                                &rest body)
  #+:Symbolics (declare (zwei:indentation 1 1))
  (expand-for-file-lines streamvar linevar path body))
#+:Lispworks (editor:setup-indent "for-file-lines" 1 2 6)

(defun expand-for-file-lines (streamvar linevar path body)
  `(with-open-file (,streamvar ,path
                             :direction :input
                             :element-type 'character
                             :if-does-not-exist :error)
     ,(expand-for-stream-lines streamvar linevar streamvar body)))


;;;
;;; FOR-STREAM-FIELDS
;;;
;;; Iterate the body over the lines of the stream.  Split the lines
;;; based on *FS*.  Depending on what fieldspec you provide, the
;;; various $n vars may or may not be set (except for $0, which is the
;;; current line).
;;;
;;; As a special case, the value 't will use *standard-input* as the stream.
;;;
(defmacro for-stream-fields ((stream &optional fieldspec
                                              (strmvar (gensym))
                                              (linevar (gensym)))
                                     &rest body)
  #+:Symbolics (declare (zwei:indentation 1 1))
  (expand-for-stream-fields strmvar linevar fieldspec stream body))
#+:Lispworks (editor:setup-indent "for-stream-fields" 1 2 6)

(defun expand-for-stream-fields (strmvar linevar fieldspec stream body
                                         &key (curfile-name stream curfile-name-p)
                                         &aux (nextlbl (gensym)))
  `(let ((,strmvar ,stream))
     (if (eq ,strmvar 't)
         (setq ,strmvar *standard-input*))
     (unless (null ,strmvar)
       (let (,@(if curfile-name-p `((*CURFILE* ,curfile-name)))
             (*CURLINE* "")
             (*FNR* -1))
         (macrolet ((next () (list 'throw ',nextlbl nil)))
           (prog ,(if (eq linevar '*CURLINE*) nil (list linevar))
             ,nextlbl
                 (setq ,linevar (read-line ,strmvar nil :eof))
                 (unless (eq ,linevar :eof)
                    (setq *CURLINE* ,linevar
                          $0 ,linevar)
                    (incf *NR*)
                    (incf *FNR*)
                    (catch ',nextlbl
                      ,(expand-with-fields fieldspec linevar '*FS* body))
                    (go ,nextlbl))))))))



;;;
;;; FOR-FILE-FIELDS
;;;
;;; Open the filepath, then iterate the body over the lines.  Split the lines
;;; based on *FS*.  Depending on what fieldspec you provide, the various $n vars
;;; may or may not be set (except for $0, which is the current line).
;;;
(defmacro for-file-fields ((path &optional fieldspec
                                          (strmvar (gensym))
                                          (linevar (gensym)))
                                 &rest body)
  #+:Symbolics (declare (zwei:indentation 1 1))
  (expand-for-file-fields strmvar linevar fieldspec path body))
#+:Lispworks (editor:setup-indent "for-file-fields" 1 2 6)

(defun expand-for-file-fields (strmvar linevar fieldspec path body)
  `(with-open-file (,strmvar ,path
                             :direction :input
                             :element-type 'character
                             :if-does-not-exist :error)
     ,(expand-for-stream-fields strmvar linevar fieldspec strmvar body :curfile-name path)))




;;;
;;; WHEN-STREAM-FIELDS
;;;
;;; Guts of AWK toplevel for a file, but unlike AWK this can be
;;; used anywhere.
;;;
(defmacro when-stream-fields ((stream &optional fieldspec) &rest clauses)
  #+:Symbolics (declare (zwei:indentation 1 1))
  (let ((docs-and-decs (extract-docs-and-decs clauses))
        (begin-clauses (extract-begin-clauses clauses))
        (end-clauses (extract-end-clauses clauses))
        (pattern-clauses (extract-pattern-clauses clauses)))
    `(locally ,@docs-and-decs
              (progn ,@(mapcan #'rest begin-clauses)
                     (for-stream-fields (,stream ,fieldspec)
                       ,@(mapcar #'expand-match-when-clause pattern-clauses))
                     ,@(mapcan #'rest end-clauses)))))
#+:Lispworks (editor:setup-indent "when-stream-fields" 1 2 6)

(defmacro when-file-fields ((path &optional fieldspec) &rest clauses)
  #+:Symbolics (declare (zwei:indentation 1 1))
  (let ((docs-and-decs (extract-docs-and-decs clauses))
        (begin-clauses (extract-begin-clauses clauses))
        (end-clauses (extract-end-clauses clauses))
        (pattern-clauses (extract-pattern-clauses clauses)))
    `(locally ,@docs-and-decs
              (progn ,@(mapcan #'rest begin-clauses)
                     (for-file-fields (,path ,fieldspec)
                       ,@(mapcar #'expand-match-when-clause pattern-clauses))
                     ,@(mapcan #'rest end-clauses)))))
#+:Lispworks (editor:setup-indent "when-file-fields" 1 2 6)



;;;
;;; Fakes a reasonably close equivalent to a top-level awk program.
;;;
;;;
;;; This needs to be modified to run the BEGIN clauses *ONCE* before
;;; any files are processed, and similarly run the END clauses once
;;; after the files are processed.
;;;

(defmacro defawk (name (&rest parms) &rest clauses)
  (let ((file-or-stream (gensym))
        (process-stream-fn (gensym))
        (strm (gensym))
        (docs-and-decs (extract-docs-and-decs clauses))
        (begin-clauses (extract-begin-clauses clauses))
        (end-clauses (extract-end-clauses clauses))
        (pattern-clauses (extract-pattern-clauses clauses)))
    `(defun ,name (&rest ARGS ,@parms ,@(if (member '&key parms) '((&allow-other-keys))))
       ;; docstrings and declarations
       ,@docs-and-decs
       ;; shadow our globals with their good values
       (let ((*CURFILE*) (*CURLINE* "")
             (*FS* +WS-FIELDSEP-PAT+)
             (*RSTART* 0) (*RLENGTH* 0) (*REND* 0)
             (*REGS* (make-array 0))
             (*FIELDS* (make-array 0))
             (*NR* 0) (*FNR* 0) (*NF* 0)
             (*SUBSEP* (string (code-char #o34)))
             (*OFS* " ") (*ORS* "\n")
             (*LAST-SUCCESSFUL-MATCH*))
         (declare (special *CURFILE* *CURLINE* *FS* *RSTART* *RLENGTH*
                           *REND* *REGS* *FIELDS* *NR* *FNR* *NF*
                           *SUBSEP* *OFS* *ORS* *LAST-SUCCESSFUL-MATCH*))
         (flet ((,process-stream-fn (,strm)
                                    ,(expand-for-stream-fields strm '*CURLINE* nil strm
                                                               (mapcar
                                                                #'expand-match-when-clause
                                                                pattern-clauses))))
           ;; run BEGIN clauses
           ,@(mapcan #'rest begin-clauses)
           ;; process files
           (dolist (,file-or-stream ARGS)
             (cond ((eq ,file-or-stream 't)
                    (,process-stream-fn *standard-input*))
                   ((and (streamp ,file-or-stream) (input-stream-p ,file-or-stream))
                    (,process-stream-fn ,file-or-stream))
                   ((or (pathnamep ,file-or-stream) (stringp ,file-or-stream))
                    (let ((*CURFILE* ,file-or-stream))
                      (declare (special *CURFILE*))
                      (with-open-file (,strm ,file-or-stream
                                             :direction :input
                                             :element-type 'character
                                             :if-does-not-exist :error)
                        (,process-stream-fn ,strm))))))
           ;; run END clauses
           ,@(mapcan #'rest end-clauses))))))
#+:LispWorks (editor:setup-indent "defawk" 1 2)


;;;
;;; misc generic functions
;;;
(defun $+ (&rest rest)
  (reduce #'+ (mapcar #'num rest)))
(defun $- (&rest rest)
  (reduce #'- (mapcar #'num rest)))
(defun $* (&rest rest)
  (reduce #'* (mapcar #'num rest)))
(defun $/ (&rest rest)
  (reduce #'/ (mapcar #'num rest)))
(defun $rem (x y)
  (rem (num x) (num y)))
(defun $exp (y)
  ($exp (num y)))
(defun $expt (x y)
  (expt (num x) (num y)))
(defun $atan2 (x y)
  (atan (num x) (num y))) 
(defun $cos (x)
  (cos (num x)))
(defun $sin (x)
  (sin (num x)))
(defun $int (x)
  (truncate (num x)))
(defun $log (x)
  (log (num x)))
(defun $sqrt (x)
  (sqrt (num x)))
(defun $rand ()
  (random 1.0))

(defvar *random-states* (make-hash-table))

(defun $srand (x)
  (let ((nx (num x)))
    (let ((oldstate (gethash nx *random-states*)))
      (if oldstate
          (setq *random-state* oldstate)
        (setf *random-state* (make-random-state)
              (gethash nx *random-states*) *random-state*)))))

(defun $++ (&rest rest)
  (reduce #'(lambda (x y) (concatenate 'string x (str y)))
          (mapcar #'str rest)))

(defgeneric ! (x))
(defmethod ! ((x number))
  (zerop x))
(defmethod ! ((x (eql nil)))
  1)
(defmethod ! ((x string))
  (zerop (num x)))

(defgeneric $== (x y))
(defmethod $== (x y)
  (= (num x) (num y)))
(defmethod $== ((x number) (y number))
  (= x y))
(defmethod $== ((x number) (y string))
  (= x (num y)))
(defmethod $== ((x string) (y number))
  (= (num x) y))
(defmethod $== ((x string) (y string))
  (string= x y))

(defgeneric $< (x y))
(defmethod $< (x y)
  (< (num x) (num y)))
(defmethod $< ((x number) (y number))
  (< x y))
(defmethod $< ((x number) (y string))
  (< x (num y)))
(defmethod $< ((x string) (y number))
  (< (num x) y))
(defmethod $< ((x string) (y string))
  (string< x y))

(defgeneric $> (x y))
(defmethod $> (x y)
  (> (num x) (num y)))
(defmethod $> ((x number) (y number))
  (> x y))
(defmethod $> ((x number) (y string))
  (> x (num y)))
(defmethod $> ((x string) (y number))
  (> (num x) y))
(defmethod $> ((x string) (y string))
  (string> x y))

(defgeneric $<= (x y))
(defmethod $<= (x y)
  (<= (num x) (num y)))
(defmethod $<= ((x number) (y number))
  (<= x y))
(defmethod $<= ((x number) (y string))
  (<= x (num y)))
(defmethod $<= ((x string) (y number))
  (<= (num x) y))
(defmethod $<= ((x string) (y string))
  (string<= x y))

(defgeneric $>= (x y))
(defmethod $>= (x y)
  (>= (num x) (num y)))
(defmethod $>= ((x number) (y number))
  (>= x y))
(defmethod $>= ((x number) (y string))
  (>= x (num y)))
(defmethod $>= ((x string) (y number))
  (>= (num x) y))
(defmethod $>= ((x string) (y string))
  (string>= x y))

(defgeneric $/= (x y))
(defmethod $/= (x y)
  (/= (num x) (num y)))
(defmethod $/= ((x number) (y number))
  (/= x y))
(defmethod $/= ((x number) (y string))
  (/= x (num y)))
(defmethod $/= ((x string) (y number))
  (/= (num x) y))
(defmethod $/= ((x string) (y string))
  (string/= x y))
(defun != (x y)
  ($/= x y))


(defgeneric $max (x &rest rest))
(defmethod $max (x &rest rest)
  (reduce #'max (mapcar #'num rest) :initial-value (num x)))
(defmethod $max ((x number) &rest rest)
  (reduce #'max (mapcar #'num rest) :initial-value x))
(defmethod $max ((x string) &rest rest)
  (reduce #'max (mapcar #'str rest) :initial-value x))
                 
(defgeneric $min (x &rest rest))
(defmethod $min (x &rest rest)
  (reduce #'min (mapcar #'num rest) :initial-value (num x)))
(defmethod $min ((x number) &rest rest)
  (reduce #'min (mapcar #'num rest) :initial-value x))
(defmethod $min ((x string) &rest rest)
  (reduce #'min (mapcar #'str rest) :initial-value x))

(defgeneric $zerop (x))
(defmethod $zerop (x)
  (declare (ignore x))
  nil)
(defmethod $zerop ((x number))
  (zerop x))
(defmethod $zerop ((x (eql nil)))
  t)
(defmethod $zerop ((x string))
  (not (null (or (string= x "") (string= x "0") (string= x "0.0")))))

(defgeneric $length (x))
(defmethod $length (x)
  (length x))
(defmethod $length ((x number))
  (length (str x)))
(defmethod $length ((x (eql nil)))
  0)

(defun $print (&rest rest)
  (do-$fprint *standard-output* rest))
(defun $fprint (stream &rest rest)
  (do-$fprint stream rest))
(defun do-$fprint (stream lst)
  (if (string= *ORS* "\n")
      (format stream "~%")
    (format stream "~A" *ORS*))
  (loop for item in lst
        do (format stream "~A~A" (str item) *OFS*)))




;;;
;;; Awk-like associative arrays (built on hashtable, obviously)
;;;
(defun $array ()
  (make-hash-table :test 'equalp))


;; AWK associative-arrays have the odd characteristic that simply
;; checking for the presence of a key adds it to the array.
(defun assoc-array-ref (tbl index)
  (multiple-value-bind (val foundp)
      (gethash index tbl)
    (if foundp
	val
	(setf (gethash index tbl) ""))))

(defsetf assoc-array-ref (tbl index) (val)
  `(setf (gethash ,index ,tbl) ,val))

(defmacro $aref (tbl index &rest other-indices)
  (if (null other-indices)
      `(assoc-array-ref ,tbl (str ,index))
    `(assoc-array-ref ,tbl
		      (concatenate
			'string
			(str ,index)
			,@(mapcan #'(lambda (x)
				      `(*SUBSEP* (str ,x)))
				  other-indices)))))

; equivalent to:  for (x in a) ...
(defmacro $for ((keyvar in tbl) &rest body)
  (declare (ignore in))
  #+:Symbolics (declare (zwei:indentation 1 1))
  `(loop for ,keyvar being the hash-keys of ,tbl
         do (progn ,@body)))
#+:Lispworks (editor:setup-indent "$for" 1 2 6)

; equivalent to: x in a
(defun $in (key tbl)
  (multiple-value-bind (val presentp)
      (getf key tbl)
    (declare (ignore val))
    presentp))

; equivalent to either delete a[i] or delete a
(defun $delete (tbl &optional (elt nil eltp))
  (if eltp
      (remhash elt tbl)
    (clrhash tbl)))

; return the cardinality of the table
(defmethod $length ((x hash-table))
  (hash-table-count x))
