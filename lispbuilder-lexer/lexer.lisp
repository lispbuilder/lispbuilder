;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LISPBUILDER-LEXER; Base: 10 -*-

(in-package :LISPBUILDER-LEXER)


;;;
;;; tokenizer/lexical analyser generator
;;;

;;;
;;; TOKENIZE
;;;
;;; Similar to MATCH-CASE, but more explicitly tailored to parsing
;;; duties.
;;;

;
; Should probably take advantage of the new regex compiler like deflexer does
;

(defmacro tokenize (str pos-var &rest rules)
  (expand-tokenize str pos-var rules))
#+:Lispworks (editor:setup-indent "tokenize" 2 2 10)

(defun expand-tokenize (str pos rules &aux (strvar (gensym)) (matchedp (gensym)))
  `(let ((,strvar ,str)
         ,matchedp *RSTART* *RLENGTH* *REGS* *LAST-SUCCESSFUL-MATCH*)
     ,(expand-tokenize-rules strvar pos matchedp rules)))


(defun expand-tokenize-rules (str pos matchedp rules)
  (unless (null rules)
    (destructuring-bind ((rule-pat . rule-actions) . rest-rules) rules
      `(progn
         (multiple-value-setq (,matchedp *RSTART* *RLENGTH* *REGS*)
             (lispbuilder-regex:match-str (get-matcher-for-pattern ,rule-pat)
                              ,str :start ,pos))
         (if ,matchedp
             (progn
               (setq *LAST-SUCCESSFUL-MATCH* ,str)
               (incf ,pos *RLENGTH*)
               ,@rule-actions)
           ,(expand-tokenize-rules str pos matchedp rest-rules))))))


; (defun tokenize-test (str &optional (pos 0))
;   (let ((end (length str)))
;     (loop while (< pos end)
;           do (tokenize str pos
;                (#/[0-9]+([.][0-9]+([Ee][0-9]+)?)/ (print `(flt ,%0)))
;                (#/[0-9]+/ (print `(int ,%0)))
;                (#/./ (print %0))))))
;
; > (tokenize-test "1.0 2.0E5 10 30")
; (flt "1.0")
; " "
; (flt "2.0E5")
; " "
; (int "10")
; " "
; (int "30")
; NIL
;
; >


;;;
;;; Define a lexical analyser generator function.  This function takes
;;; a string and optionally the start and end positions, and returns
;;; a function of no parameters that returns the two values token and
;;; value each time it is called.  When the string is exhausted, it
;;; returns (values nil nil)
;;;
;;; These lexers are compatible with the lispworks parser generator.
;;;
;;; If you need to parse a stream, then just load the stream into a
;;; string and hand it to the parser.  Or do it a line-at-a-time if
;;; you need a more incremental parser.
;;;
;;; This version uses the new regex compiler to good effect.  It
;;; parses out each pattern, then combines them all into one big
;;; pattern that is rigged to
;;;


(defun gen-flexy-lexer (name options patterns actions)
  (let ((matchervar (intern (concatenate 'string (symbol-name name) "-MATCHER")
                            *package*))
        (rcvar (gensym))
        (matchstartvar (gensym))
        (matchlenvar (gensym))
        (matchregsvar (gensym))
        (strvar (gensym))
        (prevposvar (gensym)))
      `(progn
         (defparameter ,matchervar (macroexpand-regex-expr ',(combine-flexy-patterns patterns)))
         (defun ,name (,strvar &key (start 0)
                                    (end (length ,strvar))
                                    (end-token nil)
                                    (end-value nil)
                               &aux (,prevposvar -1)
                                    ,rcvar ,matchstartvar ,matchlenvar ,matchregsvar)
           (declare (string ,strvar) (fixnum start end))
           (flet ((%n (n)
                    (let* ((rstart (lispbuilder-regex:register-start ,matchregsvar n))
                           (rend (lispbuilder-regex:register-end ,matchregsvar n)))
                      (if (and (numberp rstart) (numberp rend))
                          (subseq ,strvar rstart rend))))
                  (nextch ()
                    (when (< start end)
                      (prog1
                          (char ,strvar start)
                        (incf start)
                        )))
                  (ungetch (ch)
                    (when (and (characterp ch) (> start 0))
                      (decf start)
                      )))
             (symbol-macrolet ((%0 (%n 0)) (%1 (%n 1)) (%2 (%n 2)) (%3 (%n 3))
                               (%4 (%n 4)) (%5 (%n 5)) (%6 (%n 6)) (%7 (%n 7))
                               (%8 (%n 8)) (%9 (%n 9)) (%10 (%n 10)))
               (lambda (&aux (*flexy-match-rulenum* -1)
                             (*flexy-match-end* -1)
                             *flexy-match-start* *flexy-match-regs*)
                 (declare (special *flexy-match-rulenum*)
                          (special *flexy-match-end*)
                          (special *flexy-match-start*)
                          (special *flexy-match-regs*))
                 (loop :while (< start end)
                       :when (= start ,prevposvar)
                         :do (error "Lexer unable to recognize a token in ~S, position ~D (~S)"
                                    ,strvar start
                                    (subseq ,strvar start (min end (+ start 20))))
                       :do (progn
                            (setq ,prevposvar start)
                            (multiple-value-setq (,rcvar ,matchstartvar ,matchlenvar ,matchregsvar)
                                (match-str ,matchervar ,strvar :start start))
                            (if (and ,rcvar (>= *flexy-match-rulenum* 0) (>= *flexy-match-end* 0))
                                (progn
                                  (setf ,matchlenvar (- *flexy-match-end* ,prevposvar)
                                        ,matchregsvar *flexy-match-regs*
                                        ,rcvar *flexy-match-rulenum*)
                                  (incf start ,matchlenvar))
                              (error "~A lexing failure (no token recognized) in ~S @ ~D"
                                     ',name ,strvar start))
                            (case ,rcvar
                              ,@(make-lexer-actions actions)
                              (otherwise
                               (error "~A lexing failure (unknown token) in ~S @ ~D, ~S ~S ~S ~S"
                                      ',name ,strvar start
                                      ,rcvar ,matchstartvar ,matchlenvar ,matchregsvar))))
                       :finally (return (values end-token end-value))))))))))


(defun gen-fast-lexer (name options patterns actions)
  (let ((matchervar (intern (concatenate 'string (symbol-name name) "-MATCHER")
                            *package*))
        (rcvar (gensym))
        (matchstartvar (gensym))
        (matchlenvar (gensym))
        (matchregsvar (gensym))
        (strvar (gensym))
        (prevposvar (gensym)))
      `(progn
         (defparameter ,matchervar (macroexpand-regex-expr ',(combine-fast-patterns patterns)))
         (defun ,name (,strvar &key (start 0)
                                    (end (length ,strvar))
                                    (end-token nil)
                                    (end-value nil)
                               &aux (,prevposvar -1)
                                    ,rcvar ,matchstartvar ,matchlenvar ,matchregsvar)
           (declare (string ,strvar) (fixnum start end))
           (flet ((%n (n)
                    (let* ((rstart (lispbuilder-regex:register-start ,matchregsvar n))
                           (rend (lispbuilder-regex:register-end ,matchregsvar n)))
                      (if (and (numberp rstart) (numberp rend))
                          (subseq ,strvar rstart rend))))
                  (nextch ()
                    (when (< start end)
                      (prog1
                          (char ,strvar start)
                        (incf start)
                        )))
                  (ungetch (ch)
                    (when (and (characterp ch) (> start 0))
                      (decf start)
                      )))
             (symbol-macrolet ((%0 (%n 0)) (%1 (%n 1)) (%2 (%n 2)) (%3 (%n 3))
                               (%4 (%n 4)) (%5 (%n 5)) (%6 (%n 6)) (%7 (%n 7))
                               (%8 (%n 8)) (%9 (%n 9)) (%10 (%n 10)))
               (lambda ()
                 (loop :while (< start end)
                       :when (= start ,prevposvar)
                         :do (error "Lexer unable to recognize a token in ~S, position ~D (~S)"
                                    ,strvar start
                                    (subseq ,strvar start (min end (+ start 20))))
                       :do (progn
                            (setq ,prevposvar start)
                            (multiple-value-setq (,rcvar ,matchstartvar ,matchlenvar ,matchregsvar)
                                (match-str ,matchervar ,strvar :start start))
                            (if ,rcvar
                                (incf start ,matchlenvar)
                              (error "~A lexing failure (no token recognized) in ~S @ ~D"
                                     ',name ,strvar start))
                            (case ,rcvar
                              ,@(make-lexer-actions actions)
                              (otherwise
                               (error "~A lexing failure (unknown token) in ~S @ ~D, ~S ~S ~S ~S"
                                      ',name ,strvar start
                                      ,rcvar ,matchstartvar ,matchlenvar ,matchregsvar))))
                       :finally (return (values end-token end-value))))))))))


(defmacro deflexer (name &rest rules)
  "Create a lexical analyser.  This analyser function takes a string :position,
:end, :end-token and :end-value keyword parameters, and returns a function of
no arguments that returns the next token and value each time it is called,
or (values end-token end-value) when the input string is exhausted.
By default, position = 0, end = length of str, and end-token and end-value = nil."
  (multiple-value-bind (options patterns actions)
      (extract-patterns-and-actions rules)
    (if (member :flex-compatible options)
        (gen-flexy-lexer name options patterns actions)
      (gen-fast-lexer name options patterns actions))))
#+:Lispworks (editor:setup-indent "deflexer" 1 2 10)

; Pull out the options, patterns and actions, with rule numbers so we can keep them associated
(defun extract-patterns-and-actions (rules &aux options patterns actions)
  (loop for rule in rules
        for rulenum from 0
        do (cond ((atom rule)
                  (push rule options))
                 (t (push (list (first rule) rulenum) patterns)
                    (push (list (rest rule) rulenum) actions)))
        finally (return (values (nreverse options) (nreverse patterns) (nreverse actions)))))

; Combines patterns into one big ALT, with clause extended with a new success node
; that returns the pattern number that matched.
(defun combine-fast-patterns (pats)
  (flet ((make-specialized-parsetree (patstr hooknum)
           (let ((parsetree (lispbuilder-regex:parse-str patstr)))
             (when (null parsetree)
               (error "Regex compile error in ~S" patstr))
             (lispbuilder-regex:make-seq-node-args
              parsetree
              (lispbuilder-regex:make-success-node hooknum)))))
    (make-alt-node-list
     (mapcar #'make-specialized-parsetree
             (mapcar #'first pats)
             (mapcar #'second pats)))))

; Combines patterns into one big ALT, with clause extended with a new hook node
; that records the pattern number that matched
(defun copy-matchregs (regs)
  (loop with l = (length regs)
        with newregs = (make-array l)
        for i from 0 below l
        for reg = (aref regs i) then (aref regs i)
        do (setf (aref newregs i) (cons (car reg) (cdr reg)))
        finally (return newregs)))
        

(defun make-flexy-hookfn (rulenum)
  #'(lambda (pos)
      (declare (fixnum pos)
               (special *flexy-match-end*)
               (special *flexy-match-regs*)
               (special *flexy-match-rulenum*)
               (special lispbuilder-regex::*regs*))
      (when (> pos *flexy-match-end*)
        (setf *flexy-match-end* pos
              *flexy-match-regs* (copy-matchregs lispbuilder-regex::*regs*)
              *flexy-match-rulenum* rulenum))
      nil))


(defun combine-flexy-patterns (pats)
  (flet ((make-specialized-parsetree (patstr hooknum)
           (let ((parsetree (lispbuilder-regex:parse-str patstr)))
             (when (null parsetree)
               (error "Regex compile error in ~S" patstr))
             (lispbuilder-regex:make-seq-node-args
              parsetree
              (lispbuilder-regex:make-hook-node (make-flexy-hookfn hooknum))))))
    (make-alt-node-list
     (append
      (mapcar #'make-specialized-parsetree
              (mapcar #'first pats)
              (mapcar #'second pats))
      (list (lispbuilder-regex:make-success-node t))))))


; Build the body of the case statement that performs the action corresponding to
; the matched pattern.
(defun make-lexer-actions (actions)
  (mapcar #'(lambda (action)
              `(,(second action) (progn ,@(first action))))
          actions))


;(deflexer test-lexer
;  ("[0-9]+([.][0-9]+([Ee][0-9]+)?)"
;    (return (values 'flt (num %0))))
;  ("[0-9]+"
;    (return (values 'int (int %0))))
;  ("[0-9]+X"
;    (return (values 'int (int %0))))
;  ("[0-9]+L"
;    (return (values 'int (int %0))))
;  ("while" (return 'while))
;  ("wend" (return 'wend))
;  ("with" (return 'with))
;  ("wabbit" (return 'wabbit))
;  ("for" (return 'for))
;  ("in" (return 'in))
;  ("loop" (return 'loop))
;  ("var" (return 'var))
;  ("[:alpha:][:alnum:]*"
;    (return (values 'name %0)))
;  ("[:space:]+")
; )
;
; > (setq *lex* (test-lexer "1.0 12 fred with wabbit 10.23e12"))
; <closure ...>
;
; > (funcall *lex*)
; FLT
; 1.0
;
; > (funcall *lex*)
; INT
; 12
;
; > (funcall *lex*)
; NAME
; "fred"
;
; > (funcall *lex*)
; WITH
;
; > (funcall *lex*)
; WABBIT
;
; > (funcall *lex*)
; FLT
; 1.0229999999999997E46
;
; > (funcall *lex*)
; NIL
; NIL
;
; >

#|
; flex-compatibility test
(deflexer c-lexer
  :flex-compatible
  ("int" (return (values 'INT %0)))
  ("([:alpha:]|_)([:alnum:]|_)*" (return (values 'IDENTIFIER %0)))
  ("[:space:]"))

(setq *lex* (c-lexer "int intelligence integer"))
(funcall *lex*)


(defun test ()
  (loop with lexer = (c-lexer "int intelligence integer") 
        with tokens = '()
        finally (return (nreverse tokens)) do
        (multiple-value-bind (token value) (funcall lexer)
          (if token
              (push (cons token value) tokens)
            (loop-finish)))))



(deflexer test-lexer
  :flex-compatible
  ("[0-9]+"
    (return (values 'int (int %0))))
  ("[0-9]+([.][0-9]+([Ee][0-9]+)?)"
    (return (values 'flt (num %0))))
  ("[:space:]+")
 )

(setq *lex* (test-lexer "1.0 12 10.23e12"))

|#



;;;
;;; Cribbed from clawk
;;;

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

