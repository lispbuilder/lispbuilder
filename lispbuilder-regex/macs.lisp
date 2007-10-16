;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LISPBUILDER-REGEX; Base: 10 -*-

(in-package :LISPBUILDER-REGEX)


; disable inlining and the optimizations in the match functions.
;(eval-when (eval compile load)
;  (pushnew :debug-regex *features*) )



;;;;
;;;; Regex Configuration
;;;;


(defvar *regex-compile-verbose* nil)
(defun compile-verbose (&optional (flag t))
  (setq *regex-compile-verbose* flag))

(defvar *match-simple-strings-only* t)
(defun match-simple-strings-only (&optional (flag t))
  (setq *match-simple-strings-only* flag)
  (clear-pattern-cache))

;; this should always be set to t
(defvar *force-safe-match* t)
(defun force-safe-match (&optional (flag t))
  (setq *force-safe-match* flag)
  (clear-pattern-cache))

(defvar *escape-special-chars* nil)
(defun escape-special-chars (&optional (flag t))
  (setq *escape-special-chars* flag)
  (clear-pattern-cache))

(defvar *dot-matches-newline* nil)
(defun dot-matches-newline (&optional (flag t))
  (setq *dot-matches-newline* flag))

(defvar *allow-backmatch* t)
(defun allow-backmatch (&optional (flag t))
  (setq *allow-backmatch* flag)
  (clear-pattern-cache))

(defvar *allow-rangematch* t)
(defun allow-rangematch (&optional (flag t))
  (setq *allow-rangematch* flag)
  (clear-pattern-cache))

(defvar *allow-nongreedy-quantifiers* t)
(defun allow-nongreedy-quantifiers (&optional (flag t))
  (setq *allow-nongreedy-quantifiers* flag)
  (clear-pattern-cache))

(defvar *allow-nonregister-groups* t)
(defun allow-nonregister-groups (&optional (flag t))
  (setq *allow-nonregister-groups* flag)
  (clear-pattern-cache))

(defvar *registers-match-rightmost* nil)
(defun registers-match-rightmost (&optional (flag t))
  (setq *registers-match-rightmost* flag)
  (clear-pattern-cache))





;;;;
;;;; Regex parse tree.
;;;;

#-:debug-regex(declaim (inline make-char-node char-node-p char-node-char))
(defun make-char-node (chr)
  chr)
(defun char-node-p (node)
  (characterp node))
(defun char-node-char (char-node)
  char-node)


#-:debug-regex(declaim (inline make-string-node string-node-p string-node-string))
(defun make-string-node (str)
  str)
(defun string-node-p (node)
  (stringp node))
(defun text-node-p (node)
  (or (char-node-p node) (string-node-p node)))
(defun string-node-string (string-node)
  string-node)

#-:debug-regex(declaim (inline make-startword-node startword-node-p))
(defun make-startword-node ()
  '(startword))
(defun startword-node-p (node)
  (equalp node '(startword)))

#-:debug-regex(declaim (inline make-endword-node endword-node-p))
(defun make-endword-node ()
  '(endword))
(defun endword-node-p (node)
  (equalp node '(endword)))



#-:debug-regex(declaim (inline make-classseq-node classseq-node-p
                               classseq-node-seq))
(defun make-classseq-node (seq)
  `(classseq ,seq))
(defun classseq-node-p (node)
  (and (listp node) (eq (first node) 'classseq)))
(defun classseq-node-seq (classseq-node)
  (second classseq-node))



#-:debug-regex(declaim (inline make-backmatch-node backmatch-node-p
                               backmatch-node-regnum))
(defun make-backmatch-node (regnum)
  `(backmatch ,regnum))
(defun backmatch-node-p (node)
  (and (listp node) (eq (first node) 'backmatch)))
(defun backmatch-node-regnum (backmatch-node)
  (second backmatch-node))


#-:debug-regex(declaim (inline make-seq-node-list make-seq-node-args
                               seq-node-p
                               seq-node-children seq-node-numchildren
                               seq-node-child))
(defun make-seq-node-list (child-nodes)
  (let ((numchildren (length child-nodes)))
    (cond ((zerop numchildren) nil)
          ((= numchildren 1) (first child-nodes))
          (t `(seq ,@child-nodes)))))
(defun make-seq-node-args (&rest child-nodes)
  (make-seq-node-list child-nodes))
(defun seq-node-p (node)
  (and (listp node) (eq (first node) 'seq)))
(defun seq-node-children (seq-node)
  (rest seq-node))
(defun seq-node-numchildren (seq-node)
  (length (seq-node-children seq-node)))
(defun seq-node-child (seq-node idx)
  (elt (seq-node-children seq-node) idx))


#-:debug-regex(declaim (inline make-kleene-node
                               kleene-node-greedy-p kleene-node-nongreedy-p
                               kleene-node-child))
(defun make-kleene-node (child-node greedyp)
  (cond (greedyp
         `(* ,child-node))
        (t `(*? ,child-node))))
(defun kleene-node-p (node)
  (cond
   ((not (listp node)) nil)
   ((eq (first node) '*) t)
   ((eq (first node) '*?) t)
   (t nil)))
(defun kleene-node-greedy-p (node)
  (eq (first node) '*))
(defun kleene-node-nongreedy-p (node)
  (eq (first node) '*?))
(defun kleene-node-child (kleene-node)
  (second kleene-node))


#-:debug-regex(declaim (inline make-pkleene-node
                               pkleene-node-greedy-p
                               pkleene-node-nongreedy-p
                               pkleene-node-child))
(defun make-pkleene-node (child-node greedyp)
  (cond (greedyp
         `(+ ,child-node))
        (t `(+? ,child-node))))
(defun pkleene-node-p (node)
  (cond
   ((not (listp node)) nil)
   ((eq (first node) '+) t)
   ((eq (first node) '+?) t)
   (t nil)))
(defun pkleene-node-greedy-p (node)
  (eq (first node) '+))
(defun pkleene-node-nongreedy-p (node)
  (eq (first node) '+?))
(defun pkleene-node-child (pkleene-node)
  (second pkleene-node))


#-:debug-regex(declaim (inline make-optional-node
                               optional-node-p
                               optional-node-greedy-p
                               optional-node-nongreedy-p
                               optional-child-node))
(defun make-optional-node (child-node greedyp)
  (cond (greedyp
         `(? ,child-node))
        (t `(?? ,child-node))))
(defun optional-node-p (node)
  (cond
   ((not (listp node)) nil)
   ((eq (first node) '?) t)
   ((eq (first node) '??) t)
   (t nil)))
(defun optional-node-greedy-p (node)
  (eq (first node) '?))
(defun optional-node-nongreedy-p (node)
  (eq (first node) '??))
(defun optional-node-child (optional-node)
  (second optional-node))


#-:debug-regex(declaim (inline make-range-node
                               range-node-greedy-p
                               range-node-nongreedy-p
                               range-node-min
                               range-node-max
                               range-node-child))
(defun make-range-node (child-node min max greedyp)
  (unless (numberp min) (setq min 0))
  (unless (numberp max) (setq max nil))
  (let ((min (if (and (numberp min) (numberp max))
                 (min min max)
               min))
        (max (if (and (numberp min) (numberp max))
                 (max min max)
               max)))
    (cond (greedyp
           `(range (,min . ,max) ,child-node))
          (t `(ngrange (,min . ,max) ,child-node)))))
(defun range-node-p (node)
  (cond
   ((not (listp node)) nil)
   ((eq (first node) 'range) t)
   ((eq (first node) 'ngrange) t)
   (t nil)))
(defun range-node-greedy-p (node)
  (eq (first node) 'range))
(defun range-node-nongreedy-p (node)
  (eq (first node) 'ngrange))
(defun range-node-min (range-node)
  (car (second range-node)))
(defun range-node-max (range-node)
  (cdr (second range-node)))
(defun range-node-child (range-node)
  (third range-node))


#-:debug-regex(declaim (inline make-alt-node-list
                               make-alt-node-args
                               alt-node-p
                               alt-node-children
                               alt-node-numchildren
                               alt-node-first alt-node-second
                               alt-node-child))
(defun make-alt-node-list (child-nodes)
  `(alt ,@child-nodes))
(defun make-alt-node-args (&rest child-nodes)
  `(alt ,@child-nodes))
(defun alt-node-p (node)
  (and (listp node) (eq (first node) 'alt)))
(defun alt-node-children (alt-node)
  (rest alt-node))
(defun alt-node-numchildren (alt-node)
  (length (alt-node-children alt-node)))
(defun alt-node-first (alt-node)
  (first (alt-node-children alt-node)))
(defun alt-node-second (alt-node)
  (second (alt-node-children alt-node)))
(defun alt-node-child (alt-node idx)
  (elt (alt-node-children alt-node) idx))


#-:debug-regex(declaim (inline make-casealt-node-list
                               make-casealt-node-args
                               casealt-node-p
                               casealt-node-children
                               casealt-node-numchildren))
(defun make-casealt-node-list (child-nodes)
  `(casealt ,child-nodes))
(defun make-casealt-node-args (&rest child-nodes)
  `(casealt ,child-nodes))
(defun casealt-node-p (node)
  (and (listp node) (eq (first node) 'casealt)))
(defun casealt-node-children (casealt-node)
  (second casealt-node))
(defun casealt-node-numchildren (casealt-node)
  (length (casealt-node-children casealt-node)))



#-:debug-regex(declaim (inline make-register-node register-node-p
                               register-node-regnum register-node-child))
(defun make-register-node (regnum child)
  `(reg ,regnum ,child))
(defun register-node-p (node)
  (and (listp node) (eq (first node) 'reg)))
(defun register-node-regnum (reg-node)
  (second reg-node))
(defun register-node-child (reg-node)
  (third reg-node))


#-:debug-regex(declaim (inline make-regstart-node
                               regstart-node-p
                               regstart-node-regnum))
(defun make-regstart-node (regnum)
  `(regstart ,regnum))
(defun regstart-node-p (node)
  (and (listp node) (eq (first node) 'regstart)))
(defun regstart-node-regnum (rstart-node)
  (second rstart-node))


#-:debug-regex(declaim (inline make-regend-node
                               regend-node-p
                               regend-node-regnum))
(defun make-regend-node (regnum)
  `(regend ,regnum))
(defun regend-node-p (node)
  (and (listp node) (eq (first node) 'regend)))
(defun regend-node-regnum (rend-node)
  (second rend-node))




#-:debug-regex(declaim (inline make-lookahead-node
                               lookahead-node-p
                               lookahead-node-expr))
(defun make-lookahead-node (expr)
  `(lookahead ,expr))
(defun lookahead-node-p (node)
  (and (listp node) (eq (first node) 'lookahead)))
(defun lookahead-node-expr (lookahead-node)
  (second lookahead-node))


#-:debug-regex(declaim (inline make-nlookahead-node
                               nlookahead-node-p
                               nlookahead-node-expr))
(defun make-nlookahead-node (expr)
  `(nlookahead ,expr))
(defun nlookahead-node-p (node)
  (and (listp node) (eq (first node) 'nlookahead)))
(defun nlookahead-node-expr (nlookahead-node)
  (second nlookahead-node))





#-:debug-regex(declaim (inline make-charclass-node
                               charclass-node-negated-p
                               charclass-node-chars))
(defun make-charclass-node (chars &key negated)
  (cond (negated
         `(ncharclass ,(expand-char-class (coerce chars 'list))))
        (t `(charclass ,(expand-char-class (coerce chars 'list))))))
(defun positive-charclass-node-p (node)
  (and (listp node) (eq (first node) 'charclass)))
(defun charclass-node-p (node)
  (cond
   ((not (listp node)) nil)
   ((eq (first node) 'charclass) t)
   ((eq (first node) 'ncharclass) t)
   (t nil)))
(defun charclass-node-negated-p (charclass-node)
  (eq (first charclass-node) 'ncharclass))
(defun charclass-node-chars (charclass-node)
  (second charclass-node))
(defun char-or-class-node-p (node)
  (or (char-node-p node) (charclass-node-p node)))


#-:debug-regex(declaim (inline make-specclass-node
                               specclass-node-negated-p
                               specclass-node-class))
(defun make-specclass-node (class &key negated)
  (cond (negated
         `(nspecclass ,class))
        (t `(specclass ,class))))
(defun specclass-node-p (node)
  (cond
   ((not (listp node)) nil)
   ((eq (first node) 'specclass) t)
   ((eq (first node) 'nspecclass) t)
   (t nil)))
(defun specclass-node-negated-p (specclass-node)
  (eq (first specclass-node) 'nspecclass))
(defun specclass-node-class (specclass-node)
  (second specclass-node))


#-:debug-regex(declaim (inline make-start-anchor-node start-anchor-node-p))
(defun make-start-anchor-node ()
  '(start))
(defun start-anchor-node-p (node)
  (equalp node '(start)))


#-:debug-regex(declaim (inline make-end-anchor-node end-anchor-node-p))
(defun make-end-anchor-node ()
  '(end))
(defun end-anchor-node-p (node)
  (equalp node '(end)))


#-:debug-regex(declaim (inline make-any-node any-node-p))
(defun make-any-node ()
  '(any))
(defun any-node-p (node)
  (equalp node '(any)))


#-:debug-regex(declaim (inline make-hook-node hook-node-p
                               hook-node-function hook-node-symbol-p
                               hook-node-function-p hook-node-index-p))
(defun make-hook-node (fxn-id)
  `(hook ,fxn-id))
(defun hook-node-p (node)
  (and (listp node) (eq (first node) 'hook)))
(defun hook-node-function (hook-node)
  (second hook-node))
(defun hook-node-symbol-p (hook-node)
  (symbolp (hook-node-function hook-node)))
(defun hook-node-function-p (hook-node)
  (functionp (hook-node-function hook-node)))
(defun hook-node-index-p (hook-node)
  (integerp (hook-node-function hook-node)))



#-:debug-regex(declaim (inline make-success-node success-node-p
                               success-node-rc))
(defun make-success-node (rc)
  `(succeed ,rc))
(defun success-node-p (node)
  (and (listp node) (eq (first node) 'succeed)))
(defun success-node-rc (success-node)
  (second success-node))


;;;;
;;;; Misc handy macros
;;;;

(defun make-regs (n)
  (let ((regs (make-array n)))
    (dotimes (i n regs)
      (setf (svref regs i) (cons nil nil)))))

(declaim (inline register-start))
(defun register-start (regs n)
  (car (svref regs n)))
(define-compiler-macro register-start (regs n)
  `(car (svref ,regs ,n)))

(declaim (inline register-end))
(defun register-end (regs n)
  (cdr (svref regs n)))
(define-compiler-macro register-end (regs n)
  `(cdr (svref ,regs ,n)))

(defsetf register-start (regs n) (val)
  `(setf (car (svref ,regs ,n)) ,val))
(defsetf register-end (regs n) (val)
  `(setf (cdr (svref ,regs ,n)) ,val))

(defun register-matched-p (regs n)
  (and (numberp (register-start regs n))
       (numberp (register-end regs n))
       (<= (register-start regs n) (register-end regs n))))






(defstruct re-scanner
  (str "" :type string)
  (pos 0 :type fixnum)
  (end 0 :type fixnum)
  (mode 'in-regex :type symbol)
  (regnum 0 :type fixnum)
  (ungot-token nil)
  (ungot-value nil)
  )



(defstruct closure-info
  matchfn initfn linkfn)

(defun resolve-instr (closure-vec instr-num)
  (closure-info-matchfn (aref closure-vec instr-num)))

(defun resolve-linkfn (closure-vec instr-num)
  (closure-info-linkfn (aref closure-vec instr-num)))

(defun resolve-initfn (closure-vec instr-num)
  (closure-info-initfn (aref closure-vec instr-num)))


(defstruct matcher
  simple-string-matchfn string-matchfn numregs matchstr matchexpr acceptfn)

(defconstant +max-regex-str-cache+ 500
  "Max number of entries in the regex compiler cache.")

(defvar *pattern-cache* (make-hash-table :test #'equal))

(defun clear-pattern-cache ()
  (clrhash *pattern-cache*)
  nil)


(defparameter +special-class-names+
  '((":alpha:" alpha) (":upper:" upper) (":lower:" lower) (":digit:" digit)
    (":alnum:" alnum) (":xdigit:" xdigit) (":odigit:" odigit) (":punct:" punct)
    (":space:" space) (":word:" wordchar)))




(defun expand-char-class (chars)
"Expand an encoded char class into an explicit enumeration of all
the chars, e.g. 'a-f\A-F' --> 'abcdefABCDEF'."
  (parse-std-char-class chars))

(defun parse-std-char-class (in)
  (let ((out (make-string-output-stream)))
    (do ((chr (pop in) (pop in))
         (prv nil chr))
        ((null chr) (progn
                      (when prv (write-char prv out))
                      (get-output-stream-string out)))
      (case chr
        (dash
         (let ((nxt (pop in)))
           (cond
            ((and (null nxt) (null prv))
             (write-char #\- out))
            ((null nxt)
             (write-char prv out)
             (write-char #\- out))
            ((null prv)
             (write-char #\- out)
             (write-char nxt out))
            (t (generate-char-range out prv nxt)
               (setq prv nil)))
           (setq chr (pop in))))
        (t (when prv
             (write-char prv out)))))))

(defun generate-char-range (strm start end)
  (when (< (char-int end) (char-int start))
    (rotatef start end))
  (loop for ic from (char-int start) to (char-int end)
        do (write-char (code-char ic) strm)))

