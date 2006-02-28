;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LISPBUILDER-REGEX; Base: 10 -*-

(in-package :LISPBUILDER-REGEX)


;
; Rewrite to use parse-tree functions.
; Separate optimization from parsing.
; Add separate canonicalization and rewrite passes.
;
; Partly because I'm about to allow the caller to manipulate
; the parse trees himself, so I can't depend on the parse
; tree being in any particular format.  Partly because the
; lexer needs some beefier optimizations that I can easily
; provide in the current ad-hoc scheme.  And partly because
; compile-greedy-star/compile-greedy-plus are a mess and
; about to get messier.
;
; Planned organization:
; pass 1. Lexical analysis
; pass 2. Parsing
; pass 3. Canonicalize parse tree (needed because we allow
;         program interaction at the parse tree level). Also
;         convert (reg n <node>) to (seq (rstart n) <node> (rend n)).
;         Either are valid parse-tree inputs, but the (reg ...)
;         syntax is nearly always more convenient, but obscures 
;         the cps form, complicating some optimizations.
; pass 4. Instruction selection (fast-xxx ops)
; pass 5. CPS conversion to tuple list.

; For incremental use:
; pass 6. Code (closure) generation.
; pass 7. Linking (resolve target labels to functions).
;
; For batch use (i.e. deflexer, defregex et al)
; pass 6: Sexpr generation
;
; Passes 1&2 are coroutines.
; Pass 3 should be iterated until it reaches a fixedpoint.
; Passes 4-7 are currently all rolled into one pass, which is
; a PITA to maintain, and doesn't facilitate the generation of both
; closures and code.
;
;
; Planned optimizations:
;
;   Merge ALT heads and tails.
;     (alt (seq a A) (seq a B) (seq b A) (seq b B))
;     --> (alt (seq a (alt A B)) (seq b (alt A B))
;     --> (seq (alt (seq a) (seq b)) (alt (seq A) (seq B)))
;   DONE.
;
;   For n-way ALTs, support merging subsets of the children, and n-way
;   branches on leading char.  This should be a big win for the lexer.
;     (alt (seq a) (seq a c) (seq c)) -> (alt (seq a (alt nil (seq c))) (seq c))
;     --> (guarded-alt (a (alt nil (seq c)))
;                   (c ))
;   Since this involves pushing down alts, it conflicts with the
;   alt-merging logic, so we need a way to turn off the merging logic
;   temporarily (we need to group char type branches into a sub-alt,
;   canonicalize them, then re-run the alt-merge at the higher level
;   to re-integrate anything that didn't migrate out. Then at simplify
;   time, we look for runs of alt branches that start with char-type
;   nodes, and group these into guarded-alts.
;   DONE
;
;   Merge single-char/char-class alt clauses into char-class.
;     (seq (alt (seq a) (seq b)) (alt (seq A) (seq B)))
;     --> (seq (charclass "ab") (charclass "AB"))
;   DONE.
;
;   Merge lists of chars and strings into strings.
;   DONE.
;
;   Support lists of 2-char classes (common for case-insensitive matches)
;   DONE
;
;   Eliminate null states in sequences (caused by fully merging
;   alt clause heads, and possibly others).
;   DONE.
;
;   Support fast alt of (<char>|node), (<string>|node),
;                       (<charclass>|node), (<specclass>|node)
;
;   Support merging sequences of 1-valued nodes into a specialized matcher
;   that takes an array of "match" functions that return either nil or the
;   new pos.  If any of them fail, the whole sequence fails.
;
;   If the child of + is something trivial like char, seq of char,
;   any, charclass, specclass, or seq of charclass, leave it as a
;   + node and specialize it during the instruction selection pass.
;
;   \d \D \w \W \s \S \< \>
;   DONE
;
;   Match hooks
;   DONE (in sexpr form only)
;
;   Acceptance functions
;   DONE (in sexpr form only)
;
; Additional features:
;   Forward lookahead, negative lookahead
;   Named captures
;




;;;
;;; Code Generator
;;;

(defun compile-expr-to-matcher (parse-tree &optional str)
  (multiple-value-bind (matchfn numregs simplified-tree)
      (compile-expr-to-matchfn parse-tree)
    (cond (*match-simple-strings-only*
           (make-matcher
            :simple-string-matchfn matchfn
            :string-matchfn nil
            :numregs numregs
            :matchstr str
            :matchexpr simplified-tree))
          (t (make-matcher
              :simple-string-matchfn nil
              :string-matchfn matchfn
              :numregs numregs
              :matchstr str
              :matchexpr simplified-tree)))))

(defun compile-expr-to-matchfn (parse-tree &key (simplifyp t))
  (let* ((numregs (1+ (compute-max-regnum parse-tree)))
         (simple-tree (if simplifyp
                          (optimize-regex-tree parse-tree)
                        parse-tree))
         (instr-tree (select-instructions simple-tree)))
    (multiple-value-bind (start-instr cps-instrs)
        (gen-instr-list instr-tree)
      (when *regex-compile-verbose*
        (format t "~&~%Numregs: ~D" numregs)
        (format t "~&~%Simplified tree:")
        (pprint simple-tree)
        (format t "~&~%Instruction tree:")
        (pprint instr-tree)
        (format t "~&~%CPS instruction list (start = ~D):" start-instr)
       (pprint cps-instrs))
      (let ((closure-info (gen-closures cps-instrs)))
        (link-closures closure-info)
        (let ((matchfn (make-init-closure
                        (remove-if #'null (map 'list
                                               #'closure-info-initfn
                                               closure-info))
                        (resolve-instr closure-info start-instr))))
          (values (make-anchored-matcher matchfn) numregs simple-tree))))))


(defun make-anchored-matcher (matchfn)
  #'(lambda (*str* *regs* *start* *end*
             *start-is-anchor* *end-is-anchor*
             *acceptfn* *hooks*)
      #-:debug-regex (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
      (declare (special *str* *regs* *start* *end*
                        *start-is-anchor* *end-is-anchor*
                        *acceptfn* *hooks*))
      (declare (ftype (function (fixnum) t) matchfn))
      (catch 'cease-matching
	(funcall matchfn *start*))))


;;;
;;; code-expansion version...
;;;

;(defun expand-anchored-matcher (matchfn vars funs)
;  `(lambda (%str% %regs% %start% %end%
;             %start-is-anchor% %end-is-anchor%
;             %acceptfn% %hooks%)
;      #-:debug-regex (declare (optimize (speed 3) (safety 1) (space 0) (debug 0)))
;      (declare (ignorable %str% %regs% %start% %end%
;                        %start-is-anchor% %end-is-anchor%
;                        %acceptfn% %hooks%))
;      (declare (special %str% %regs% %start% %end%
;                        %start-is-anchor% %end-is-anchor%
;                        %acceptfn% %hooks%))
;      `(let ,vars
;         (labels ,funs
;           ;;(declare (inline ,@(mapcar #'first funs)))
;           (catch 'cease-matching
;             (,matchfn %start%))))))


;(defun expand-anchored-matcher (matchfn vars funs)
;  `(locally
;     (declare (special *str* *regs* *start* *end*
;                       *start-is-anchor* *end-is-anchor*
;                       *acceptfn* *hooks*))
;     (labels ,funs
;       #'(lambda (*str* *regs* *start* *end*
;                        *start-is-anchor* *end-is-anchor*
;                        *acceptfn* *hooks*)
;           (declare (special *str* *regs* *start* *end*
;                             *start-is-anchor* *end-is-anchor*
;                             *acceptfn* *hooks*))
;           (let ,vars
;             #-:debug-regex (declare (optimize (speed 3) (safety 1) (space 0) (debug 0)))
;             (catch 'cease-matching
;               (,matchfn *start*)))))))


;(defun expand-expr-to-matchfn (parse-tree &key charfn (simplifyp t))
;  (let* ((numregs (1+ (compute-max-regnum parse-tree)))
;         (simple-tree (if simplifyp
;                          (optimize-regex-tree parse-tree)
;                        parse-tree))
;         (instr-tree (select-instructions simple-tree)))
;    (multiple-value-bind (start-instr cps-instrs)
;        (gen-instr-list instr-tree)
;      (when *regex-compile-verbose*
;        (format t "~&~%Numregs: ~D" numregs)
;        (format t "~&~%Simplified tree:")
;        (pprint simple-tree)
;        (format t "~&~%Instruction tree:")
;        (pprint instr-tree)
;        (format t "~&~%CPS instruction list (start = ~D):" start-instr)
;       (pprint cps-instrs))
;      (let ((code-info (expand-code cps-instrs charfn)))
;        (let ((matchfn
;               (expand-anchored-matcher
;                (fnname-for-state start-instr)
;                (remove-if #'null (mapcar #'second code-info))
;                (mapcar #'(lambda (inf)
;                            `(,(first inf) ,@(rest (third inf))))
;                        code-info))))
;          (values matchfn numregs simple-tree))))))





; search the parse tree, looking for the highest register in use.
(defun compute-max-regnum (node)
  (cond ((seq-node-p node)
         (reduce #'max
                 (mapcar #'compute-max-regnum (seq-node-children node))
                 :initial-value -1))
        ((alt-node-p node)
         (reduce #'max
                 (mapcar #'compute-max-regnum (alt-node-children node))
                 :initial-value -1))
        ((kleene-node-p node)
         (compute-max-regnum (kleene-node-child node)))
        ((pkleene-node-p node)
         (compute-max-regnum (pkleene-node-child node)))
        ((optional-node-p node)
         (compute-max-regnum (optional-node-child node)))
        ((range-node-p node)
         (compute-max-regnum (range-node-child node)))
        ((backmatch-node-p node)
         (backmatch-node-regnum node))
        ((register-node-p node)
         (max (register-node-regnum node)
              (compute-max-regnum (register-node-child node))))
        ((regstart-node-p node)
         (regstart-node-regnum node))
        ((regend-node-p node)
         (regend-node-regnum node))
        ((lookahead-node-p node)
         (compute-max-regnum (lookahead-node-expr node)))
        ((nlookahead-node-p node)
         (compute-max-regnum (nlookahead-node-expr node)))
        (t 0)))



;;;
;;; Pass 7 - Linking
;;;

; resolve the target labels with the actual target closures
(defun link-closures (link-info)
  (loop for info across link-info
        for linkfn = (closure-info-linkfn info)
        when (functionp linkfn)
          do (funcall linkfn link-info)))






;;;;
;;;; Compiled Matcher structure, and high-level functions
;;;;


(defun compile-str (patstr)
  "Parse a string regex expression, and compile it into matcher object.
   Uses the pattern cache."
   ; (format t "~A entries in cache" (hash-table-count *pattern-cache*))
  (let ((cached-machine (gethash patstr *pattern-cache*)))
    (or cached-machine (newly-compiled-str-matcher patstr))))

(defun compile-expr (regexpr)
  "Parse a string regex expression, and compile it into matcher object.
   Uses the pattern cache."
   ; (format t "~A entries in cache" (hash-table-count *pattern-cache*))
  (let ((cached-machine (gethash regexpr *pattern-cache*)))
    (or cached-machine (newly-compiled-expr-matcher regexpr))))


(defun macroexpand-regex-str (patstr)
  `(compile-expr-to-matcher ',(parse-str patstr) ,patstr))

(defmacro defregex (name patstr &rest rest)
  `(defparameter ,name ,(macroexpand-regex-str patstr) ,@rest))

(defmacro macroexpand-regex-expr (regex-expr)
  `(compile-expr-to-matcher ,regex-expr))


;;;
;;; code-expansion version...
;;;

;(defun macroexpand-regex-expr (regex-expr)
;  "Parse a string regex expression, and translate it to lisp code."
;  (multiple-value-bind (matchfn numregs simplified-tree)
;          (expand-expr-to-matchfn regex-expr
;                                  :charfn (if *match-simple-strings-only* 'schar 'char))
;    (declare (ignore simplified-tree))
;    (cond (*match-simple-strings-only*
;           `(make-matcher :simple-string-matchfn ,matchfn
;                          :string-matchfn nil
;                          :numregs ,numregs
;                          :matchstr nil
;                          :matchstr nil))
;          (t `(make-matcher :simple-string-matchfn ,matchfn
;                            :string-matchfn nil
;                            :numregs ,numregs
;                            :matchstr nil
;                            :matchstr nil)))))
        


;(defun macroexpand-regex-str (patstr)
;  "Parse a string regex expression, and translate it to lisp code."
;  (macroexpand-regex-expr (parse-str patstr)))

;(defmacro defregex (name patstr &rest rest)
;  `(defparameter ,name ,(macroexpand-regex-str patstr) ,@rest))




; Try to use the quickest matcher for the input string.  If the
; candidate string isn't a simple string, then match with the slower
; string-matcher.  Since this isn't compiled by default, it may need
; to be compiled from the saved expr.
(defun match-str-all-parms (matcher candstr regs
                            start length
                            start-is-anchor end-is-anchor
                            acceptfn hooks)
  (dotimes (i (length regs))
    (let ((reg (aref regs i)))
      (setf (car reg) nil)
      (setf (cdr reg) nil)))
  (cond ((simple-string-p candstr)
         (cond ((functionp (matcher-simple-string-matchfn matcher))
                (funcall (matcher-simple-string-matchfn matcher)
	                 candstr regs start (+ start length)
                         start-is-anchor end-is-anchor
                         acceptfn hooks))
               ((functionp (matcher-string-matchfn matcher))
                (funcall (matcher-string-matchfn matcher)
	                 candstr regs start (+ start length)
                         start-is-anchor end-is-anchor
                         acceptfn hooks))
               (t (error "REGEX Error: ~S is not a valid regex matcher" matcher))))
        ((stringp candstr)
         (cond ((functionp (matcher-string-matchfn matcher))
                (funcall (matcher-string-matchfn matcher)
                         candstr regs start (+ start length)
                         start-is-anchor end-is-anchor
                         acceptfn hooks))
               ((functionp (matcher-matchexpr matcher))
                (let ((*match-simple-strings-only* nil))
                  (setf (matcher-string-matchfn matcher)
                        (compile-expr-to-matcher (matcher-matchexpr matcher))))
                (unless (matcher-string-matchfn matcher)
                  (error "REGEX Error: ~S does not have a valid match function for class STRING"
                         matcher))
                (funcall (matcher-string-matchfn matcher)
                         candstr regs start (+ start length)
                         start-is-anchor end-is-anchor
                         acceptfn hooks))))
        (t (error "REGEX Error: ~S is not a string" candstr))))

(defun match-str (matcher candstr
                  &key (regs (make-regs (matcher-numregs matcher)))
                       (start 0)
                       (length (- (length candstr) start))
                       (start-is-anchor (= start 0))
                       (end-is-anchor (= length (length candstr)))
                       acceptfn hooks)
"Run a matcher against a candidate string, without scanning
\(so it is implicitly anchored\).  Returns \(values t start end regs\) on
success, nil on failure."
  (match-str-all-parms matcher candstr regs start length
                       start-is-anchor end-is-anchor acceptfn hooks))

(define-compiler-macro match-str (matcher candstr
                                  &key
				  (regs `(make-regs (matcher-numregs ,matcher)))
                                  (start 0)
                                  (length `(- (length ,candstr) ,start))
                                  (start-is-anchor `(= ,start 0))
                                  (end-is-anchor `(= ,length (length ,candstr)))
                                  acceptfn hooks)
  `(match-str-all-parms ,matcher ,candstr
			,regs ,start ,length
                        ,start-is-anchor ,end-is-anchor
                        ,acceptfn ,hooks))



;;; This really needs a prefix-map array in the matcher structure so we can
;;; quickly find potential beginnings to the string
(defun scan-str-all-parms (matcher str regs
                           start length
                           start-is-anchor end-is-anchor
                           acceptfn hooks)
  (declare (type matcher matcher)
           (string str)
           (fixnum start length))
  (let ((matchedp t)
	match-start
        (match-start-pos start)
        (len-remaining length)
        (match-len length)
        (match-regs nil))
    (loop
     (multiple-value-setq (matchedp match-start match-len match-regs)
         (match-str-all-parms matcher str regs match-start-pos len-remaining
                              (and start-is-anchor (= match-start-pos start))
                              end-is-anchor acceptfn hooks))
     (cond
      (matchedp
       (return-from scan-str-all-parms
         (values matchedp match-start match-len match-regs)))
      ((>= match-start-pos (+ start length))
       (return-from scan-str-all-parms nil))
      (t (incf match-start-pos)
         (decf len-remaining))))))

(defun scan-str (matcher candstr
                 &key (regs (make-regs (matcher-numregs matcher)))
                      (start 0)
                      (length (length candstr))
                      (start-is-anchor (= start 0))
                      (end-is-anchor (= length (length candstr)))
                      acceptfn hooks)
"Run a matcher against a candidate string, scanning forward if necessary.
Returns \(values t start end regs\) on success, nil on failure."
  (scan-str-all-parms matcher candstr regs
                      start length
                      start-is-anchor end-is-anchor
                      acceptfn hooks))

(define-compiler-macro scan-str (matcher candstr
                                 &key
                                 (regs `(make-regs (matcher-numregs ,matcher)))
                                 (start 0)
                                 (length `(length ,candstr))
                                 (start-is-anchor `(= ,start 0))
                                 (end-is-anchor `(= ,length (length ,candstr)))
                                 acceptfn hooks)
  `(scan-str-all-parms ,matcher ,candstr
		       ,regs ,start ,length
                       ,start-is-anchor ,end-is-anchor
                       ,acceptfn ,hooks))



(defun uncached-compile-str (patstr)
  (let ((result (catch 'regex-parse-error
		  (compile-expr-to-matcher (parse-str patstr) patstr))))
    (cond
     ((matcher-p result) result)
     (t (apply #'format (cons t result))
        nil))))

(defun uncached-compile-expr (regexpr &optional str)
  (let ((result (catch 'regex-parse-error
		  (compile-expr-to-matcher regexpr str))))
    (cond
     ((matcher-p result) result)
     (t (apply #'format (cons t result))
        nil))))

(defun newly-compiled-str-matcher (patstr)
  (when (>= (hash-table-count *pattern-cache*) +max-regex-str-cache+)
    (clrhash *pattern-cache*))
  (setf (gethash patstr *pattern-cache*)
	(uncached-compile-str patstr)))

(defun newly-compiled-expr-matcher (regexpr)
  (when (>= (hash-table-count *pattern-cache*) +max-regex-str-cache+)
    (clrhash *pattern-cache*))
  (setf (gethash regexpr *pattern-cache*)
	(uncached-compile-expr regexpr)))



;;;
;;; Testing
;;;

(defun testcomp (str)
  (compile-expr-to-matcher (parse-str str) str))

(defun testmatch (str pat &key hooks)
  (clear-pattern-cache)
  (match-str (compile-str pat) str :hooks hooks))

