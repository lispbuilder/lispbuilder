;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LISPBUILDER-REGEX; Base: 10 -*-

(in-package :LISPBUILDER-REGEX)




;;;
;;; Pass 3 - Canonicalization/tree rewrites/simplification
;;;

; (class "c") --> #\c
; (seq <char> <char> ... <non-char> ...) --> (seq <str> <non-char> ...)
; (seq a b (seq c d) e f) --> (seq a b c d e f)
; (alt a b (alt c d) e f) --> (alt a b c d e f)
; (reg n <expr>) --> (seq (rstart n) <expr> (rend n))
; (opt a) --> (alt a nil) or (alt nil a)

; Iteratively canonicalize the tree, until it stabilizes, then simplify
(defun optimize-regex-tree (tree)
  (let* ((canonical (canonicalize tree))
         (better (improve canonical))
         (even-better (split-alts better))
         (simple (simplify even-better)))
    simple))


(defun canonicalize (tree)
  (when *regex-compile-verbose*
    (format t "~&~%Canonicalize Start:")
    (pprint tree))
  (loop with prev-tree = tree
        for pass from 1
        for new-tree = (canonicalize-once prev-tree)
        when (or (equal prev-tree new-tree) (> pass 10))
          return new-tree
        do (progn
             (when *regex-compile-verbose*
               (format t "~&Canonicalize Pass ~D:" pass)
               (pprint new-tree))
             (setq prev-tree new-tree))))


(defun improve (tree)
  (when *regex-compile-verbose*
    (format t "~&~%Optimize Start:")
    (pprint tree))
  (loop with prev-tree = tree
        for pass from 1
        for new-tree = (improve-once prev-tree)
        when (or (equal prev-tree new-tree) (> pass 10))
          return new-tree
        do (progn
             (when *regex-compile-verbose*
               (format t "~&Optimize Pass ~D:" pass)
               (pprint new-tree))
             (setq prev-tree new-tree))))
  

(defun split-alts (tree)
  (when *regex-compile-verbose*
    (format t "~&~%Split Alts Start:")
    (pprint tree))
  (split-alts-aux tree))
  

(defun simplify (tree)
  (when *regex-compile-verbose*
    (format t "~&~%Simplify Start:")
    (pprint tree))
  (loop with prev-tree = tree
        for pass from 1
        for new-tree = (simplify-once prev-tree)
        when (or (equal prev-tree new-tree) (> pass 10))
          return new-tree
        do (progn
             (when *regex-compile-verbose*
               (format t "~&Simplify Pass ~D:" pass)
               (pprint new-tree))
             (setq prev-tree new-tree))))



; for ALT's, after hoisting, try partitioning the set on the leading element,
; and see if we can't reduce things further:
;   (alt (seq a) (seq a c) (seq c)) -> (alt (seq a (alt nil (seq c))) (seq c))
;   --> (alt-case (a (alt nil (seq c)))
;                 (c ))
;
; for Kleene's, check to see if the child node is also a kleene w/ no
; registers, and if so remove the inner kleene.
;
(defun canonicalize-once (node)
  (cond ;; expand strings into sequences of char to enable more optimizations
        ((string-node-p node)
         (make-seq-node-list (coerce (string-node-string node) 'list)))
        ;; expand class-sequences into sequences of charclass
        ((classseq-node-p node)
         (make-seq-node-list (mapcar #'make-charclass-node
                                     (classseq-node-seq node))))
        ((seq-node-p node)
         (let* ((canonicalchildren (mapcar #'canonicalize-once
                                           (seq-node-children node)))
                (denullchildren (remove-if #'null canonicalchildren))
                (flatchildren (flatten-sequence denullchildren))
                (children flatchildren)
                (numchildren (length flatchildren)))
           (cond ((zerop numchildren) nil)
                 ((= numchildren 1) (first children))
                 (t (make-seq-node-list children)))))
        ((alt-node-p node)
         (let* ((children (mapcar #'canonicalize-once (alt-node-children node)))
                (flatchildren (flatten-alt children))
                (uniquechildren (remove-duplicates flatchildren :from-end t))
                (children uniquechildren))
           (make-alt-node-list children)))
        ((kleene-node-p node)
         (make-kleene-node (canonicalize-once (kleene-node-child node))
                           (kleene-node-greedy-p node)))
        ;; If the child node is something trivial like char, seq of char,
        ;; any, charclass, specclass,or seq of charclass, may want to go
        ;; ahead and leave it as a + node and specialize it during the
        ;; instruction selection pass.
        ((pkleene-node-p node)
         (let ((greedyp (pkleene-node-greedy-p node))
               (canonical-child (canonicalize-once (pkleene-node-child node))))
           (make-seq-node-args
            canonical-child
            (make-kleene-node
             (cond (*registers-match-rightmost* canonical-child)
                   (t (canonicalize-once (unregister canonical-child))))
             greedyp))))
        ((optional-node-p node)
         (let ((greedyp (optional-node-greedy-p node))
               (canonical-child (canonicalize-once (optional-node-child node))))
           (cond (greedyp
                  (make-alt-node-args canonical-child nil))
                 (t (make-alt-node-args nil canonical-child)))))
        ((charclass-node-p node)
         (let* ((negp (charclass-node-negated-p node))
                (chars (charclass-node-chars node))
                (cclen (length chars)))
           (cond ((zerop cclen)
                  nil)
                 ((and (= cclen 1) (not negp))
                  (make-char-node (char chars 0)))
                 (t node))))
        ((register-node-p node)
         (let ((regnum (register-node-regnum node)))
           (make-seq-node-args
            (make-regstart-node regnum)
            (canonicalize-once (register-node-child node))
            (make-regend-node regnum))))
        ((range-node-p node)
         (expand-range (range-node-greedy-p node)
                       (range-node-min node) (range-node-max node)
                       (canonicalize-once (range-node-child node))))
        ((lookahead-node-p node)
         (make-lookahead-node (canonicalize-once (lookahead-node-expr node))))
        ((nlookahead-node-p node)
         (make-nlookahead-node (canonicalize-once (nlookahead-node-expr node))))
        (t node)))



(defun improve-once (node)
  (cond ((alt-node-p node)
         (let ((children (mapcar #'improve-once (alt-node-children node))))
           (multiple-value-bind (prefix altbody suffix)
               (hoist-alt-ends children)
             (cond (altbody
                    (make-seq-node-list `(,@prefix
                                          ,(make-alt-node-list altbody)
                                          ,@suffix)))
                   ((and (or prefix suffix) (null altbody))
                    (make-seq-node-list `(,@prefix ,@suffix)))
                   (t
                    (make-alt-node-list altbody))))))
        ((seq-node-p node)
         (make-seq-node-list (mapcar #'improve-once (seq-node-children node))))
        ((kleene-node-p node)
         (let* ((greedyp (kleene-node-greedy-p node))
                (child (improve-once (kleene-node-child node)))
                (hasregs (contains-registers-p child)))
           (cond ((or *registers-match-rightmost* (not hasregs))
                  (make-kleene-node child greedyp))
                 (greedyp
                  (let ((more (make-kleene-node (unregister child)
                                                greedyp)))
                    (make-alt-node-args (make-seq-node-args
                                         child
                                         more)
                                        nil)))
                 (t ;; not greedy
                    (let ((more (make-kleene-node (unregister child)
                                                  greedyp)))
                      (make-alt-node-args
                       nil
                       (make-seq-node-args child more)))))))
        ((lookahead-node-p node)
         (make-lookahead-node (improve-once (lookahead-node-expr node))))
        ((nlookahead-node-p node)
         (make-nlookahead-node (improve-once (nlookahead-node-expr node))))
        (t node)))


(defun split-alts-aux (node)
  (cond ((seq-node-p node)
         (let ((children (mapcar #'split-alts-aux (seq-node-children node))))
           (make-seq-node-list children)))
        ((alt-node-p node)
         (let ((children (mapcar #'split-alts-aux (alt-node-children node)))
               (num-children (alt-node-numchildren node)))
           (multiple-value-bind (unknown-char known-char-sets)
               (partition-on-leading-char children)
             (let* ((num-unknown-char (length unknown-char))
                    (num-known-char-sets (length known-char-sets))
                    (worth-partitioning-p
                     (worth-alt-case-partitioning-p num-children
                                                    num-unknown-char
                                                    num-known-char-sets)))
               (cond ((and unknown-char known-char-sets worth-partitioning-p)
                      (make-alt-node-list
                       `(,(make-casealt-node-list (mapcar #'subalt-if-necessary known-char-sets))
                         ,@unknown-char)))
                     ((and (not unknown-char) known-char-sets worth-partitioning-p)
                      (make-casealt-node-list (mapcar #'subalt-if-necessary known-char-sets)))
                     (t node))))))
        ((kleene-node-p node)
         (make-kleene-node (split-alts-aux (kleene-node-child node))
                           (kleene-node-greedy-p node)))
        ((lookahead-node-p node)
         (make-lookahead-node (split-alts-aux (lookahead-node-expr node))))
        ((nlookahead-node-p node)
         (make-nlookahead-node (split-alts-aux (nlookahead-node-expr node))))
        (t node)))


(defun simplify-once (node)
  (cond ((seq-node-p node)
         (let ((newchildren (combine-sequence-text
                             (mapcar #'simplify-once (seq-node-children node)))))
           (cond ((= (length newchildren) 1)
                  (first newchildren))
                 (t (make-seq-node-list newchildren)))))
        ((alt-node-p node)
         (let ((newalts (combine-alt-charclass
                         (mapcar #'simplify-once (alt-node-children node)))))
           (cond ((= (length newalts) 1)
                  (first newalts))
                 (t (make-alt-node-list newalts)))))
        ((casealt-node-p node)
         (make-casealt-node-list (mapcar #'(lambda (arm)
                                             (list (first arm) (simplify-once (second arm))))
                                         (casealt-node-children node))))
        ((kleene-node-p node)
         (make-kleene-node (simplify-once (kleene-node-child node))
                           (kleene-node-greedy-p node)))
        ((lookahead-node-p node)
         (make-lookahead-node (simplify-once (lookahead-node-expr node))))
        ((nlookahead-node-p node)
         (make-nlookahead-node (simplify-once (nlookahead-node-expr node))))
        (t node)))



; expand out a range
(defun expand-range (greedyp lowbound highbound node)
  (cond
   ((and *registers-match-rightmost* (numberp highbound))
    (let* ((reqd
            (loop for i from 0 below lowbound
                  collect node))
           (opt
            (loop with tmp = nil
                  for i from lowbound below highbound
                  when (= i lowbound)
                  do (setq tmp (make-optional-node node greedyp))
                  when (> i lowbound)
                  do (let ((seq (make-seq-node-args node tmp)))
                       (setq tmp
                             (cond (greedyp
                                    (make-alt-node-args seq nil))
                                   (t (make-alt-node-args nil seq)))))
                  finally (return tmp))))
      (make-seq-node-list `(,@reqd ,opt))))
   ((and *registers-match-rightmost* (null highbound))
    (let* ((reqd (loop for i from 0 below lowbound
                       collect node)))
      (make-seq-node-list `(,@reqd ,(make-kleene-node node greedyp)))))
   ((and (not *registers-match-rightmost*) (numberp highbound))
    (let* ((registerless-node (unregister node))
           (reqd
            (loop for i from 0 below lowbound
                  when (zerop i) collect node
                  when (> i 0) collect registerless-node))
           (opt
            (loop with tmp = nil
                  for i from lowbound below highbound
                  when (= i lowbound)
                  do (setq tmp
                           (make-optional-node (if (zerop lowbound)
                                                   node
                                                 registerless-node)
                                               greedyp))
                  when (> i lowbound)
                  do (let ((seq (make-seq-node-args registerless-node tmp)))
                       (setq tmp
                             (cond (greedyp
                                    (make-alt-node-args seq nil))
                                   (t (make-alt-node-args nil seq)))))
                  finally (return tmp))))
      (make-seq-node-list `(,@reqd ,opt))))
   (t (let* ((registerless-node (unregister node))
             (reqd
              (loop for i from 0 below lowbound
                    when (zerop i) collect node
                    when (> i 0) collect registerless-node)))
        (cond ((zerop lowbound)
               (make-kleene-node node greedyp))
              (t (make-seq-node-list `(,@reqd ,(make-kleene-node registerless-node greedyp)))))))))

(defun coercetostring (x)
  (cond ((stringp x) x)
        ((characterp x) (string x))))

; unnest sequences where possible
(defun flatten-sequence (nodes)
  (cond ((null nodes) nil)
        ((seq-node-p (first nodes))
         (flatten-sequence (append (seq-node-children (first nodes))
                                   (rest nodes))))
        (t (cons (first nodes)
                 (flatten-sequence (rest nodes))))))

; combine runs of chars and strings into strings
; combine runs of character classes into charclass-sequence's
(defun combine-sequence-text (nodes)
  (cond ((null nodes) nil)
        ((string-seq-p nodes)
         (multiple-value-bind (str restseq)
             (partition-string-sequence nodes)
           (cons (make-string-node str)
                 (combine-sequence-text restseq))))
        ((char-class-seq-p nodes)
         (multiple-value-bind (classseq restseq)
             (partition-charclass-sequence nodes)
           (cons (make-classseq-node classseq)
                 (combine-sequence-text restseq))))
        (t (cons (first nodes)
                 (combine-sequence-text (rest nodes))))))

; combine multiple character classes in an ALT into one character class
(defun combine-alt-charclass (nodes)
  (cond ((>= (count-if #'char-or-class-node-p nodes) 2)
         (multiple-value-bind (chars othernodes)
             (partition-charclass-alt nodes)
           (cons (make-charclass-node chars)
                 othernodes)))
        (t nodes)))

;; does this sequence start out with a run of chars?
(defun string-seq-p (seq)
  (loop for item in seq
        for i from 0
        while (text-node-p item)
        when (>= i 1) return t) )

;; does this sequence start out with a run of char-class?
(defun char-class-seq-p (seq)
  (loop for item in seq
        for i from 0
        while (positive-charclass-node-p item)
        when (>= i 1) return t) )

;; partitions sequences into a string representing the leading run,
;; and the rest of the sequence
(defun partition-string-sequence (seq)
  (loop with strseq = nil
        for item = (pop seq)
        when (text-node-p item) do (push item strseq)
        when (not (text-node-p item))
        return (values (append-strings (mapcar #'coercetostring (reverse strseq)))
                       (cond ((and (null item) (null seq)) nil)
                             ((null seq) (list item))
                             (t (cons item seq))))))

(defun append-strings (strings)
  (cond ((null strings) "")
        (t (loop for str in strings
                 for result = str then (concatenate 'simple-string result str)
                 finally (return result)))))

(defun partition-charclass-sequence (seq)
  (loop with classseq = nil
        for item = (pop seq)
        when (positive-charclass-node-p item) do (push item classseq)
        when (not (charclass-node-p item))
        return (values (mapcar #'classseq-node-seq (reverse classseq))
                       (cond ((and (null item) (null seq)) nil)
                             ((null seq) (list item))
                             (t (cons item seq))))))

(defun partition-charclass-alt (nodes)
  (loop with chars = ""
        for node = (pop nodes)
        when (charclass-node-p node)
          do (setq chars (concatenate 'string chars (charclass-node-chars node)))
        when (char-node-p node)
          do (setq chars (concatenate 'string chars (string (char-node-char node))))
        when (not (char-or-class-node-p node))
        return (values chars
                       (cond ((and (null node) (null nodes)) nil)
                             ((null nodes) (list node))
                             (t (cons node nodes))))))

(defun flatten-alt (nodes)
  (cond ((null nodes) nil)
        ((alt-node-p (first nodes))
         (flatten-alt (append (alt-node-children (first nodes)) (rest nodes))))
        (t (cons (first nodes)
                 (flatten-alt (rest nodes))))))

(defun hoist-alt-ends (nodes)
  (multiple-value-bind (prefix restnodes)
      (hoist-alt-prefix nodes)
    (multiple-value-bind (altbody suffix)
        (hoist-alt-suffix restnodes)
      (values prefix altbody suffix))))

(defun seq-first (node)
  (cond ((seq-node-p node)
         (first (seq-node-children node)))
        (t node)))
(defun seq-first-char (node)
  (cond ((seq-node-p node)
         (let ((children (seq-node-children node)))
           (loop for child in children
                 when (or (alt-node-p child)
                          (kleene-node-p child)
                          (specclass-node-p child)
                          (hook-node-p child)
                          (backmatch-node-p child)
                          (lookahead-node-p child)
                          (nlookahead-node-p child)) return nil
                 when (or (charclass-node-p child)
                          (char-node-p child)) return child)))
        (t nil)))

(defun seq-rest (node)
  (cond ((seq-node-p node)
         (make-seq-node-list (rest (seq-node-children node))))
        (t nil)))

;; unlike CL's last, this returns the last car, not the last cons cell
(defun seq-last (node)
  (cond ((seq-node-p node)
         (car (last (seq-node-children node))))
        (t node)))

(defun seq-butlast (node)
  (cond ((seq-node-p node)
         (make-seq-node-list (butlast (seq-node-children node))))
        (t nil)))
  
(defun hoist-alt-prefix (nodes)
  (let ((prefixes (mapcar #'seq-first nodes))
        (rests (mapcar #'seq-rest nodes)))
    (cond ((or (null prefixes) (some #'null prefixes))
           (values nil nodes))
          ((every #'equal prefixes (rest prefixes))
           (cond ((or (null rests) (every #'null rests))
                  (values (list (first prefixes)) nil))
                 ((or (null rests) (some #'null rests))
                  (values (list (first prefixes)) rests))
                 (t (multiple-value-bind (other-prefixes altnodes)
                        (hoist-alt-prefix rests)
                      (values (cons (first prefixes) other-prefixes)
                              altnodes)))))
          (t (values nil nodes)))))

(defun hoist-alt-suffix (nodes)
  (let ((suffixes (mapcar #'seq-last nodes))
        (butlasts (mapcar #'seq-butlast nodes)))
    (cond ((or (null suffixes) (some #'null suffixes))
           (values nodes nil))
          ((every #'equal suffixes (rest suffixes))
           (cond ((or (null butlasts) (some #'null butlasts))
                  (values butlasts (list (first suffixes))))
                 (t (multiple-value-bind (altnodes other-suffixes)
                        (hoist-alt-suffix butlasts)
                      (values altnodes (cons (first suffixes) other-suffixes))))))
          (t (values nodes nil)))))


; (alt "aaa" "bbb" "bbc" "ccc" "ccd" (* #\e))
;  -->
; ((* #\e))
; ((#\a "aaa")
;  (#\b "bbb" "bbc")
;  (#\c "ccc" "ccd"))

;; Now handles character classes as well
(defun partition-on-leading-char (children)
  (let* ((leading-char-alist (make-discriminant-char-alist children))
         (children-without-leading-char (mapcar #'second (remove-if #'first leading-char-alist)))
         (children-with-leading-char (remove-if-not #'first leading-char-alist))
         (sorted-lc-children (sort children-with-leading-char #'sort-lc-pred :key #'first)))
    (loop with partition = nil
          for (prev-leading-char prev-childnode) in sorted-lc-children
          for (leading-char childnode) in (rest sorted-lc-children)
          for current-set = (list prev-childnode) then current-set
          when (not (equalp prev-leading-char leading-char))
          do (progn
               (push (cons prev-leading-char (reverse current-set)) partition)
               (setq current-set (list childnode)))
          when (equalp prev-leading-char leading-char)
          do (push childnode current-set)
          finally (return (values children-without-leading-char
                                  (reverse (cons (cons leading-char (reverse current-set))
                                                 partition)))))))

(defun sort-lc-pred (a b)
  (cond ((and (characterp a) (characterp b)) (char< a b))
        ((and (characterp a) (stringp b)) (char< a (char b 0)))
        ((and (stringp a) (characterp b)) (char< (char a 0) b))))


(defun make-discriminant-char-alist (nodes &aux alist)
  (dolist (node nodes alist)
    (setq alist (nconc alist (get-discriminant-chars-alist node)))))

;; if first in sequence is a positive charclass, return all chars as the discriminant
(defun get-discriminant-chars-alist (node)
  (let ((first (seq-first-char node)))
    (cond ((char-node-p first)
           (list (list (char-node-char first) node)))
          ((charclass-node-p first)
           (loop for x across (charclass-node-chars first)
                 collect (list x node)))
          (t (list (list nil node))))))

(defun worth-alt-case-partitioning-p (num-children num-unknown-char-sets num-known-char-sets)
  "Is it worth partitioning an alt into a case-alt?"
  (and  ;; Partition must split out at least 4 alternatives
        (>= (- num-children num-unknown-char-sets) 4)
        ;; Must be more than one set in the alt-case (the case of 1 should
        ;; be handled separately)
        (> num-known-char-sets 1)))

(defun subalt-if-necessary (casealt-arm)
  (let ((len (length (rest casealt-arm))))
    (cond ((zerop len)
           (error "sub-alt must have at least one clause ~S" casealt-arm))
          ((= len 1)
           `(,(first casealt-arm) ,(make-seq-node-args (second casealt-arm))))
          (t `(,(first casealt-arm) ,(make-alt-node-list (rest casealt-arm)))))))


(defun obviously-nullable-pattern (tree)
  (and (listp tree)
       (or (not (null (member (first tree) '(* *?))))
           (and (eq (first tree) 'alt) (<= (length tree) 2)))))

(defun contains-looping-pattern-p (tree)
  (tree-any #'(lambda (x) (member x '(* + *? +?) :test #'eq)) tree))

(defun contains-registers-p (tree)
  (tree-any #'(lambda (x) (member x '(reg regstart regend))) tree))

(defun tree-any (fxn tree)
  (labels ((tree-any-aux (tree)
             (cond
              ((null tree) nil)
              ((atom tree) (funcall fxn tree))
              (t (or (funcall fxn tree)
                     (safer-some #'tree-any-aux tree))))))
    (tree-any-aux tree)))

;; Similar to CL's SOME, but doesn't barf on improper lists.
(defun safer-some (fxn lst)
  (loop while (consp lst)
        for x = (pop lst)
        when (funcall fxn x) return t
        finally (return (cond ((null lst) nil)
                              (t (funcall fxn lst))))))

(defun unregister (node)
  (cond
   ((null node) nil)
   ((char-node-p node) node)
   ((string-node-p node) node)
   ((classseq-node-p node) node)
   ((backmatch-node-p node) node)
   ((seq-node-p node)
    (make-seq-node-list
     (remove-if #'null (mapcar #'unregister (seq-node-children node)))))
   ((kleene-node-p node)
    (make-kleene-node (unregister (kleene-node-child node))
                      (kleene-node-greedy-p node)))
   ((pkleene-node-p node)
    (make-pkleene-node (unregister (pkleene-node-child node))
                       (pkleene-node-greedy-p node)))
   ((optional-node-p node)
    (make-optional-node (unregister (optional-node-child node))
                        (optional-node-greedy-p node)))
   ((range-node-p node)
    (make-range-node (unregister (range-node-child node))
                     (range-node-min node)
                     (range-node-max node)
                     (range-node-greedy-p node)))
   ((alt-node-p node)
    ; don't descend into alt nodes -- we may match one branch one iter, and another
    ; branch the next.
    ;(make-alt-node-list (mapcar #'unregister (alt-node-children node))))
    node)
   ((start-anchor-node-p node) node)
   ((end-anchor-node-p node) node)
   ((register-node-p node)
    (unregister (register-node-child node)))
   ((regstart-node-p node)
    nil)
   ((regend-node-p node)
    nil)
   ((charclass-node-p node)
    (make-charclass-node (charclass-node-chars node)
                         :negated (charclass-node-negated-p node)))
   ((specclass-node-p node)
    (make-specclass-node (specclass-node-class node)
                         :negated (specclass-node-negated-p node)))
   ((any-node-p node) node)
   ((hook-node-p node) node)
   ((lookahead-node-p node)
    (make-lookahead-node (unregister (lookahead-node-expr node))))
   ((nlookahead-node-p node)
    (make-nlookahead-node (unregister (nlookahead-node-expr node))))
   (t ;; once we're done, this should throw the :invalid-parse-tree tag
      (throw 'regex-parse-error
             (list "unregister: Unrecognized regex parse tree node ~S"
                   node)))) )






;;;
;;; Pass 4 - Instruction selection
;;;

; replace *, +, etc with the actual instructions to be used...
(defun select-instructions (node)
  (cond
   ((null node) nil)
   ((char-node-p node)
    (select-char-instr (char-node-char node)))
   ((string-node-p node)
    (select-string-instr (string-node-string node)))
   ((classseq-node-p node)
    (select-classseq-instr (classseq-node-seq node)))
   ((backmatch-node-p node)
    node)
   ((seq-node-p node)
    (select-sequence-instrs (seq-node-children node)))
   ((kleene-node-p node)
    (cond ((kleene-node-greedy-p node)
           (select-greedy-kleene-instr (kleene-node-child node)))
          (t (select-nongreedy-kleene-instr (kleene-node-child node)))))
   ;; pkleene-nodes have been removed by the simplification process
   ((optional-node-p node)
    (cond ((optional-node-greedy-p node)
         (select-greedy-optional-instr (optional-node-child node)))
        (t (select-nongreedy-optional-instr (optional-node-child node)))))
   ;; range-nodes have been removed by simplification process
   ((alt-node-p node)
    (select-alt-instrs (alt-node-children node)))
   ((casealt-node-p node)
    (select-casealt-instr (casealt-node-children node)))
   ((start-anchor-node-p node)
    node)
   ((end-anchor-node-p node)
    node)
   ;; register-nodes have been removed by simplification process
   ((regstart-node-p node)
    (let ((regnum (regstart-node-regnum node)))
      (cond (*registers-match-rightmost*
             `(right-rstart ,regnum))
            (t `(left-rstart ,regnum)))))
   ((regend-node-p node)
    node)
   ((charclass-node-p node)
    (let ((chars (charclass-node-chars node)))
      (cond ((not (charclass-node-negated-p node))
             (select-charclass-instr chars))
            (t (select-negated-charclass-instr chars)))))
   ((specclass-node-p node)
    (cond ((not (specclass-node-negated-p node))
           (select-specclass-instr (specclass-node-class node)))
          (t (select-negated-specclass-instr (specclass-node-class node)))))
   ((any-node-p node)
    node)
   ((hook-node-p node)
    node)
   ((success-node-p node)
    node)
   ((startword-node-p node)
    node)
   ((endword-node-p node)
    node)
   ((lookahead-node-p node)
    (make-lookahead-node (select-instructions (lookahead-node-expr node))))
   ((nlookahead-node-p node)
    (make-nlookahead-node (select-instructions (nlookahead-node-expr node))))
   (t ;; once we're done, this should throw the :invalid-parse-tree tag
      (throw 'regex-parse-error
             (list "select-instructions: Unhandled regex parse tree node ~S"
                   node)))))

(defun select-char-instr (chr)
  `(char ,chr))

(defun select-string-instr (str)
  ;; the pattern string must always be a simple-string
  (let ((simple-pat-str (coerce str 'simple-string)))
    `(string ,simple-pat-str)))

(defun select-classseq-instr (classseq)
  `(classseq ,classseq))

(defun select-sequence-instrs (children)
  (make-seq-node-list (mapcar #'select-instructions children)))

;; optimize (alt-2 <char> <node>)
;; optimize (alt-2 <string> <node>)
;; optimize (alt-2 <charclass> <node>)
;; optimize (alt-2 <specclass> <node>)
(defun select-alt-instrs (children)
  (make-alt-node-list (mapcar #'select-instructions children)))

(defun select-casealt-instr (children)
  (make-casealt-node-list (mapcar #'(lambda (arm)
                                      (list (first arm)
                                            (select-instructions (second arm))))
                                  children)))


;; By this point, we have already been unrolled to move registers out
;; of loops, so we can just worry about the special cases.
(defun select-greedy-kleene-instr (child &aux (nullpat (nullable-pattern-p child))
                                         (looppat (contains-looping-pattern-p child)))
  (cond ((char-node-p child)
         `(char-greedy-kleene ,(char-node-char child)))
        ((string-node-p child)
         ;; the pattern string must always be a simple-string
         (let ((simple-pat-str (coerce (string-node-string child)
                                       'simple-string)))
           `(str-greedy-kleene ,simple-pat-str)))
        ((charclass-node-p child)
         (let* ((negp (charclass-node-negated-p child))
                (chars (charclass-node-chars child))
                (ccsize (length chars)))
           (case ccsize
             (0 '(any-greedy-kleene))
             (1 (cond ((not negp)
                       `(char-greedy-kleene ,(char chars 0)))
                      (negp
                       `(not-char-greedy-kleene ,(char chars 0)))))
             (2 (cond ((not negp)
                       `(cclass-2-greedy-kleene ,(char chars 0)
                                                ,(char chars 1)))
                      (negp
                       `(not-cclass-2-greedy-kleene ,(char chars 0)
                                                    ,(char chars 1)))))
             (t (let ((schars (coerce chars 'simple-string)))
                  (cond ((not negp)
                         `(cclass-greedy-kleene ,schars))
                        (t `(not-cclass-greedy-kleene ,schars))))))))
        ((specclass-node-p child)
         (let* ((negp (specclass-node-negated-p child))
                (specclass (specclass-node-class child)))
           (cond ((not negp)
                  `(specclass-greedy-kleene ,specclass))
                 (negp
                  `(not-specclass-greedy-kleene ,specclass)))))
        ((not nullpat)
         `(greedy-kleene-no-termcheck ,(select-instructions child)))
        ((or (and (not nullpat) looppat) (and nullpat (not looppat)))
         `(greedy-kleene-simple-termcheck ,(select-instructions child)))
        (t `(greedy-kleene-full-termcheck ,(select-instructions child)))) )

(defun select-nongreedy-kleene-instr (child)
  (let ((nullpat (nullable-pattern-p child))
        (looppat (contains-looping-pattern-p child)))
    (cond ((not nullpat)
           `(ngkleene-no-termcheck ,(select-instructions child)))
          ((or (and (not nullpat) looppat) (and nullpat (not looppat)))
           `(ngkleene-simple-termcheck ,(select-instructions child)))
          (t `(ngkleene-full-termcheck ,(select-instructions child))))) )

(defun select-greedy-optional-instr (child)
  (make-alt-node-args (select-instructions child) nil))

(defun select-nongreedy-optional-instr (child)
  (make-alt-node-args nil (select-instructions child)))

(defun select-charclass-instr (chars)
  (let ((ccsize (length chars)))
    (case ccsize
      (1 (select-char-instr (char chars 0)))
      (2 `(cclass-2 ,(char chars 0) ,(char chars 1)))
      (t (let ((simple-chars (coerce chars 'simple-string)))
           `(cclass ,simple-chars))))))

(defun select-negated-charclass-instr (chars)
  (let ((ccsize (length chars)))
    (case ccsize
      (1 `(not-char ,(char chars 0)))
      (2 `(not-cclass-2 ,(char chars 0) ,(char chars 1)))
      (t (let ((simple-chars (coerce chars 'simple-string)))
           `(not-cclass ,simple-chars))))))

(defun select-specclass-instr (class)
  `(specclass ,class))

(defun select-negated-specclass-instr (class)
  `(not-specclass ,class) )

(defun nullable-pattern-p (node)
  (cond
   ((null node) t)
   ((char-node-p node) nil)
   ((string-node-p node) nil)
   ((classseq-node-p node) nil)
   ((backmatch-node-p node) t)
   ((seq-node-p node)
    (every #'nullable-pattern-p (seq-node-children node)))
   ((kleene-node-p node) t)
   ((pkleene-node-p node)
    (nullable-pattern-p (pkleene-node-child node)))
   ((optional-node-p node) t)
   ((range-node-p node) (zerop (range-node-min node)))
   ((alt-node-p node)
    (some #'nullable-pattern-p (alt-node-children node)))
   ((start-anchor-node-p node) t)
   ((end-anchor-node-p node) t)
   ((register-node-p node)
    (nullable-pattern-p (register-node-child node)))
   ((regstart-node-p node)
    t)
   ((regend-node-p node)
    t)
   ((charclass-node-p node) nil)
   ((specclass-node-p node) nil)
   ((any-node-p node) nil)
   ((hook-node-p node) t)
   ((lookahead-node-p node) t)
   ((nlookahead-node-p node) t)
   (t ;; once we're done, this should throw the :invalid-parse-tree tag
      (throw 'regex-parse-error
             (list "nullable-pattern-p: Unrecognized regex parse tree node ~S"
                   node)))) )




