;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LISPBUILDER-REGEX; Base: 10 -*-

(in-package :LISPBUILDER-REGEX)

;;;
;;; Pass 5 - Generate list of CPS instructions
;;;
; Generate a flattened list of instructions, with continuation
; targets, and (init) and (success) in the proper places...

(defun gen-instr-list (tree)
  (let* ((*instrs*)
	 (*next-instr* 0))
    (declare (special *instrs* *next-instr*))
    (let* ((success-instr (alloc-instr))
           (start-instr (emit-instr tree success-instr)))
      (add-instr success-instr '(success t))
      (values start-instr 
              (sort *instrs* #'< :key #'first)))))

(defun add-instr (statenum instr)
  (declare (special *instrs*))
  (push (cons statenum instr) *instrs*)
  statenum)

(defun alloc-instr ()
  (declare (special *next-instr*))
  (let ((instr *next-instr*))
    (incf *next-instr*)
    instr))



; similar to old compile-state-machine
(defun emit-instr (node next)
  (cond
   ((null node) next)
   ((eq (first node) 'char)
    (add-instr (alloc-instr) `(char ,(second node) ,next)))
   ((eq (first node) 'string)
    (add-instr (alloc-instr) `(string ,(second node) ,next)))
   ((classseq-node-p node)
    (add-instr (alloc-instr) `(classseq ,(classseq-node-seq node) ,next)))
   ((backmatch-node-p node)
    (add-instr (alloc-instr) `(backmatch ,(backmatch-node-regnum node) ,next)))
   ((seq-node-p node)
    (emit-sequence (seq-node-children node) next))
   ;; kleene-nodes have been optimized to specialized instructions
   ((eq (first node) 'str-greedy-kleene)
    (add-instr (alloc-instr)
               `(str-greedy-kleene ,(second node) ,next)))
   ((eq (first node) 'char-greedy-kleene)
    (add-instr (alloc-instr)
               `(char-greedy-kleene ,(second node) ,next)))
   ((eq (first node) 'not-char-greedy-kleene)
    (add-instr (alloc-instr)
               `(not-char-greedy-kleene ,(second node) ,next)))
   ((eq (first node) 'cclass-2-greedy-kleene)
    (add-instr (alloc-instr)
               `(cclass-2-greedy-kleene ,(second node) ,(third node) ,next)))
   ((eq (first node) 'not-cclass-2-greedy-kleene)
    (add-instr (alloc-instr)
               `(not-cclass-2-greedy-kleene ,(second node) ,(third node) ,next)))
   ((eq (first node) 'cclass-greedy-kleene)
    (add-instr (alloc-instr) `(cclass-greedy-kleene ,(second node) ,next)))
   ((eq (first node) 'not-cclass-greedy-kleene)
    (add-instr (alloc-instr) `(not-cclass-greedy-kleene ,(second node) ,next)))
   ((eq (first node) 'specclass-greedy-kleene)
    (add-instr (alloc-instr) `(specclass-greedy-kleene ,(second node) ,next)))
   ((eq (first node) 'not-specclass-greedy-kleene)
    (add-instr (alloc-instr) `(not-specclass-greedy-kleene ,(second node) ,next)))
   ((eq (first node) 'greedy-kleene-no-termcheck)
    (emit-greedy-kleene-no-termcheck (second node) next))
   ((eq (first node) 'greedy-kleene-simple-termcheck)
    (emit-greedy-kleene-simple-termcheck (second node) next))
   ((eq (first node) 'greedy-kleene-full-termcheck)
    (emit-greedy-kleene-full-termcheck (second node) next))
   ((eq (first node) 'ngkleene-no-termcheck)
    (emit-ngkleene-no-termcheck (second node) next))
   ((eq (first node) 'ngkleene-simple-termcheck)
    (emit-ngkleene-simple-termcheck (second node) next))
   ((eq (first node) 'ngkleene-full-termcheck)
    (emit-ngkleene-full-termcheck (second node) next))
   ;; pkleene-nodes are long since converted to opt and kleene nodes
   ;; optional nodes are long since converted to alt nodes
   ;; range-nodes are long since gone...
   ((alt-node-p node)
    (emit-alt (alt-node-children node) next))
   ((casealt-node-p node)
    (emit-casealt (casealt-node-children node) next))
   ((start-anchor-node-p node)
    (add-instr (alloc-instr) `(startanchor ,next)))
   ((end-anchor-node-p node)
    (add-instr (alloc-instr) `(endanchor ,next)))
   ;; register-nodes are long since gone, converted to regstart/regend
   ((eq (first node) 'right-rstart)
    (add-instr (alloc-instr)
               `(right-rstart ,(second node) ,next)))
   ((eq (first node) 'left-rstart)
    (add-instr (alloc-instr)
               `(left-rstart ,(second node) ,next)))
   ((regend-node-p node)
    (add-instr (alloc-instr) `(rend ,(regend-node-regnum node) ,next)))
   ;; charclass-nodes are long since converted to specialized instr nodes
   ((eq (first node) 'cclass-2)
    (add-instr (alloc-instr)
               `(cclass-2 ,(second node) ,(third node) ,next)))
   ((eq (first node) 'cclass)
    (add-instr (alloc-instr)
               `(cclass ,(second node) ,next)))
   ((eq (first node) 'not-char)
    (add-instr (alloc-instr)
               `(not-char ,(second node) ,next)))
   ((eq (first node) 'not-cclass-2)
    (add-instr (alloc-instr)
               `(not-cclass-2 ,(second node) ,(third node) ,next)))
   ((eq (first node) 'not-cclass)
    (add-instr (alloc-instr) `(not-cclass ,(second node) ,next)))
   ;; specclass-nodes are long since gone, converted to specialized instr nodes
   ((eq (first node) 'specclass)
    (add-instr (alloc-instr) `(specclass ,(second node) ,next)))
   ((eq (first node) 'not-specclass)
    (add-instr (alloc-instr) `(not-specclass ,(second node) ,next)))
   ((any-node-p node)
    (add-instr (alloc-instr) `(any ,next)))
   ((startword-node-p node)
    (add-instr (alloc-instr) `(startword ,next)))
   ((endword-node-p node)
    (add-instr (alloc-instr) `(endword ,next)))
   ((lookahead-node-p node)
    (emit-lookahead (second node) next))
   ((nlookahead-node-p node)
    (emit-nlookahead (second node) next))
   ((hook-node-p node)
    (add-instr (alloc-instr)
               `(hook ,(hook-node-function node) ,next)))
   ((success-node-p node)
    (add-instr (alloc-instr)
               `(success ,(success-node-rc node) ,next)))
   (t ;; once we're done, this should throw the :invalid-parse-tree tag
      (throw 'regex-parse-error
             (list "codegen: Unhandled intermediate node ~S" node)))))

(defun emit-sequence (children next)
  (cond ((null children) next)
        (t (emit-instr (first children)
                       (emit-sequence (rest children) next)))))

(defun emit-lookahead (child-node next)
  (let* ((childsuccess-instr-num (alloc-instr))
         (child-instr-num (emit-instr child-node childsuccess-instr-num)))
    (add-instr childsuccess-instr-num '(success t))
    (add-instr (alloc-instr)
               `(lookahead ,child-instr-num ,next)) ))

(defun emit-nlookahead (child-node next)
  (let* ((childsuccess-instr-num (alloc-instr))
         (child-instr-num (emit-instr child-node childsuccess-instr-num)))
    (add-instr childsuccess-instr-num '(success t))
    (add-instr (alloc-instr)
               `(nlookahead ,child-instr-num ,next)) ))

(defun emit-greedy-kleene-no-termcheck (child-node next)
  (let* ((loop-instr-num (alloc-instr))
         (body-instr-num (emit-instr child-node loop-instr-num)))
    (add-instr loop-instr-num
               `(alt-2-no-termcheck ,body-instr-num ,next))))

(defun emit-greedy-kleene-simple-termcheck (child-node next)
  (let* ((loop-instr-num (alloc-instr))
         (body-instr-num (emit-instr child-node loop-instr-num)))
    (add-instr loop-instr-num
               `(alt-2-simple-termcheck-1 ,body-instr-num ,next))))

(defun emit-greedy-kleene-full-termcheck (child-node next)
  (let* ((loop-instr-num (alloc-instr))
         (body-instr-num (emit-instr child-node loop-instr-num)))
    (add-instr loop-instr-num
               `(alt-2-full-termcheck ,body-instr-num ,next))))

(defun emit-ngkleene-no-termcheck (child-node next)
  (let* ((loop-instr-num (alloc-instr))
         (body-instr-num (emit-instr child-node loop-instr-num)))
    (add-instr loop-instr-num
               `(alt-2-no-termcheck ,next ,body-instr-num))))

(defun emit-ngkleene-simple-termcheck (child-node next)
  (let* ((loop-instr-num (alloc-instr))
         (body-instr-num (emit-instr child-node loop-instr-num)))
    (add-instr loop-instr-num
               `(alt-2-simple-termcheck-2 ,next ,body-instr-num))))

(defun emit-ngkleene-full-termcheck (child-node next)
  (let* ((loop-instr-num (alloc-instr))
         (body-instr-num (emit-instr child-node loop-instr-num)))
    (add-instr loop-instr-num
               `(alt-2-full-termcheck ,next ,body-instr-num))))

(defun emit-alt (child-nodes next)
  (let* ((alt-instr-num (alloc-instr))
         (child-instr-nums (mapcar (lambda (child-node)
                                     (emit-instr child-node next))
                                   child-nodes))
         (num-children (length child-nodes)))
    (cond ((< num-children 2)
           (error "Too few child nodes for alt ~S" child-nodes))
          ((= (length child-nodes) 2)
           (add-instr alt-instr-num
                      `(alt-2-no-termcheck ,@child-instr-nums)))
          (t (add-instr alt-instr-num
                        `(alt-no-termcheck ,child-instr-nums))))))

;; The arms may be replicated many times, so we need to be careful to re-use
;; them when necessary
(defun emit-casealt (child-nodes next)
  (let* ((alt-instr-num (alloc-instr))
         (arm-reuse-cache (make-hash-table :test 'equalp))
         (child-instr-branches
          (mapcar (lambda (arm)
                    (let* ((guard (first arm))
                           (consequent-node (second arm))
                           (prev-lbl (gethash consequent-node arm-reuse-cache)))
                      (if prev-lbl
                          (list guard prev-lbl)
                        (list guard (setf (gethash consequent-node arm-reuse-cache)
                                          (emit-instr consequent-node next))))))
                  child-nodes))
         (num-children (length child-nodes)))
    (cond ((< num-children 2)
           (error "Too few child nodes for alt ~S" child-nodes))
          (t
           (add-instr alt-instr-num
                      `(casealt ,child-instr-branches))))))

