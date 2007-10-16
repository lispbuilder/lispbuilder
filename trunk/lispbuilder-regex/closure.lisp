;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LISPBUILDER-REGEX; Base: 10 -*-

(in-package :LISPBUILDER-REGEX)

;;;
;;; Pass 6 - Code generation to closures
;;;

; returns array of info structures
(defun gen-closures (cpscode)
  (let ((info (make-array (length cpscode))))
    (loop for instr in cpscode
          for offset = (first instr)
          for opcode = (second instr)
          for args = (cddr instr)
          do (setf (aref info offset) (gen-closure opcode args)))
    info))

(defun gen-closure (opcode args)
  (destructuring-bind (&optional arg1 arg2 arg3) args
    (case opcode
      (char
       (gen-char-closure arg1 arg2))
      (string
       (gen-string-closure (length arg1) arg1 arg2))
      (classseq
       (let ((num-classes (length arg1)))
         (gen-classseq-closure
          num-classes
          (make-array num-classes :initial-contents arg1)
          arg2)))
      (backmatch
       (gen-backmatch-closure arg1 arg2))
      (str-greedy-kleene
       (gen-str-greedy-kleene-closure (length arg1) arg1 arg2))
      (char-greedy-kleene
       (gen-char-greedy-kleene-closure arg1 arg2))
      (not-char-greedy-kleene
       (gen-not-char-greedy-kleene-closure arg1 arg2))
      (cclass-2-greedy-kleene
       (gen-cclass-2-greedy-kleene-closure arg1 arg2 arg3))
      (not-cclass-2-greedy-kleene
       (gen-not-cclass-2-greedy-kleene-closure arg1 arg2 arg3))
      (cclass-greedy-kleene
       (gen-cclass-greedy-kleene-closure arg1 arg2))
      (not-cclass-greedy-kleene
       (gen-not-cclass-greedy-kleene-closure arg1 arg2))
      (specclass-greedy-kleene
       (gen-specclass-greedy-kleene-closure
        (get-spec-pat-fxn arg1) arg2))
      (not-specclass-greedy-kleene
       (gen-not-specclass-greedy-kleene-closure
        (get-spec-pat-fxn arg1) arg2))
      (alt-2-simple-termcheck-1
       (gen-alt-2-simple-termcheck-1-closure arg1 arg2))
      (alt-2-simple-termcheck-2
       (gen-alt-2-simple-termcheck-2-closure arg1 arg2))
      (alt-2-full-termcheck
       (gen-alt-2-full-termcheck-closure arg1 arg2))
      (alt-2-no-termcheck
       (gen-alt-2-no-termcheck-closure arg1 arg2))
      (alt-no-termcheck
       (let ((num-branches (length arg1)))
         (gen-alt-no-termcheck-closure
          num-branches
          (make-array num-branches :initial-contents arg1))))
      (casealt
       (multiple-value-bind (numbranches jmptbl)
           (make-casealt-jmptable arg1)
         (gen-casealt-closure numbranches jmptbl)))
      (startanchor
       (gen-startanchor-closure arg1))
      (endanchor
       (gen-endanchor-closure arg1))
      (left-rstart
       (gen-left-rstart-closure arg1 arg2))
      (right-rstart
       (gen-right-rstart-closure arg1 arg2))
      (rend
       (gen-rend-closure arg1 arg2))
      (cclass-2
       (gen-cclass-2-closure arg1 arg2 arg3))
      (cclass
       (gen-cclass-closure arg1 arg2))
      (not-char
       (gen-not-char-closure arg1 arg2))
      (not-cclass-2
       (gen-not-cclass-2-closure arg1 arg2 arg3))
      (not-cclass
       (gen-not-cclass-closure arg1 arg2))
      (specclass
       (gen-specclass-closure (get-spec-pat-fxn arg1) arg2))
      (not-specclass
       (gen-not-specclass-closure (get-spec-pat-fxn arg1) arg2))
      (any
       (gen-any-closure arg1))
      (startword
       (gen-startword-closure arg1))
      (endword
       (gen-endword-closure arg1))
      (lookahead
       (gen-lookahead-closure arg1 arg2))
      (nlookahead
       (gen-nlookahead-closure arg1 arg2))
      (hook
       (gen-hook-closure arg1 arg2))
      (success
       (gen-success-closure arg1))
      (t (error "gen-closure: Unknown instruction ~S ~S" opcode args)))) )


(defun make-casealt-jmptable (jumps)
  (let* ((num-jump-targets (length jumps))
         (jump-tbl (make-array (* 2 num-jump-targets))))
    (loop for discriminator-idx from 0 by 2
          for destination-idx from 1 by 2
          for (discriminator destination) in jumps
          do (progn
               (setf (aref jump-tbl discriminator-idx) discriminator)
               (setf (aref jump-tbl destination-idx) destination))
          finally (return (values num-jump-targets jump-tbl)))))

(defmacro make-text-closure (&key matcher initializer linker)
  `(make-closure-info
    :matchfn
    (if *match-simple-strings-only*
        (macrolet ((re-char (str idx)
                     `(the character (schar (the simple-string ,str)
                                            (the fixnum ,idx)))))
          ,matcher)
      (macrolet ((re-char (str idx)
                   `(the character (char (the string ,str)
                                         (the fixnum ,idx)))))
        ,matcher))
    :initfn
    ,initializer
    :linkfn
    ,linker))

(defmacro make-nontext-closure (&key matcher initializer linker)
  `(make-closure-info
    :matchfn
    ,matcher
    :initfn
    ,initializer
    :linkfn
    ,linker))


(defun gen-char-closure (chr next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
        (declare (fixnum pos)
                 (character chr)
                 (ftype (function (fixnum) t) next)
                 (special *str* *end*)
                 (string *str*) (fixnum *end*))
	(cond ((and (< pos *end*) (char= chr (re-char *str* pos)))
	       (funcall next (1+ pos)))))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next)) )))


(defun gen-string-closure (len patstr next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0) ))
       (declare (fixnum pos)
                (fixnum len) (string patstr)
                (ftype (function (fixnum) t) next)
                (special *str* *end*)
                (string *str*) (fixnum *end*))
       (let ((end (+ pos len)))
         (declare (fixnum end))
         (when (<= end *end*)
           (loop for i fixnum from pos below end
                 for j fixnum from 0 below len
                 when (char/= (re-char *str* i) (re-char patstr j))
                 return nil
                 finally (funcall next i)))))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))


(defun gen-classseq-closure (len chrclasses next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0) ))
       (declare (fixnum pos)
                (fixnum len) (simple-vector chrclasses)
                (ftype (function (fixnum) t) next)
                (special *str* *end*)
                (string *str*) (fixnum *end*))
       (let ((reg-str-end (+ pos len)))
         (declare (fixnum reg-str-end))
         (when (<= reg-str-end *end*)
           (loop for i fixnum from pos below reg-str-end
                 for j fixnum from 0 below len
                 unless (find (re-char *str* i) (svref chrclasses j))
                   return nil
                 finally (funcall next reg-str-end)))))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))


(defun gen-backmatch-closure (regnum next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (fixnum regnum) (ftype (function (fixnum) t) next)
                (special *str* *regs* *end*)
                (string *str*) (type simple-vector *regs*) (fixnum *end*))
       (let ((reg-start (register-start *regs* regnum))
             (reg-end (register-end *regs* regnum)))
         (cond ((numberp reg-start)
                ;; If reg-start is set, but reg-end isn't, then we're matching
                ;; inside that register's context.  So use POS as the end.
                (unless (integerp reg-end)
                  (setq reg-end pos))
                (let* ((reg-len (- reg-end reg-start))
                       (reg-str-end (+ pos reg-len)))
                  (declare (fixnum reg-len reg-str-end))
                  (when (<= reg-str-end *end*)
                    (loop for i fixnum from pos below reg-str-end
                          for j fixnum from reg-start below reg-end
                          when (char/= (re-char *str* i) (re-char *str* j))
                            return nil
                          finally (funcall next i)))))
               ;; backmatching an unmatched register is ok -- it may mean
               ;; that the register was for ()* or ()? or something, so treat
               ;; it as a 0-length register
               (t (funcall next pos)))))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))


(defun gen-str-greedy-kleene-closure (len patstr next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (fixnum len) (string patstr)
                (character chr) (ftype (function (fixnum) t) next)
                (special *str* *end*)
                (string *str*) (fixnum *end*))
       (let ((firstend (+ pos len))
             (lastpos pos)
             (end (- *end* len)))
         (declare (fixnum firstend lastpos end))
         (when (<= firstend *end*)
           (loop for testpos fixnum from pos upto end by len
                 while (loop for j fixnum from 0 below len
                             for i fixnum from testpos
                             when (char/= (re-char *str* i) (re-char patstr j))
                               do (return nil)
                             finally (return t))
                 do (incf lastpos len)))
         (loop for testpos fixnum from lastpos downto pos by len
               do (funcall next testpos))))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next)))))


(defun gen-char-greedy-kleene-closure (chr next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (character chr) (ftype (function (fixnum) t) next)
                (special *str* *end*)
                (string *str*) (fixnum *end*))
       (let ((newpos pos))
         (declare (fixnum newpos))
         (loop while (and (< newpos *end*) (char= chr (re-char *str* newpos)))
               do (incf newpos))
         (loop while (>= newpos pos)
               do (progn
                    (funcall next newpos)
                    (decf newpos)))
         nil))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))


(defun gen-not-char-greedy-kleene-closure (chr next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (ftype (function (fixnum) t) next) (character chr)
                (special *str* *end*)
                (string *str*) (fixnum *end*))
       (let ((newpos pos))
         (declare (fixnum newpos))
         (loop while (and (< newpos *end*)
                          (char/= chr (re-char *str* newpos)))
               do (incf newpos))
         (loop while (>= newpos pos)
               do (progn
                    (funcall next newpos)
                    (decf newpos)))
         nil))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))


(defun gen-cclass-2-greedy-kleene-closure (chr1 chr2 next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (character chr1 chr2)
                (ftype (function (fixnum) t) next)
                (special *str* *end*)
                (fixnum *start* *end*) (string *str*))
       (let ((newpos pos))
         (declare (fixnum newpos))
         (loop while (and (< newpos *end*)
                          (let ((chr (re-char *str* newpos)))
                            (declare (character chr))
                            (or (char= chr1 chr)
                                (char= chr2 chr))))
               do (incf newpos))
         (loop while (>= newpos pos)
               do (progn
                    (funcall next newpos)
                    (decf newpos)))
         nil))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))


(defun gen-not-cclass-2-greedy-kleene-closure (chr1 chr2 next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (character chr1 chr2)
                (ftype (function (fixnum) t) next)
                (special *str* *end*)
                (fixnum *start* *end*) (string *str*))
       (let ((newpos pos))
         (declare (fixnum newpos))
         (loop while (and (< newpos *end*)
                          (not (let ((chr (re-char *str* newpos)))
                                 (declare (character chr))
                                 (or (char= chr1 chr)
                                     (char= chr2 chr)))))
               do (incf newpos))
         (loop while (>= newpos pos)
               do (progn
                    (funcall next newpos)
                    (decf newpos)))
         nil))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))


(defun gen-cclass-greedy-kleene-closure (chrs next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (simple-string chrs)
                (ftype (function (fixnum) t) next)
                (special *str* *end*)
                (fixnum *end*) (string *str*))
       (let ((newpos pos))
         (declare (fixnum newpos))
         (loop while (and (< newpos *end*)
                          (find (re-char *str* newpos) chrs))
               do (incf newpos))
         (loop while (>= newpos pos)
               do (progn
                    (funcall next newpos)
                    (decf newpos)))
         nil))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))


(defun gen-not-cclass-greedy-kleene-closure (chrs next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (simple-string chrs)
                (ftype (function (fixnum) t) next)
                (special *str* *end*)
                (fixnum *end*) (string *str*))
       (let ((newpos pos))
         (declare (fixnum newpos))
         (loop while (and (< newpos *end*)
                          (not (find (re-char *str* newpos) chrs)))
               do (incf newpos))
         (loop while (>= newpos pos)
               do (progn
                    (funcall next newpos)
                    (decf newpos)))
         nil))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))


(defun gen-specclass-greedy-kleene-closure (classfn next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (ftype (function (character) t) classfn)
                (ftype (function (fixnum) t) next)
                (special *str* *end*)
                (string *str*) (fixnum *end*))
       (let ((newpos pos))
         (declare (fixnum newpos))
         (loop while (and (< newpos *end*)
                          (funcall classfn (re-char *str* newpos)))
               do (incf newpos))
         (loop while (>= newpos pos)
               do (progn
                    (funcall next newpos)
                    (decf newpos)))
         nil))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))


(defun gen-not-specclass-greedy-kleene-closure (classfn next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (ftype (function (character) t) classfn)
                (ftype (function (fixnum) t) next)
                (special *str* *end*)
                (string *str*) (fixnum *end*))
       (let ((newpos pos))
         (declare (fixnum newpos))
         (loop while (and (< newpos *end*)
                          (not (funcall classfn (re-char *str* newpos))))
               do (incf newpos))
         (loop while (>= newpos pos)
               do (progn
                    (funcall next newpos)
                    (decf newpos)))
         nil))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))


(defun gen-alt-2-simple-termcheck-1-closure (next1 next2 &aux (oldpos -1))
  (make-nontext-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (ftype (function (fixnum) t) next1 next2)
                (fixnum oldpos)
                (special *str* *end*)
                (string *str*) (fixnum *end*))
       (when (> pos oldpos)
         (setq oldpos pos)
         (funcall next1 pos))
       (funcall next2 pos)
       (setq oldpos -1)
       nil)
   :initializer
   #'(lambda ()
       (setq oldpos -1))
   :linker
   #'(lambda (link-info)
       (setq next1 (resolve-instr link-info next1))
       (setq next2 (resolve-instr link-info next2))) ))


(defun gen-alt-2-simple-termcheck-2-closure (next1 next2 &aux (oldpos -1))
  (make-nontext-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (ftype (function (fixnum) t) next1 next2)
                (fixnum oldpos))
       (funcall next1 pos)
       (when (> pos oldpos)
         (setq oldpos pos)
         (funcall next2 pos))
       (setq oldpos -1)
       nil)
   :initializer
   #'(lambda ()
       (setq oldpos -1))
   :linker
   #'(lambda (link-info)
       (setq next1 (resolve-instr link-info next1))
       (setq next2 (resolve-instr link-info next2))) ))


(defun gen-alt-2-full-termcheck-closure (next1 next2
                                         &aux (firsttimep t)
                                              (seen1 (make-hash-table))
                                              (seen2 (make-hash-table)))
  (make-nontext-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (fixnum firstpos)
                (ftype (function (fixnum) t) next1 next2))
       (cond (firsttimep
              (setq firsttimep nil)
              (unless (gethash pos seen1)
                (setf (gethash pos seen1) t)
                (funcall next1 pos))
              (unless (gethash pos seen2)
                (setf (gethash pos seen2) t)
                (funcall next2 pos))
              (clrhash seen1)
              (clrhash seen2)
              (setq firsttimep t)
              nil)
             (t (unless (gethash pos seen1)
                  (setf (gethash pos seen1) t)
                  (funcall next1 pos))
                (unless (gethash pos seen2)
                  (setf (gethash pos seen2) t)
                  (funcall next2 pos)))))
   :initializer
   #'(lambda ()
       (setq firsttimep t))
   :linker
   #'(lambda (link-info)
       (setq next1 (resolve-instr link-info next1))
       (setq next2 (resolve-instr link-info next2))) ))


(defun gen-alt-2-no-termcheck-closure (next1 next2)
  (make-nontext-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (ftype (function (fixnum) t) next1 next2))
       (funcall next1 pos)
       (funcall next2 pos))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next1 (resolve-instr link-info next1))
       (setq next2 (resolve-instr link-info next2))) ))


(defun gen-alt-no-termcheck-closure (num-nexts nexts)
  (make-nontext-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos num-nexts))
       (dotimes (i num-nexts)
         (funcall (svref nexts i) pos)))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (dotimes (i num-nexts nil)
         (declare (fixnum i))
         (setf (svref nexts i)
               (resolve-instr link-info (svref nexts i))))) ))

(defun gen-casealt-closure (num-jump-entries jmptbl)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (special *str* *end*)
                (string *str*) (fixnum *end*))
       (when (< pos *end*)
         (let ((chr (re-char *str* pos)))
           (declare (character chr))
           (loop for discriminator-idx fixnum from 0 by 2 below (* 2 num-jump-entries)
                 when (char= chr (svref jmptbl discriminator-idx))
                 do (funcall (svref jmptbl (1+ discriminator-idx)) pos)
                 finally (return nil)))))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (loop for destination-idx from 1 by 2 below (* 2 num-jump-entries)
             do (setf (svref jmptbl destination-idx)
                      (resolve-instr link-info (svref jmptbl destination-idx))))) ))

(defun gen-startanchor-closure (next)
  (make-text-closure
   :matcher
    #'(lambda (pos)
        #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                         #+:lispworks (hcl:fixnum-safety 0)))
        (declare (fixnum pos)
                 (ftype (function (fixnum) t) next)
                 (special *str* *start* *start-is-anchor*)
                 (string *str*) (fixnum *start*))
	(if (or (and *start-is-anchor* (= pos *start*))
                (and (> pos 0) (char= (re-char *str* (1- pos))
                                      #\newline)))
	    (funcall next pos)))
   :initializer
   nil
   :linker
    #'(lambda (link-info)
	(setq next (resolve-instr link-info next))) ))


(defun gen-endanchor-closure (next)
  (make-text-closure
   :matcher
    #'(lambda (pos)
        #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                         #+:lispworks (hcl:fixnum-safety 0)))
        (declare (fixnum pos)
                 (ftype (function (fixnum) t) next)
                 (special *str* *end* *end-is-anchor*)
                 (fixnum *end*) (string *str*))
	(if (or (and *end-is-anchor* (= pos *end*))
                (and (< pos *end*) (char= (re-char *str* pos)
                                          #\newline)))
	    (funcall next pos)))
    :initializer
    nil
    :linker
    #'(lambda (link-info)
	(setq next (resolve-instr link-info next))) ))


;(defun gen-left-rstart-closure (regnum next)
;  (make-nontext-closure
;   :matcher
;   #'(lambda (pos)
;       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
;                                        #+:lispworks (hcl:fixnum-safety 0)))
;       (declare (fixnum pos)
;                (fixnum regnum)
;                (ftype (function (fixnum) t) next)
;                (special *regs*)
;                (type simple-vector *regs*))
;       (let ((reg (svref *regs* regnum)))
;         (declare (cons reg))
;         (setf (car reg) pos (cdr reg) nil)
;         (funcall next pos)
;         (setf (car reg) nil)
;         nil))
;   :initializer
;   nil
;   :linker
;   #'(lambda (link-info)
;       (setq next (resolve-instr link-info next))) ))




(defun gen-left-rstart-closure (regnum next)
  (make-nontext-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (fixnum regnum)
                (ftype (function (fixnum) t) next)
                (special *regs*)
                (type simple-vector *regs*))
       (let ((reg (svref *regs* regnum)))
         (declare (cons reg))
         (cond ((and (car reg) (cdr reg))
                (funcall next pos))
               (t
                (setf (car reg) pos (cdr reg) nil)
                (funcall next pos)
                (setf (car reg) nil)
                nil))))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))



(defun gen-right-rstart-closure (regnum next)
  (make-nontext-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (fixnum regnum)
                (ftype (function (fixnum) t) next)
                (special *regs*)
                (type simple-vector *regs*))
       (let ((reg (svref *regs* regnum)))
         (declare (cons reg))
         (let ((prevstart (car reg))
               (prevend (cdr reg)))
           (setf (car reg) pos (cdr reg) nil)
           (funcall next pos)
           (setf (car reg) prevstart (cdr reg) prevend)))
         nil)
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))


(defun gen-rend-closure (regnum next)
  (make-nontext-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (fixnum regnum)
                (ftype (function (fixnum) t) next)
                (special *regs*)
                (type simple-vector *regs*))
       (cond ((register-end *regs* regnum)
              (funcall next pos))
             (t
              (setf (register-end *regs* regnum) pos)
              (funcall next pos)
              (setf (register-end *regs* regnum) nil))))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))


(defun gen-cclass-2-closure (chr1 chr2 next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (character chr1 chr2)
                (ftype (function (fixnum) t) next-fn)
                (special *str* *end*)
                (string *str*) (fixnum *end*))
       (if (< pos *end*)
           (let ((chr (re-char *str* pos)))
             (declare (character chr))
             (if (or (char= chr chr1) (char= chr chr2))
                 (funcall next (1+ pos))))))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))


(defun gen-cclass-closure (chrs next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (simple-string chrs)
                (ftype (function (fixnum) t) next)
                (special *str* *end*)
                (string *str*) (fixnum *end*))
       (if (and (< pos *end*)
                (find (re-char *str* pos) chrs))
           (funcall next (1+ pos))))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))


(defun gen-not-char-closure (chr next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (character chr)
                (ftype (function (fixnum) t) next)
                (special *str* *end*)
                (string *str*) (fixnum *end*))
       (if (and (< pos *end*)
                (char/= (re-char *str* pos) chr))
           (funcall next (1+ pos))))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))


(defun gen-not-cclass-2-closure (chr1 chr2 next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (character chr1 chr2)
                (ftype (function (fixnum) t) next)
                (special *str* *end*)
                (string *str*) (fixnum *end*))
       (if (< pos *end*)
           (let ((chr (re-char *str* pos)))
             (declare (character chr))
             (if (not (or (char= chr chr1) (char= chr chr2)))
                 (funcall next (1+ pos))))))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))


(defun gen-not-cclass-closure (chrs next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (simple-string chrs)
                (ftype (function (fixnum) t) next)
                (special *str* *end*)
                (string *str*) (fixnum *end*))
       (if (and (< pos *end*)
                (not (find (re-char *str* pos) chrs)))
           (funcall next (1+ pos))))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))


(defun gen-specclass-closure (classfn next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (ftype (function (character) t) classfn)
                (ftype (function (fixnum) t) next)
                (special *str* *end*)
                (string *str*) (fixnum *end*))
       (if (and (< pos *end*)
                (funcall classfn (re-char *str* pos)))
           (funcall next (1+ pos))))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))

  
(defun gen-not-specclass-closure (classfn next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (ftype (function (character) t) classfn)
                (ftype (function (fixnum) t) next)
                (special *str* *end*)
                (string *str*) (fixnum *end*))
       (if (and (< pos *end*)
                (not (funcall classfn (re-char *str* pos))))
           (funcall next (1+ pos))))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))


(defun gen-any-closure (next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (ftype (function (fixnum) t) next)
                (special *str* *end*)
                (fixnum pos *start* *end*)
                (string *str*)
                (type simple-vector *regs*))
       (if (and (< pos *end*)
                (or *dot-matches-newline*
                    (char/= (re-char *str* pos) #\newline)))
           (funcall next (1+ pos))))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))


(defun gen-startword-closure (next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (ftype (function (fixnum) t) next)
                (special *str* *end* *start*)
                (fixnum pos *start* *end* *start*)
                (string *str*)
                (type simple-vector *regs*))
       ;; at start-of-word if previous char is nonword (or
       ;; start-of-str) and this char is word
       (if (and (or (= pos *start*)
                    (and (> pos *start*) (not (wordcharp (re-char *str* (1- pos))))))
                (< pos *end*)
                (wordcharp (re-char *str* pos)))
           (funcall next pos)))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))
       

(defun gen-endword-closure (next)
  (make-text-closure
   :matcher
   #'(lambda (pos)
       #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        #+:lispworks (hcl:fixnum-safety 0)))
       (declare (fixnum pos)
                (ftype (function (fixnum) t) next)
                (special *str* *end* *start*)
                (fixnum pos *start* *end* *start*)
                (string *str*)
                (type simple-vector *regs*))
       ;; at end-of-word if previous char is word and this char is
       ;; nonword or end-of-string
       (if (and (> pos *start*)
                (wordcharp (re-char *str* (1- pos)))
                (or (= pos *end*)
                    (and (< pos *end*) (not (wordcharp (re-char *str* pos))))))
           (funcall next pos)))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))



(defun gen-hook-closure (hookfn next)
  (make-nontext-closure
   :matcher
   #'(lambda (pos)
       (declare (fixnum pos)
                (ftype (function (fixnum) t) next)
                (special *end* *hooks*)
                (fixnum *end*))
       (let ((rc (cond ((integerp hookfn)
                        (funcall (aref *hooks* hookfn) pos))
                       ((or (functionp hookfn) (symbolp hookfn))
                        (funcall hookfn pos))
                       (t nil))))
         (cond
          ;; user hook returned an integer --> new matching position
          ((integerp rc)
           (funcall next rc))
          ;; user hook returned t --> continue matching at pos
          (rc (funcall next pos))
          ;; user hook returned nil --> fail
          (t nil))))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq next (resolve-instr link-info next))) ))

(defun gen-lookahead-closure (childfn next)
  (make-nontext-closure
   :matcher
   #'(lambda (pos)
       (declare (fixnum pos)
                (ftype (function (fixnum) t) next)
                (special *end* *hooks*)
                (fixnum *end*))
       (let ((*acceptfn* nil))
         (declare (special *acceptfn*))
         (if (catch 'cease-matching (funcall childfn pos))
             (funcall next pos)
           nil)))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq childfn (resolve-instr link-info childfn))
       (setq next (resolve-instr link-info next))) ))
       
(defun gen-nlookahead-closure (childfn next)
  (make-nontext-closure
   :matcher
   #'(lambda (pos)
       (declare (fixnum pos)
                (ftype (function (fixnum) t) next)
                (special *end* *hooks*)
                (fixnum *end*))
       (let ((*acceptfn* nil))
         (declare (special *acceptfn*))
         (if (not (catch 'cease-matching (funcall childfn pos)))
             (funcall next pos)
           nil)))
   :initializer
   nil
   :linker
   #'(lambda (link-info)
       (setq childfn (resolve-instr link-info childfn))
       (setq next (resolve-instr link-info next))) ))

(defun gen-success-closure (rc)
  (make-nontext-closure
   :matcher
    #'(lambda (pos)
        #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                         #+:lispworks (hcl:fixnum-safety 0)))
        (declare (ignore pos)
                 (special *regs* *acceptfn*)
                 (type simple-vector *regs*))
	(let* ((match-start (register-start *regs* 0))
	       (match-end (register-end *regs* 0))
               (valid-acceptfn-p (or (functionp *acceptfn*)
                                     (and (symbolp *acceptfn*)
                                          (fboundp *acceptfn*)
                                          (functionp (symbol-function *acceptfn*)))))
               (really-succeeded-p (if valid-acceptfn-p
                                       (funcall *acceptfn* match-start match-end)
                                     t)))
          (declare (fixnum match-start match-end))
          (when really-succeeded-p
            (throw 'cease-matching
                   (values rc
                           match-start
                           (- match-end match-start)
                           *regs*)))))
   :initializer
   nil
   :linker
   nil ))


(defun make-init-closure (reset-fns next)
  #'(lambda (pos)
      #-:debug-regex(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                       #+:lispworks (hcl:fixnum-safety 0)))
      (dolist (reset-fn reset-fns)
        (funcall reset-fn))
      (funcall next pos)) )
  




;;;
;;; Special pattern support
;;;

(defun get-spec-pat-fxn (patclass)
  "Return the function to test for the special pattern?"
  (case patclass
    (alpha #'alpha-char-p)
    (upper #'upper-case-p)
    (lower #'lower-case-p)
    (digit #'digit-char-p)
    (alnum #'alphanumericp)
    (xdigit #'xdigitp)
    (odigit #'odigitp)
    (punct #'punctp)
    (space #'spacep)
    (t (error "Couldn't find special class ~S" patclass))))


(defun xdigitp (ch)
  "Is this character a hexidecimal digit?"
  (or (digit-char-p ch)
      (find (char-upcase ch) "ABCDEF")))

(defun odigitp (ch)
  "Is this character an octal digit?"
  (find ch "01234567"))

(defun punctp (ch)
  "Is this character a punctuation mark?"
  (find ch "!.,;:'\"?`")
  )

(defun spacep (ch)
  "Is this character some type of whitespace?"
  (or (char= ch #\tab) (char= ch #\Space) (char= ch #\newline) (char= ch #\return)))

(defun wordcharp (ch)
  (or (alphanumericp ch) (char= ch #\_)))
