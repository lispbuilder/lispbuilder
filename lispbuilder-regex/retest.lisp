;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LISPBUILDER-REGEX-TEST; Base: 10 -*-

(in-package :LISPBUILDER-REGEX-TEST)


;;;
;;; testing...
;;;

(defun test (patstr candstr shouldmatchp &optional (verbosep nil))
  (when verbosep
    (format t "~%Testing pattern ~S against string ~S" patstr candstr)
    (format t "~%Compiling..."))
  (let ((matcher (compile-str patstr)))
    (unless matcher
      (format t "~%Error compiling pattern")
      (return-from test nil))
    (multiple-value-bind (matchedp start len regs)
        (match-str matcher candstr)
      (format t "~%matched=~A start=~A len=~A regs=~A" matchedp start len regs)
      (when verbosep
        (cond ((and shouldmatchp (not matchedp))
               (format t "~%***** Error: Should have matched, but didn't *****"))
              ((and (not shouldmatchp) matchedp)
               (format t "~%***** Error: Shouldn't have matched, but did *****"))
              (matchedp
               (format t "~%Success: Matched"))
              (t (format t "~%Success: Didn't match"))))
      (when matchedp
        (dotimes (i (array-dimension regs 0))
          (let ((start (register-start regs i))
                (end (register-end regs i)))
            (format t "~%REG ~D start=~D end=~D" i start end)
            (when (register-matched-p regs i)
              (format t " substr = \"")
              (loop for j from start below end
                    do (princ (char candstr j)))
              (format t "\""))))))))

(defun coveragetest ()
  (test "AB" "AB" t t)
  (test "A*" "" t t)
  (test "A*" "A" t t)
  (test "A*" "AA" t t)
  (test "A+" "" nil t)
  (test "A+" "A" t t)
  (test "A+" "AA" t t)
  
  ;; test '.' and '?'
  (test ".BC" "ABC" t t)
  (test ".BC" "BC" nil t)
  (test "A?BC" "ABC" t t)
  (test "A?BC" "BC" t t)
  
  ;; test alternation
  (test "A|B" "A" t t)
  (test "(A)|(B)" "B" t t)
  
  ;; more complicated test
  (test "((A*B)|(AC))D" "BD" t t)
  (test "((A*B)|(AC))D" "ABD" t t)
  (test "((A*B)|(AC))D" "AABD" t t)
  (test "((A*B)|(AC))D" "AAABD" t t)
  (test "((A*B)|(AC))D" "AAABC" nil t)
  (test "((A*B)|(AC))D" "ACD" t t)
  (test "(ABC)*DEF" "ABCABCABCDEF" t t)
  
  ;; test character patterns and anchors
  (test "[a-z][0-9][z-a][9-0]" "a0a0" t t)
  (test "[a-z][0-9][z-a][9-0]" "A0A0" nil t)
  (test "[^a-z][0-9]" "A0" t t)
  (test "[^a-z][0-9]" "a0" nil t)
  (test "^[abcdefg]*$" "abcdefg" t t)
  (test "^[abcdefg]*$" "abcdefgh" nil t)
  (test "^[abcdefg]*$" "ABCDEFG" nil t)
  
  ;; test special character patterns
  (test "[:lower:][:digit:][:upper:][:xdigit:]" "a0A0" t t)
  (test "[:lower:][:digit:][:upper:][:xdigit:]" "a0Aa" t t)
  (test "[:lower:][:digit:][:upper:][:xdigit:]" "a0AA" t t)
  (test "[:lower:][:digit:][:upper:][:xdigit:]" "a0Af" t t)
  (test "[:lower:][:digit:][:upper:][:xdigit:]" "a0AF" t t)
  
  ;; test compiler errors
  (format t "~%~%All of the following should generate compiler errors!")
  (test "(abc" "(abc" nil t)
  (test "(abc" "abc" nil t)
  (test "abc)def" "abc)def" nil t)
  (test "abc)def" "abc" nil t)
  (test "[abc" "[abc" nil t)
  (test "[abc" "abc" nil t)
;; Unlike the C++ parser, this one treats unattached ] as a normal character 
;;  (test "abc]def" "abc]def" nil t)
;;  (test "abc]def" "abc" nil t)
  (test "[:digit]*" "012345" nil t)
  )

(defun respeedtest-closure (numreps patstr candstr)
  (let* ((matcher (compile-str patstr))
	 (regs (make-regs (matcher-numregs matcher)))
	 (matchedp nil))
    (when (null matcher)
      (format t "Error compiling pattern ~A" patstr)
      (return-from respeedtest-closure nil))
    (format t "~%~%Timing ~S (closure)" patstr)
    (let ((starttime (get-internal-run-time)))
      (dotimes (rep numreps)
	(setq matchedp (match-str matcher candstr :regs regs)))
      (let* ((endtime (get-internal-run-time))
             (elapsed (- endtime starttime)))
        (format t "~%~T: ~D secs, ~D/sec, ~S --> ~S~%"
                (round (/ elapsed internal-time-units-per-second))
                (round (/ numreps (/ elapsed internal-time-units-per-second)))
                patstr
                candstr)))
    (when (not matchedp)
      (format t "~%Didn't match"))))


(defun respeedtest-comp (numreps patstr matcher candstr)
  (let* ((regs (make-regs (matcher-numregs matcher)))
	 (matchedp nil))
    (when (null matcher)
      (format t "Error compiling pattern ~A" patstr)
      (return-from respeedtest-comp nil))
    (format t "~%~%Timing ~S (compiled)" patstr)
    (let ((starttime (get-internal-run-time)))
      (dotimes (rep numreps)
	(setq matchedp (match-str matcher candstr :regs regs)))
      (let* ((endtime (get-internal-run-time))
             (elapsed (- endtime starttime)))
        (format t "~%~T: ~D secs, ~D/sec, ~S --> ~S~%"
                (round (/ elapsed internal-time-units-per-second))
                (round (/ numreps (/ elapsed internal-time-units-per-second)))
                patstr
                candstr)))
    (when (not matchedp)
      (format t "~%Didn't match"))))


(defun strcmpspeedtest (numreps patstr candstr compname compfxn)
  (format t "~%~%Timing ~S ~S" compname patstr)
  (let ((matchedp nil)
        (starttime (get-internal-run-time)))
    (dotimes (rep numreps)
      (setq matchedp (funcall compfxn patstr candstr)))
    (let* ((endtime (get-internal-run-time))
           (elapsed (- endtime starttime)))
      (format t "~%~T: ~D secs, ~D/sec, ~S --> ~S~%"
              (round (/ elapsed internal-time-units-per-second))
              (round (/ numreps (/ elapsed internal-time-units-per-second)))
              patstr candstr))
    (when (not matchedp)
      (format t "~%Didn't match"))))


;;;
;;; Speeds are under Win NT, P3-600mhz.  GNU speeds are w/ MSVC6.0.
;;;
(defregex test1 "A*BD")
(defregex test2 "(?A|A)*BD")
(defregex test3 "(A|A)*BD")
(defregex test4 "(A|B)*BD")
(defregex test5 "(B|A)*BD")
(defregex test6 "((A*B)|(AC))D")
(defregex test7 "((A*B)|(A*C))D")
(defregex test8 "[Aa]*[Bb][Dd]")

(defun speedtest ()
  (let ((numreps #-:Genera 1000000
                 #+:Genera 250000)
        (candstr "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABD"))
    (lispbuilder-regex::clear-pattern-cache)

    ;; CLAWK: 9 secs; MSVC/GNU: 53 secs
    (respeedtest-closure numreps "A*BD" candstr)

    ;; CLAWK: 10 secs; MSVC/GNU: No known equivalent
    (respeedtest-closure numreps "(?A|A)*BD" candstr)

    ;; CLAWK: 10 secs; MSVC/GNU: 171 secs
    (respeedtest-closure numreps "(A|A)*BD" candstr)

    ;; CLAWK: 34 secs; MSVC/GNU: 176 secs
    (respeedtest-closure numreps "(A|B)*BD" candstr)

    ;; CLAWK: 55 secs; MSVC/GNU: 178 secs
    (respeedtest-closure numreps "(B|A)*BD" candstr)

    ;; CLAWK: 10 secs; MSVC/GNU: 71 secs
    (respeedtest-closure numreps "((A*B)|(AC))D" candstr)

    ;; CLAWK: 11 secs; MSVC/GNU: 72 secs
    (respeedtest-closure numreps "((A*B)|(A*C))D" candstr)

    ;; CLAWK: 9 secs; MSVC/GNU: 63 secs
    (respeedtest-closure numreps "[Aa]*[Bb][Dd]" candstr)

    ;; LWW: 27 secs; MSVC/MSVC: 1 secs
    (strcmpspeedtest numreps candstr candstr "string=" #'string=)

    ;; LWW: 65 secs; MSVC/MSVC: 2 secs
    (strcmpspeedtest numreps candstr candstr "string-equal" #'string-equal)
  ))

(defun run-tests ()
  (format t "~%Starting coverage test~%")
  (coveragetest)
  (format t "~%Starting Sebastien's coverage test~%")
  (run-sebastien-tests)
  (format t "~%Starting speed test~%")
  (speedtest)
  (format t "~%Done~%")
)

