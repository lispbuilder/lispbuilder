;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLAWK-TEST; Base: 10 -*-

(in-package :LISPBUILDER-CLAWK-TEST)


;;;
;;; Various tests, based on translations of AWK examples from the AWK
;;; book
;;;

; Chapter 1, page 1, example 1
; Print all employees that have worked this week
;
; $3 > 0 { print $1 $2 $3 }
;
(defun test-1-1-1 (&optional (filename "emp.data"))
  (for-file-lines (filename)
    (with-fields ((name payrate hrsworked))
      (when ($> hrsworked 0)
	($print name payrate hrsworked)))))

; alternatively like this
(defun test-1-1-2 (&optional (filename "emp.data"))
  (for-file-fields (filename (name payrate hrsworked))
    (when ($> hrsworked 0)
      ($print name payrate hrsworked)))) 

; or this
(defun test-1-1-3 (&optional (filename "emp.data"))
  (for-file-fields (filename)
    (when ($> $3 0) ($print $1 $2 $3)))) 

; or even like this
(defawk test-1-1-4 ()
  "Print out the employees that have worked this week."
  (($> $3 0) ($print $1 $2 $3)))


(defun test-1-2 (&optional (filename "emp.data"))
  (for-file-lines (filename)
    (with-fields ((name payrate hrsworked))
      (when ($zerop hrsworked)
        ($print name payrate hrsworked)))))

;;; also awk-like
(defawk test-1-6 ()
  (t ($print *NR* $0)))

(defun test-1-7 (&optional (filename "emp.data"))
  (for-file-lines (filename)
    (with-fields ((name payrate hrsworked))
      ($print "total pay for" name "is" ($* payrate hrsworked)))))

(defun test-1-9 (&optional (filename "emp.data"))
  (for-file-lines (filename inf line)
    (with-fields ((name payrate hrsworked))
      (declare (ignore name hrsworked))
      (when ($>= payrate 5)
        ($print line)))))

; as close to the original awk as possible
(defawk test-1-11-1 ()
  ((/= *NF* 3) ($print $0 "number of fields is not equal to 3"))
  (($< $2 3.35) ($print $0 "rate is below minimum wage"))
  (($> $2 10) ($print $0 "rate exceeds $10 per hour"))
  (($< $3 0) ($print $0 "negative hours worked"))
  (($> $3 60) ($print $0 "too many hours worked")) )

; as close to the original awk as possible
(defawk test-1-11-2 ()
  (BEGIN ($print "NAME	RATE	HOURS")
         ($print))
  (t ($print $0)) )                     ; empty condition 

(defun test-1-12-1 (&optional (filename "emp.data") &aux (emp 0))
  (for-file-lines (filename)
    (with-fields ((name payrate hrsworked))
      (declare (ignore name payrate))
      (when ($> hrsworked 15)
        (incf emp))))
  ($print emp "employees worked more than 15 hours"))

(defawk test-1-12-3 (&aux (pay 0))
  (t (incf pay ($* $2 $3)))
  (END ($print *NR* "employees")
       ($print "total pay is" pay)
       ($print "average pay is" (/ pay *NR*))))

(defun sample (filename &aux (pay 0) (nr 0))
  (for-file-fields (filename (name payrate hrsworked))
    (declare (ignore name))
    (incf pay (* (num payrate) (num hrsworked)))
    (incf nr) )
  (format t "~%~D employees" nr)
  (format t "~%total pay is ~F" pay)
  (format t "~%average pay is ~F" (/ pay nr)) )

(defun test-1-12-4 (&optional (filename "emp.data") &aux maxrate maxemp)
  (for-file-lines (filename)
    (with-fields ((name payrate hrsworked))
      (declare (ignore hrsworked))
      (when ($> payrate maxrate)
        (setq maxrate payrate maxemp name)))) 
  ($print "highest hourly rate:" maxrate "for" maxemp))

(defun test-1-13-1 (&optional (filename "emp.data") &aux names)
  (for-file-lines (filename)
    (with-fields ((name payrate hrsworked))
      (declare (ignore payrate hrsworked))
      (setq names ($++ names name " "))))
  ($print names))

(defun test-1-13-2 (&optional (filename "emp.data") &aux last)
  "Print last line"
  (for-file-lines (filename inf line)
    (setq last line))
  ($print last))

(defun test-1-14-1 (&optional (filename "emp.data"))
  (for-file-lines (filename)
    (with-fields ((name payrate hrsworked))
      (declare (ignore payrate hrsworked))
      ($print name ($length name)))))

(defun test-1-14-2 (filename &aux (nc 0) (nw 0))
  "Count lines, words, and characters"
  (setq *NR* 0)					; the low-level macros don't do this for you
  (for-file-lines (filename inf line)
    (with-fields ((&rest rest))
      (declare (ignore rest))
      (incf nc (1+ (length line)))
      (incf nw *NF*)))
  ($print *NR* "lines," nw "words," nc "characters"))

(defun test-1-14-3 (&optional (filename "emp.data") &aux (n 0) (pay 0))
  (for-file-lines (filename)
    (with-fields ((name payrate hrsworked))
      (declare (ignore name))
      (when ($> payrate 6)
        (incf n)
        (incf pay ($* payrate hrsworked)))))
  (if (> n 0)
      ($print n "employees, total pay is" pay "average pay is" (/ pay n))
      ($print "no employees are paid more than $6/hour")))

; doesn't use emp.data
(defun test-1-15 (filename)
  "Input lines: amount rate years"
  (for-file-lines (filename inf line)
    (with-fields ((&optional amount rate years))
      ($print line)
      (loop for i from 1 by 1
	    while ($<= i years)
	    do (format t "~%~t~.2F" ($* amount ($expt ($+ 1 rate) i)))))))

(defun test-1-16-2-1 (filename &aux lines)
  "Print lines in reverse order"
  (for-file-lines (filename inf line)
    (push line lines))
  (loop for line in lines
        do ($print line)))

;; even more like the original awk, but not as quick
(defawk test-1-16-2-2 (&aux (lines ($array)))
  "Print lines in reverse order"
  (t (setf ($aref lines *NR*) $0))
  (END (loop for i from *NR* above 0
             do ($print ($aref lines i)))))

(defun test-1-17-3-1 (filename)
  "Print the last field of every input line"
  (for-file-lines (filename)
    (with-fields ((&rest fields))
      ($print (elt fields (1- *NF*))))))

; even more like awk
(defawk test-1-17-3-2 ()
  (t ($print ($n *NF*))))
