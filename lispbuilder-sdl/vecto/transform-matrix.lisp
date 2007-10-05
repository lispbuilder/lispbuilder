;;; Copyright (c) 2007 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;; $Id: transform-matrix.lisp,v 1.6 2007/09/28 20:35:08 xach Exp $

(in-package #:vecto)

(defstruct (transform-matrix (:type vector))
 (x-scale 1.0)
 (y-skew 0.0)
 (x-skew 0.0)
 (y-scale 1.0)
 (x-offset 0.0)
 (y-offset 0.0))

(defmacro matrix-bind (lambda-list vector &body body)
  (when (/= (length lambda-list) 6)
    (error "Bad lambda-list for MATRIX-BIND: 6 arguments required"))
  (let ((vec (gensym)))
  `(let ((,vec ,vector))
     (let (,@(loop for i from 0 below 6
                  for var in lambda-list
                  collect (list var `(aref ,vec ,i))))
       ,@body))))

(defun matrix (a b c d e f)
  (vector a b c d e f))

(defun make-transform-function (transform-matrix)
  (matrix-bind (a b c d e f)
      transform-matrix
    (lambda (x y)
      (values (+ (* a x) (* c y) e)
              (+ (* b x) (* d y) f)))))

(defun transform-coordinates (x y transform-matrix)
  (matrix-bind (a b c d e f)
      transform-matrix
    (values (+ (* a x) (* c y) e)
            (+ (* b x) (* d y) f))))


;;; Multiplication:
;;;
;;;   a b 0     a*b*0
;;;   c d 0  x  c*d*0
;;;   e f 1     e*f*1

(defun mult (m1 m2)
  (matrix-bind (a b c d e f)
      m1
    (matrix-bind (a* b* c* d* e* f*)
        m2
      (matrix (+ (* a a*)
                 (* b c*))
              (+ (* a b*)
                 (* b d*))
              (+ (* c a*)
                 (* d c*))
              (+ (* c b*)
                 (* d d*))
              (+ (* e a*)
                 (* f c*)
                 e*)
              (+ (* e b*)
                 (* f d*)
                 f*)))))

(defun nmult (m1 m2)
  "Destructive MULT; M2 is modified to hold the result of multiplication."
  (matrix-bind (a b c d e f)
      m1
    (matrix-bind (a* b* c* d* e* f*)
        m2
      (setf (aref m2 0)
            (+ (* a a*)
               (* b c*))
            (aref m2 1)
            (+ (* a b*)
               (* b d*))
            (aref m2 2)
            (+ (* c a*)
               (* d c*))
            (aref m2 3)
            (+ (* c b*)
               (* d d*))
            (aref m2 4)
            (+ (* e a*)
               (* f c*)
               e*)
            (aref m2 5)
            (+ (* e b*)
               (* f d*)
               f*))
      m2)))

(defun translation-matrix (tx ty)
  (matrix 1 0 0 1 tx ty))

(defun scaling-matrix (sx sy)
  (matrix sx 0 0 sy 0 0))

(defun rotation-matrix (theta)
  (let ((cos (cos theta))
        (sin (sin theta)))
    (matrix cos sin (- sin) cos 0 0)))

(defun skewing-matrix (alpha beta)
  (matrix 1 (tan alpha) (tan beta) 1 0 0))

(defun identity-matrix ()
  (matrix 1.0 0.0 0.0 1.0 0.0 0.0))
