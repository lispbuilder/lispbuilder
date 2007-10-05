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
;;; $Id: user-shortcuts.lisp,v 1.6 2007/09/21 01:39:07 xach Exp $

(in-package #:vecto)

(defconstant +kappa+ (* 4.d0 (/ (- (sqrt 2.0d0) 1.0d0) 3.0d0))
  "From http://www.whizkidtech.redprince.net/bezier/circle/, the top
Google hit for my vague recollection of this constant.")

(defun centered-ellipse-path (x y rx ry)
  "Add an elliptical subpath centered at X,Y with x radius RX and
y radius RY."
  (let ((cx (* rx +kappa+))
        (cy (* ry +kappa+)))
    ;; upper left
    (move-to (- x rx) y)
    (curve-to (- x rx) (+ y cy)
              (- x cx) (+ y ry)
              x (+ y ry))
    ;; upper right
    (curve-to (+ x cx) (+ y ry)
              (+ x rx) (+ y cy)
              (+ x rx) y)
    ;; lower right
    (curve-to (+ x rx) (- y cy)
              (+ x cx) (- y ry)
              x (- y ry))
    (curve-to (- x cx) (- y ry)
              (- x rx) (- y cy)
              (- x rx) y)
    (close-subpath)))

(defun centered-circle-path (x y radius)
  "Add a circular subpath centered at X,Y with radius RADIUS."
  (centered-ellipse-path x y radius radius))

(defun rectangle (x y width height)
  (move-to x y)
  (line-to (+ x width) y)
  (line-to (+ x width) (+ y height))
  (line-to x (+ y height))
  (close-subpath))

(defun rounded-rectangle (x y width height rx ry)
  ;; FIXME: This should go counter-clockwise, like RECTANGLE!
  (let* ((x3 (+ x width))
         (x2 (- x3 rx))
         (x1 (+ x rx))
         (x0 x)
         (xkappa (* rx +kappa+))
         (y3 (+ y height))
         (y2 (- y3 ry))
         (y1 (+ y ry))
         (y0 y)
         (ykappa (* ry +kappa+)))
    ;; west
    (move-to x0 y1)
    (line-to x0 y2)
    ;; northwest
    (curve-to x0 (+ y2 ykappa)
              (- x1 xkappa) y3
              x1 y3)
    ;; north
    (line-to x2 y3)
    ;; northeast
    (curve-to (+ x2 xkappa) y3
              x3 (+ y2 ykappa)
              x3 y2)
    ;; east
    (line-to x3 y1)
    ;; southeast
    (curve-to x3 (- y1 ykappa)
              (+ x2 xkappa) y0
              x2 y0)
    ;; south
    (line-to x1 y0)
    ;; southwest
    (curve-to (- x1 xkappa) y0
              x0 (+ y0 ykappa)
              x0 y1)
    ;; fin
    (close-subpath)))
