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
;;; $Id: text.lisp,v 1.8 2007/09/21 17:39:36 xach Exp $

(in-package #:vecto)

(defclass font ()
  ((loader
    :initarg :loader
    :accessor loader)
   (transform-matrix
    :initarg :transform-matrix
    :accessor transform-matrix)
   (size
    :initarg :size
    :accessor size)))

(defun glyph-path-point (point)
  (paths:make-point (zpb-ttf:x point)
                    (zpb-ttf:y point)))

(defun glyph-paths (glyph)
  (let* ((paths '())
         (path nil))
    (zpb-ttf:do-contours (contour glyph (nreverse paths))
      (when (plusp (length contour))
        (let ((first-point (aref contour 0)))
          (setf path (paths:create-path :polygon))
          (push path paths)
          (paths:path-reset path (glyph-path-point first-point))
          (zpb-ttf:do-contour-segments* (control end)
              contour
            (if control
                (paths:path-extend path (paths:make-bezier-curve
                                         (list (glyph-path-point control)))
                                   (glyph-path-point end))
                (paths:path-extend path (paths:make-straight-line)
                                   (glyph-path-point end)))))))))

(defun string-glyphs (string loader)
  "Return STRING converted to a list of ZPB-TTF glyph objects from FONT."
  (map 'list (lambda (char) (zpb-ttf:find-glyph char loader)) string))

(defun string-paths (x y string font)
  "Return the paths of STRING, transformed by the font scale of FONT."
  (let ((glyphs (string-glyphs string (loader font)))
        (loader (loader font))
        (matrix (mult (transform-matrix font) (translation-matrix x y)))
        (paths '()))
    (loop for (glyph . rest) on glyphs do
          (let ((glyph-paths (glyph-paths glyph))
                (fun (make-transform-function matrix)))
            (dolist (path glyph-paths)
              (push (transform-path path fun) paths))
            (when rest
              (let* ((next (first rest))
                     (offset (+ (zpb-ttf:advance-width glyph)
                                (zpb-ttf:kerning-offset glyph next loader))))
                (setf matrix (nmult (translation-matrix offset 0)
                                    matrix))))))
    paths))

(defun nmerge-bounding-boxes (b1 b2)
  "Create a minimal bounding box that covers both B1 and B2 and
destructively update B1 with its values. Returns the new box."
  (setf (xmin b1) (min (xmin b1) (xmin b2))
        (ymin b1) (min (ymin b1) (ymin b2))
        (xmax b1) (max (xmax b1) (xmax b2))
        (ymax b1) (max (ymax b1) (ymax b2)))
  b1)

(defun advance-bounding-box (bbox offset)
  "Return a bounding box advanced OFFSET units horizontally."
  (vector (+ (xmin bbox) offset)
          (ymin bbox)
          (+ (xmax bbox) offset)
          (ymax bbox)))

(defun empty-bounding-box ()
  (vector most-positive-fixnum most-positive-fixnum
          most-negative-fixnum most-negative-fixnum))

(defun ntransform-bounding-box (bbox fun)
  "Return BBOX transformed by FUN; destructively modifies BBOX
with the new values."
  (setf (values (xmin bbox) (ymin bbox))
        (funcall fun (xmin bbox) (ymin bbox))
        (values (xmax bbox) (ymax bbox))
        (funcall fun (xmax bbox) (ymax bbox)))
  bbox)
                 
(defun loader-font-scale (size loader)
  "Return the horizontal and vertical scaling needed to draw the
glyphs of LOADER at SIZE units."
  (float (/ size (zpb-ttf:units/em loader))))

(defun string-bounding-box (string size loader)
  (let* ((bbox (empty-bounding-box))
         (scale (loader-font-scale size loader))
         (fun (make-transform-function (scaling-matrix scale scale)))
         (glyphs (string-glyphs string loader))
         (offset 0))
    (loop for (glyph . rest) on glyphs do
          (let ((glyph-box (advance-bounding-box (bounding-box glyph) offset)))
            (setf bbox (nmerge-bounding-boxes bbox glyph-box))
            (incf offset (zpb-ttf:advance-width glyph))
            (when rest
              (let* ((next-glyph (first rest))
                     (kerning (zpb-ttf:kerning-offset glyph next-glyph loader)))
                (incf offset kerning)))))
    (ntransform-bounding-box bbox fun)))
