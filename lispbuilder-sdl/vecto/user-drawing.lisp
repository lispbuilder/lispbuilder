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
;;; $Id: user-drawing.lisp,v 1.21 2007/10/01 14:12:55 xach Exp $

(in-package #:vecto)

(defvar *graphics-state*)
(setf (documentation '*graphics-state* 'variable)
      "The currently active graphics state. Bound for the
      duration of WITH-GRAPICS-STATE.")

;;; Low-level path construction

(defun %move-to (state x y)
  (let ((path (paths:create-path :open-polyline)))
    (push (setf (path state) path) (paths state))
    (paths:path-reset path (paths:make-point x y))))

(defun %line-to (state x y)
  (paths:path-extend (path state) (paths:make-straight-line)
                     (paths:make-point x y)))

(defun %curve-to (state cx1 cy1 cx2 cy2 x y)
  "Draw a cubic Bezier curve from the current point to (x,y)
through two control points."
  (let ((control-point-1 (paths:make-point cx1 cy1))
        (control-point-2 (paths:make-point cx2 cy2))
        (end-point (paths:make-point x y)))
    (paths:path-extend (path state)
                       (paths:make-bezier-curve (list control-point-1
                                                      control-point-2))
                       end-point)))

(defun %quadratic-to (state cx cy x y)
  "Draw a quadratic Bezier curve from the current point to (x,y)
through one control point."
  (paths:path-extend (path state)
                     (paths:make-bezier-curve (list (paths:make-point cx cy)))
                     (paths:make-point x y)))

(defun %close-subpath (state)
  (setf (paths::path-type (path state)) :closed-polyline))

;;; Clipping path

(defun %end-path-no-op (state)
  (after-painting state))

(defun %clip-path (state)
  (call-after-painting state
                       (make-clipping-path-function state :nonzero-winding)))

(defun %even-odd-clip-path (state)
  (call-after-painting state
                       (make-clipping-path-function state :even-odd)))

;;; Text

(defun %get-font (state file)
  (find-font-loader state file))

(defun %set-font (state loader size)
  (let* ((scale (loader-font-scale size loader))
         (matrix (scaling-matrix scale scale)))
    (setf (font state)
          (make-instance 'font
                         :loader loader
                         :transform-matrix matrix
                         :size size))))

(defun %draw-string (state x y string)
  (let ((font (font state)))
    (unless font
      (error "No font currently set"))
    (let ((paths (string-paths x y string font)))
      (draw-paths/state paths state))))

(defun %draw-centered-string (state x y string)
  (let* ((font (font state))
         (bbox (string-bounding-box string (size font) (loader font)))
         (width/2 (/ (- (xmax bbox) (xmin bbox)) 2.0)))
    (draw-string (- x width/2) y string)))


;;; Low-level transforms

(defun %translate (state tx ty)
  (apply-matrix state (translation-matrix tx ty)))

(defun %scale (state sx sy)
  (apply-matrix state (scaling-matrix sx sy)))

(defun %skew (state x y)
  (apply-matrix state (skewing-matrix x y)))

(defun %rotate (state radians)
  (apply-matrix state (rotation-matrix radians)))

;;; User-level commands

(defun move-to (x y)
  (%move-to *graphics-state* x y))

(defun line-to (x y)
  (%line-to *graphics-state* x y))

(defun curve-to (cx1 cy1 cx2 cy2 x y)
  (%curve-to *graphics-state* cx1 cy1 cx2 cy2 x y))

(defun quadratic-to (cx cy x y)
  (%quadratic-to *graphics-state* cx cy x y))

(defun close-subpath ()
  (%close-subpath *graphics-state*))

(defun end-path-no-op ()
  (%end-path-no-op *graphics-state*)
  (clear-paths *graphics-state*))

(defun clip-path ()
  (%clip-path *graphics-state*))

(defun even-odd-clip-path ()
  (%even-odd-clip-path *graphics-state*))

(defun get-font (file)
  (%get-font *graphics-state* file))

(defun set-font (font size)
  (%set-font *graphics-state* font size))

(defun draw-string (x y string)
  (%draw-string *graphics-state* x y string))

(defun draw-centered-string (x y string)
  (%draw-centered-string *graphics-state* x y string))

(defun set-dash-pattern (vector phase)
  (if (zerop (length vector))
      (setf (dash-vector *graphics-state*) nil
            (dash-phase *graphics-state*) nil)
      (setf (dash-vector *graphics-state*) vector
            (dash-phase *graphics-state*) phase)))

(defun set-line-cap (style)
  (assert (member style '(:butt :square :round)))
  (setf (cap-style *graphics-state*) style))

(defun set-line-join (style)
  (assert (member style '(:bevel :miter :round)))
  (setf (join-style *graphics-state*) (if (eql style :bevel) :none style)))

(defun set-line-width (width)
  (setf (line-width *graphics-state*) width))

(defun set-rgba-color (color r g b a)
  (setf (red color) (clamp-range 0.0 r 1.0)
        (green color) (clamp-range 0.0 g 1.0)
        (blue color) (clamp-range 0.0 b 1.0)
        (alpha color) (clamp-range 0.0 a 1.0))
  color)

(defun set-rgb-color (color r g b)
  (setf (red color) (clamp-range 0.0 r 1.0)
        (green color) (clamp-range 0.0 g 1.0)
        (blue color) (clamp-range 0.0 b 1.0)
        (alpha color) 1.0)
  color)

(defun set-rgb-stroke (r g b)
  (set-rgb-color (stroke-color *graphics-state*) r g b))

(defun set-rgba-stroke (r g b a)
  (set-rgba-color (stroke-color *graphics-state*) r g b a))

(defun set-rgb-fill (r g b)
  (set-rgb-color (fill-color *graphics-state*) r g b))

(defun set-rgba-fill (r g b a)
  (set-rgba-color (fill-color *graphics-state*) r g b a))

(defun stroke ()
  (draw-stroked-paths *graphics-state*)
  (clear-paths *graphics-state*))

(defun fill-path ()
  (draw-filled-paths *graphics-state*)
  (after-painting *graphics-state*)
  (clear-paths *graphics-state*))

(defun even-odd-fill ()
  (draw-even-odd-filled-paths *graphics-state*)
  (after-painting *graphics-state*)
  (clear-paths *graphics-state*))

(defun fill-and-stroke ()
  (draw-filled-paths *graphics-state*)
  (draw-stroked-paths *graphics-state*)
  (clear-paths *graphics-state*))

(defun even-odd-fill-and-stroke ()
  (draw-even-odd-filled-paths *graphics-state*)
  (draw-stroked-paths *graphics-state*)
  (after-painting *graphics-state*)
  (clear-paths *graphics-state*))


(defun clear-canvas ()
  (let ((color (fill-color *graphics-state*)))
    (fill-image (image-data *graphics-state*)
                (red color)
                (green color)
                (blue color)
                (alpha color))))

(defun translate (x y)
  (%translate *graphics-state* x y))

(defun scale (x y)
  (%scale *graphics-state* x y))

(defun skew (x y)
  (%skew *graphics-state* x y))

(defun rotate (radians)
  (%rotate *graphics-state* radians))

(defun rotate-degrees (degrees)
  (%rotate *graphics-state* (* (/ pi 180) degrees)))

(defun save-png (file)
  (png:write-png (image *graphics-state*) file))

(defun save-png-stream (stream)
  (png:write-png-stream (image *graphics-state*) stream))

(defmacro with-canvas ((&key width height) &body body)
  `(let ((*graphics-state* (make-instance 'graphics-state)))
     (state-image *graphics-state* ,width ,height)
     (unwind-protect
          (progn
            ,@body)
       (clear-state *graphics-state*))))

(defmacro with-graphics-state (&body body)
  `(let ((*graphics-state* (copy *graphics-state*)))
     ,@body))
