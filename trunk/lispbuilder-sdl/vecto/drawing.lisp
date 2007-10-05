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
;;; $Id: drawing.lisp,v 1.17 2007/10/01 19:05:13 xach Exp $

(in-package #:vecto)

(deftype octet ()
  '(unsigned-byte 8))

(deftype vector-index ()
  `(mod ,array-dimension-limit))

(deftype octet-vector ()
  '(simple-array (unsigned-byte 8) (*)))

(defun nonzero-winding-alpha (alpha)
  (min 255 (abs alpha)))

(defun even-odd-alpha (alpha)
  (let ((value (mod alpha 512)))
    (min 255 (if (< value 256) value (- 512 value)))))

;; ( (t) = (a) * (b) + 0x80, ( ( ( (t)>>8 ) + (t) )>>8 ) )

(defun imult (a b)
  (let ((temp (+ (* a b) #x80)))
    (logand #xFF (ash (+ (ash temp -8) temp) -8))))

(defun lerp (p q a)
  (logand #xFF (+ p (imult a (- q p)))))

(defun prelerp (p q a)
  (logand #xFF (- (+ p q) (imult a p))))

(defun draw-function (data width height r.fg g.fg b.fg a.fg alpha-fun)
  "From http://www.teamten.com/lawrence/graphics/premultiplication/"
  (declare (ignore height))
  (let ((r.fg (float-octet r.fg))
        (g.fg (float-octet g.fg))
        (b.fg (float-octet b.fg))
        (a.fg (float-octet a.fg)))
    (lambda (x y alpha)
      (setf alpha (funcall alpha-fun alpha))
      (when (plusp alpha)
        (let* ((i (* +png-channels+ (+ x (* y width))))
               (r.bg (aref data (+ i 0)))
               (g.bg (aref data (+ i 1)))
               (b.bg (aref data (+ i 2)))
               (a.bg (aref data (+ i 3)))
               (a.fg (imult alpha a.fg))
               (gamma (prelerp a.fg a.bg a.bg)))
          (flet ((blend (fg bg)
                   (let ((value (lerp (imult bg a.bg) fg a.fg)))
                     (float-octet (/ value gamma)))))
            (unless (zerop gamma)
              (setf (aref data (+ i 0)) (blend r.fg r.bg)
                    (aref data (+ i 1)) (blend g.fg g.bg)
                    (aref data (+ i 2)) (blend b.fg b.bg)))
            (setf (aref data (+ i 3)) gamma)))))))

(defun draw-function/clipped (data clip-data
                              width height
                              r.fg g.fg b.fg a.fg
                              alpha-fun)
  "Like DRAW-FUNCTION, but uses uses the clipping channel."
  (declare (ignore height))
  (let ((r.fg (float-octet r.fg))
        (g.fg (float-octet g.fg))
        (b.fg (float-octet b.fg))
        (a.fg (float-octet a.fg)))
    (lambda (x y alpha)
      (let* ((clip-index (+ x (* y width)))
             (clip (aref clip-data clip-index)))
        (setf alpha (imult clip (funcall alpha-fun alpha)))
        (when (plusp alpha)
          (let* ((i (* clip-index +png-channels+))
                 (r.bg (aref data (+ i 0)))
                 (g.bg (aref data (+ i 1)))
                 (b.bg (aref data (+ i 2)))
                 (a.bg (aref data (+ i 3)))
                 (a.fg (imult alpha a.fg))
                 (gamma (prelerp a.fg a.bg a.bg)))
            (flet ((blend (fg bg)
                     (let ((value (lerp (imult bg a.bg) fg a.fg)))
                       (float-octet (/ value gamma)))))
              (unless (zerop gamma)
                (setf (aref data (+ i 0)) (blend r.fg r.bg)
                      (aref data (+ i 1)) (blend g.fg g.bg)
                      (aref data (+ i 2)) (blend b.fg b.bg)))
              (setf (aref data (+ i 3)) gamma))))))))

(defun make-draw-function (data clipping-path
                           width height
                           r g b a
                           alpha-fun)
  (if (emptyp clipping-path)
      (draw-function data width height r g b a alpha-fun)
      (draw-function/clipped data (clipping-data clipping-path)
                             width height
                             r g b a
                             alpha-fun)))

(defun intersect-clipping-paths (data temp)
  (declare (type (simple-array (unsigned-byte 8) (*)) data temp))
  (map-into data #'imult temp data))

(defun draw-clipping-path-function (data width height alpha-fun)
  (declare (ignore height)
           (type (simple-array (unsigned-byte 8) (*)) data))
  (lambda (x y alpha)
    (let ((i (+ x (* width y))))
      (let ((alpha (funcall alpha-fun alpha)))
        (setf (aref data i) alpha)))))

(defun draw-paths (&key width height paths
                   transform-function
                   draw-function)
  "Use DRAW-FUNCTION as a callback for the cells sweep function
for the set of paths PATHS."
  (let ((state (aa:make-state))
        (paths (mapcar (lambda (path)
                         ;; FIXME: previous versions lacked
                         ;; paths:path-clone, and this broke fill &
                         ;; stroke because transform-path damages the
                         ;; paths. It would be nicer if transform-path
                         ;; wasn't destructive, since I didn't expect
                         ;; it to be.
                         (transform-path (paths:path-clone path)
                                         transform-function))
                       paths)))
    (vectors:update-state state paths)
    (aa:cells-sweep/rectangle state 0 0 width height draw-function)))

;;; FIXME: this was added for drawing text paths, but the text
;;; rendering mode could be changed in the future, making it a little
;;; silly to have a fixed draw-function.

(defun draw-paths/state (paths state)
  (draw-paths :paths paths
              :width (width state)
              :height (height state)
              :transform-function (transform-function state)
              :draw-function (fill-draw-function state)))

(defun fill-image (image-data red green blue alpha)
  "Completely fill IMAGE with the given colors."
  (let ((r (float-octet red))
        (g (float-octet green))
        (b (float-octet blue))
        (a (float-octet alpha)))
    (do ((h 0 (+ h 4))
         (i 1 (+ i 4))
         (j 2 (+ j 4))
         (k 3 (+ k 4)))
        ((<= (length image-data) k))
      (setf (aref image-data h) r
            (aref image-data i) g
            (aref image-data j) b
            (aref image-data k) a))))

(defun state-draw-function (state color fill-style)
  "Create a draw function for the graphics state STATE."
  (make-draw-function (image-data state)
                      (clipping-path state)
                      (width state)
                      (height state)
                      (red color)
                      (green color)
                      (blue color)
                      (alpha color)
                      (ecase fill-style
                        (:even-odd #'even-odd-alpha)
                        (:nonzero-winding #'nonzero-winding-alpha))))

(defun stroke-draw-function (state)
  (state-draw-function state (stroke-color state) :nonzero-winding))

(defun fill-draw-function (state)
  (state-draw-function state (fill-color state) :nonzero-winding))

(defun even-odd-fill-draw-function (state)
  (state-draw-function state (fill-color state) :even-odd))

(defun tolerance-scale (state)
  (let ((matrix (transform-matrix state)))
    (abs (/ 1.0 (min (transform-matrix-x-scale matrix)
                     (transform-matrix-y-scale matrix))))))
         

(defun draw-stroked-paths (state)
  "Create a set of paths representing a stroking of the current
paths of STATE, and draw them to the image."
  (let ((paths (dash-paths (paths state)
                           (dash-vector state)
                           (dash-phase state)))
        (paths:*bezier-distance-tolerance*
         (* paths:*bezier-distance-tolerance* (tolerance-scale state))))
    (setf paths (stroke-paths paths
                              :line-width (line-width state)
                              :join-style (join-style state)
                              :cap-style (cap-style state)))
    (draw-paths :paths paths
                :width (width state)
                :height (height state)
                :transform-function (transform-function state)
                :draw-function (stroke-draw-function state))))

(defun close-paths (paths)
  (dolist (path paths)
    (setf (paths::path-type path) :closed-polyline)))

(defun draw-filled-paths (state)
  "Fill the paths of STATE into the image."
  (close-paths (paths state))
  (draw-paths :paths (paths state)
              :width (width state)
              :height (height state)
              :transform-function (transform-function state)
              :draw-function (fill-draw-function state)))

(defun draw-even-odd-filled-paths (state)
  "Fill the paths of STATE into the image."
  (close-paths (paths state))
  (draw-paths :paths (paths state)
              :width (width state)
              :height (height state)
              :transform-function (transform-function state)
              :draw-function (even-odd-fill-draw-function state)))

(defun draw-clipping-path (state alpha-fun)
  (let ((data (writable-clipping-data (clipping-path state)))
        (scratch (scratch (clipping-path state)))
        (width (width state))
        (height (height state)))
    (declare (type octet-vector data scratch))
    (fill scratch 0)
    (draw-paths :paths (paths state)
                :width (width state)
                :height (height state)
                :transform-function (transform-function state)
                :draw-function (draw-clipping-path-function scratch
                                                            width
                                                            height
                                                            alpha-fun))
    (intersect-clipping-paths data scratch)))

(defun make-clipping-path-function (state type)
  (ecase type
    (:nonzero-winding
     (lambda ()
       (draw-clipping-path state #'nonzero-winding-alpha)))
    (:even-odd
     (lambda ()
       (draw-clipping-path state #'even-odd-alpha)))))

