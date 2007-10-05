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
;;; $Id: graphics-state.lisp,v 1.15 2007/10/01 02:24:44 xach Exp $

(in-package #:vecto)

(defconstant +png-channels+ 4)
(defconstant +png-color-type+ :truecolor-alpha)

(defclass graphics-state ()
  ((paths
    :initarg :paths
    :accessor paths)
   (path
    :initarg :path
    :accessor path)
   (height
    :initarg :height
    :accessor height)
   (width
    :initarg :width
    :accessor width)
   (image
    :initarg :image
    :accessor image)
   (stroke-color
    :initarg :stroke-color
    :accessor stroke-color)
   (line-width
    :initarg :line-width
    :accessor line-width)
   (dash-vector
    :initarg :dash-vector
    :accessor dash-vector)
   (dash-phase
    :initarg :dash-phase
    :accessor dash-phase)
   (fill-color
    :initarg :fill-color
    :accessor fill-color)
   (join-style
    :initarg :join-style
    :accessor join-style)
   (cap-style
    :initarg :cap-style
    :accessor cap-style)
   (transform-matrix
    :initarg :transform-matrix
    :accessor transform-matrix)
   (clipping-path
    :initarg :clipping-path
    :accessor clipping-path)
   (after-paint-fun
    :initarg :after-paint-fun
    :accessor after-paint-fun)
   (font-loaders
    :initarg :font-loaders
    :accessor font-loaders)
   (font
    :initarg :font
    :accessor font))
  (:default-initargs
   :paths nil
   :path nil
   :stroke-color (make-instance 'rgba-color)
   :line-width 1.0
   :dash-vector nil
   :dash-phase 0
   :fill-color (make-instance 'rgba-color)
   :join-style :miter
   :cap-style :butt
   :transform-matrix (scaling-matrix 1.0 -1.0)
   :after-paint-fun (constantly nil)
   :font-loaders (make-hash-table :test 'equal)
   :font nil))

(defgeneric image-data (state)
  (:method (state)
    (png::image-data (image state))))

(defgeneric transform-function (state)
  (:documentation "Return a function that takes x, y coordinates
and returns them transformed by STATE's current transformation
matrix as multiple values.")
  (:method (state)
    (make-transform-function (transform-matrix state))))


(defgeneric call-after-painting (state fun)
  (:documentation
   "Call FUN after painting, and reset the post-painting fun to a no-op.")
  (:method (state fun)
    (setf (after-paint-fun state)
          (lambda ()
            (funcall fun)
            (setf (after-paint-fun state) (constantly nil))))))

(defgeneric after-painting (state)
  (:documentation "Invoke the post-painting function.")
  (:method (state)
    (funcall (after-paint-fun state))))


(defgeneric apply-matrix (state matrix)
  (:documentation "Replace the current transform matrix of STATE
with the result of premultiplying it with MATRIX.")
  (:method (state matrix)
    (let ((old (transform-matrix state)))
      (setf (transform-matrix state) (mult matrix old)))))

(defgeneric clear-paths (state)
  (:documentation "Clear out any paths in STATE.")
  (:method (state)
    (setf (paths state) nil
          (path state) nil
          (after-paint-fun state) (constantly nil))))


(defun make-image-data (width height bpp)
  "Make an octet vector suitable for use as the image data vector of a
backing image."
  (make-array (* width height bpp)
              :element-type '(unsigned-byte 8)
              :initial-element #x00))

(defun state-image (state width height)
  "Set the backing image of the graphics state to an image of the
specified dimensions."
  (setf (image state)
        (make-instance 'png:png
                       :width width
                       :height height
                       :color-type +png-color-type+
                       :image-data (make-image-data width height
                                                    +png-channels+))
        (width state) width
        (height state) height
        (clipping-path state) (make-clipping-path width height))
  (apply-matrix state (translation-matrix 0 (- height))))
  

(defun find-font-loader (state file)
  (let* ((cache (font-loaders state))
         (key (namestring (truename file))))
    (or (gethash key cache)
        (setf (gethash key cache) (zpb-ttf:open-font-loader file)))))

(defgeneric close-font-loaders (state)
  (:documentation "Close any font loaders that were obtained with GET-FONT.")
  (:method (state)
    (maphash (lambda (filename loader)
               (declare (ignore filename))
               (ignore-errors (zpb-ttf:close-font-loader loader)))
             (font-loaders state))))

(defgeneric clear-state (state)
  (:documentation "Clean up any state in STATE.")
  (:method ((state graphics-state))
    (close-font-loaders state)))


(defmethod copy ((state graphics-state))
  (make-instance 'graphics-state
                 :paths (paths state)
                 :path (path state)
                 :height (height state)
                 :width (width state)
                 :image (image state)
                 :stroke-color (copy (stroke-color state))
                 :line-width (line-width state)
                 :dash-vector (copy-seq (dash-vector state))
                 :dash-phase (dash-phase state)
                 :fill-color (copy (fill-color state))
                 :join-style (join-style state)
                 :cap-style (cap-style state)
                 :transform-matrix (copy-seq (transform-matrix state))
                 :clipping-path (copy (clipping-path state))
                 :after-paint-fun (after-paint-fun state)
                 :font-loaders (font-loaders state)
                 :font (font state)))
