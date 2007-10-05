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
;;; $Id: clipping-paths.lisp,v 1.2 2007/10/01 16:25:48 xach Exp $

(in-package #:vecto)

;;; Clipping paths are represented as a grayscale channel against
;;; which drawing operations are masked; it's intersected with the
;;; alpha channel. They are part of the graphics state that are saved
;;; and restored by WITH-GRAPHICS-STATE. However, there's no reason to
;;; pay a channel copying penalty if the clipping path is not
;;; modified, or pay a data creation/drawing penalty if the clipping
;;; path is empty.
;;;
;;; This is implemented by making WRITABLE-CLIPPING-DATA the method to
;;; obtain the data of a clipping path; it will create data for an
;;; empty clipping path, and copy data for a clipping path in a
;;; temporary graphics state. If WRITABLE-CLIPPING-DATA is never
;;; called, no mask will be created, and drawing operations won't
;;; bother consulting the clipping path.
;;;
;;; TODO: Store a bounding box with a clipping path, so drawing can be
;;; limited to the clipping path area when possible.

(defclass clipping-path ()
  ((height
    :initarg :height
    :accessor height)
   (width
    :initarg :width
    :accessor width)
   (data
    :initarg :data
    :accessor data)
   (scratch
    :initarg :scratch
    :accessor scratch
    :documentation "A temporary channel used to store the new clipping
    path to intersect with the old one.")))

(defclass empty-clipping-path (clipping-path) ())

(defclass proxy-clipping-path (clipping-path) ())

(defmethod print-object ((clipping-path clipping-path) stream)
  (print-unreadable-object (clipping-path stream :type t :identity t)
    (format stream "~Dx~D" (width clipping-path) (height clipping-path))))

(defmethod copy ((clipping-path clipping-path))
  (make-instance 'proxy-clipping-path
                 :data (data clipping-path)
                 :scratch (scratch clipping-path)
                 :height (height clipping-path)
                 :width (width clipping-path)))

(defmethod copy ((clipping-path empty-clipping-path))
  (make-instance 'empty-clipping-path
                 :height (height clipping-path)
                 :width (width clipping-path)))

(defgeneric emptyp (object)
  (:method (object)
    nil)
  (:method ((object empty-clipping-path))
    t))

(defun make-clipping-channel (width height initial-element)
  (make-array (* width height)
              :element-type '(unsigned-byte 8)
              :initial-element initial-element))

(defgeneric clipping-data (object)
  (:method ((clipping-path clipping-path))
    (data clipping-path))
  (:method ((clipping-path empty-clipping-path))
    nil))

(defgeneric writable-clipping-data (object)
  (:method ((clipping-path clipping-path))
    (data clipping-path))
  (:method ((clipping-path empty-clipping-path))
    (let* ((width (width clipping-path))
           (height (height clipping-path))
           (data (make-clipping-channel width height #xFF))
           (scratch (make-clipping-channel width height #x00)))
      (change-class clipping-path 'clipping-path
                    :data data
                    :scratch scratch)
      data))
  (:method ((clipping-path proxy-clipping-path))
    (let ((data (copy-seq (data clipping-path))))
      (change-class clipping-path 'clipping-path :data data)
      data)))

(defun make-clipping-path (width height)
  (make-instance 'empty-clipping-path :width width :height height))
