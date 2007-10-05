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
;;; $Id: paths.lisp,v 1.2 2007/09/28 18:11:35 xach Exp $

(in-package #:vecto)

;;; Applying a transform function to a path

(defgeneric transformablep (interpolation)
  (:method (interpolation)
    nil)
  (:method ((interpolation paths::bezier))
    t)
  (:method ((interpolation (eql :straight-line)))
    t))

(defun transform-point (point fun)
  (multiple-value-call #'paths:make-point
    (funcall fun (paths:point-x point) (paths:point-y point))))

(defgeneric transform-interpolation (interpolation fun)
  (:method (interpolation fun)
    (declare (ignore fun))
    (error "Unhandled interpolation ~A" interpolation))
  (:method ((interpolation symbol) fun)
    (declare (ignore fun))
    interpolation)
  (:method ((interpolation paths::bezier) fun)
    (let ((control-points (slot-value interpolation
                                      'paths::control-points)))
      (dotimes (i (length control-points) interpolation)
        (setf (aref control-points i)
              (transform-point (aref control-points i) fun))))))

(defun empty-path-p (path)
  (zerop (length (paths::path-knots path))))
  

(defun transform-path (path fun)
  (when (empty-path-p path)
    (return-from transform-path path))
  (let ((new-path (paths:create-path (paths::path-type path)))
        (iterator (paths:path-iterator-segmented path
                                                 (complement #'transformablep))))
    (loop
     (multiple-value-bind (interpolation knot endp)
         (paths:path-iterator-next iterator)
       (paths:path-extend new-path
                          (transform-interpolation interpolation fun)
                          (transform-point knot fun))
       (when endp
         (return new-path))))))

(defun transform-paths (paths fun)
  (mapcar (lambda (path) (transform-path path fun)) paths))


;;; Applying a dash pattern

(defun apply-dash-phase (dash-vector phase)
  "cl-vectors and PDF have different semantics for dashes. Given
a PDF-style dash vector and phase value, return a
cl-vectors-style dash vector and TOGGLE-P value."
  (let ((sum (reduce #'+ dash-vector)))
    (when (or (zerop phase)
              (= phase sum))
      ;; Don't bother doing anything for an empty phase
      (return-from apply-dash-phase (values dash-vector 0))))
  (let ((index 0)
        (invertp t))
    (flet ((next-value ()
             (cond ((< index (length dash-vector))
                    (setf invertp (not invertp)))
                   (t
                    (setf invertp nil
                          index 0)))
             (prog1
                 (aref dash-vector index)
               (incf index)))
           (join (&rest args)
             (apply 'concatenate 'vector
                    (mapcar (lambda (thing)
                              (if (vectorp thing)
                                  thing
                                  (vector thing)))
                            args))))
      (loop
       (let ((step (next-value)))
         (decf phase step)
         (when (not (plusp phase))
           (let ((result (join (- phase)
                               (subseq dash-vector index)
                               dash-vector)))
             (when invertp
               (setf result (join 0 result)))
             (return (values result
                             (- (length result) (length dash-vector)))))))))))



(defun dash-paths (paths dash-vector dash-phase)
  (if dash-vector
      (multiple-value-bind (sizes cycle-index)
              (apply-dash-phase dash-vector dash-phase)
        (paths:dash-path paths sizes :cycle-index cycle-index))
      paths))

(defun stroke-paths (paths &key line-width join-style cap-style)
  (mapcan (lambda (path)
            (paths:stroke-path path line-width
                               :joint join-style
                               :caps cap-style))
          paths))
