
;; OpenRM library using CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>
;; see COPYING for license

(in-package #:lispbuilder-openrm-cffi)

;; (defun create-list-if-not (var)
;;   (if (listp var)
;;       var
;;       (list var)))

;; (defun vertex-copy (vertices vertex-array)
;;   (let ((index 0))
;;     (mapcar #'(lambda (vertex)
;; 		(copy-to-foreign-vertex vertex (cffi:mem-aref vertex-array 'rm-vertex-3d index))
;; 		(incf index))
;; 	    vertices))
;;   vertex-array)

;; (defun color-copy (colors col-array)
;;   (let ((index 0))
;;     (mapcar #'(lambda (color)
;; 		(copy-to-foreign-color color (cffi:mem-aref col-array 'RM-color-4D index))
;; 		(incf index))
;; 	    colors))
;;   col-array)


(defctype s-float :float)

;; (defcstruct matrix
;;   (m s-float :count 16))

(defmethod translate-to-foreign (value (type (eql 's-float)))
  (coerce value 'single-float))

(defmethod translate-to-foreign (value (type (eql 'float-pointer)))
  (let ((float-ptr (cffi:foreign-alloc :float)))
    (setf (cffi:mem-aref float-ptr :float) value)
    (values float-ptr t)))

(defmethod translate-to-foreign (value (type (eql 'float-array)))
  (values (cffi:foreign-alloc :float :count (length value) :initial-contents value) t))

(defmethod free-translated-object (ptr (name (eql 'float-pointer)) free-p)
  (if free-p
      (cffi:foreign-free ptr)))

(defmethod free-translated-object (ptr (name (eql 'float-array)) free-p)
  (if free-p
      (cffi:foreign-free ptr)))


