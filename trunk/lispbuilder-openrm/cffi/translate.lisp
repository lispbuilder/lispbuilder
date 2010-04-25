
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

(defun to-s-float (value)
  (coerce value 'single-float))

(cffi:defctype s-float (:wrapper :float
				 :to-c to-s-float))


(defmethod cffi:translate-to-foreign (value (type (eql 'rm-cffi::rmode-pointer)))
  value)

(defmethod cffi:translate-to-foreign (value (type (eql 'rm-cffi::rmvertex3d-pointer)))
  value)

(defmethod cffi:translate-to-foreign (value (type (eql 'rmcamera3d-pointer)))
  value)

;; (defmethod cffi:translate-to-foreign ((value vector) (type (eql 'rmvertex3d-pointer)))
;;   (values value t))


;; (defcstruct matrix
;;   (m s-float :count 16))

(defmethod translate-to-foreign (value (type (eql 's-float)))
  (coerce value 'single-float))

;; (defmethod translate-to-foreign (value (type (eql 'float-pointer)))
;;   (let ((float-ptr (cffi:foreign-alloc :float)))
;;     (setf (cffi:mem-aref float-ptr :float) value)
;;     (values float-ptr t)))

(defmethod translate-to-foreign (value (type (eql 'float-array)))
  (values (cffi:foreign-alloc :float :count (length value) :initial-contents value) t))

;; (defmethod free-translated-object (ptr (name (eql 'float-pointer)) free-p)
;;   (if free-p
;;       (cffi:foreign-free ptr)))

(defmethod free-translated-object (ptr (name (eql 'float-array)) free-p)
  (if free-p
      (cffi:foreign-free ptr)))

(defun to-rm-enum (value)
  (if (keywordp value)
      (cffi:foreign-enum-value 'rm-enum-wrapper value)
      (if value
	  1
	  0)))

(defun from-rm-enum (value)
  (case value
    ((-1 0) nil)
    (1 t)
    (otherwise (cffi:foreign-enum-keyword 'rm-enum-wrapper value))))

(defun pointer-to-from-rm-enum (value)
  (case (cffi::mem-aref value :int)
    ((-1 0) nil)
    (1 t)
    (otherwise (foreign-enum-keyword 'rm-enum-wrapper (cffi::mem-aref value :int)))))

;; (defmethod translate-from-foreign (value (type (eql 'rm-enum)))
;;   (format t "value == ~A~%" value)
;;   (case value
;;       (1 t)
;;       ((-1 0) nil)
;;       (otherwise (cffi:foreign-enum-keyword 'rm-enum value))))
