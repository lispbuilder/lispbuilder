
(in-package #:rm)

(defclass openrm-object (foreign-object)
  ((id
    :reader id
    :initform (generate-id))
   (copy-data
    :initform t
    :reader copy-p)))

(defmethod copy-data ((self openrm-object))
  (if (gc-p self)
    :RM-COPY-DATA
    :RM-DONT-COPY-DATA))

(defmethod (setf copy-p) (value (self openrm-object))
  (unless value
    (when (gc-p self)
      (setf (slot-value self 'copy-data) :RM-DONT-COPY-DATA)
      (setf (gc-p self) nil))))


;(defmethod copy-data :around ((self openrm-object))
;  (format t "COPY-DATA: ~A: ~A~%" self (if (gc-p self)
;                                         :RM-COPY-DATA
;                                         :RM-DONT-COPY-DATA))
;  (call-next-method))

;(defmethod copy-p :around ((self openrm-object))
;  (format t "COPY-P: ~A: ~A~%" self (slot-value self 'copy-data))
;  (call-next-method))


;(defmethod (setf copy-p) :around (value (self openrm-object))
;  (format t "(SETF COPY-P): ~A: ~A~%" self value)
;  (call-next-method))
