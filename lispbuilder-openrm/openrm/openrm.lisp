
(in-package #:rm)

(defclass trackable ()
  ((id
  :reader id
  :initform (generate-id)))
  (:documentation
   "Creates and assigns a unique id to the object upon object creation."))

(defclass copyable-object ()
  ((copy-data
    :initform t
    :reader copy-p)
   (free-on-delete-p
    :initarg :free-on-delete
    :accessor free-on-delete-p
    :initform t)
   (free-on-delete-fn
    :initarg :free-on-delete-fn
    :accessor free-on-delete-fn
    :initform nil)))

(defmethod initialize-instance :after ((self copyable-object) &key
                                        copy-p)
  (unless copy-p
    (setf (copy-p self) copy-p)))

(defmethod copy-data ((self copyable-object))
  (if (slot-value self 'copy-data)
    :RM-COPY-DATA
    :RM-DONT-COPY-DATA))

(defmethod (setf copy-p) (value (self copyable-object))
  ;; Only works one way, from :RM-COPY-DATA to :RM-DONT-COPY-DATA
  (unless value
    (setf (slot-value self 'copy-data) nil)
    ;(when (gc-p self)
    ;  (setf  (gc-p self) nil))
    ))

(defmethod free-callback ((self copyable-object))
  (cffi-sys:%callback (if (free-on-delete-p self)
                        (free-on-delete-fn self)
                        'ignore-proc)))

(cffi:defcallback ignore-proc :pointer
    ((data-fp :pointer))
  "Called when a copyable object is deleted,
but the memory should be freed by the application."
  (declare (ignorable data-fp))
  (log5:log-for (info) "DEFCALLBACK:IGNORE-PROC")
  (cffi:null-pointer))

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
