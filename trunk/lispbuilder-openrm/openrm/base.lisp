
(in-package #:simfin)

(defclass foreign-object ()
  ((foreign-pointer-to-object
    :reader fp
    :initform (error ":FP must not be NIL.")
    :initarg :fp)
   (garbage-collect
    :reader gc-p
    :initform t
    :initarg :gc)
   (free-function
    :initform #'cffi:foreign-free
    :initarg :free))
  (:documentation 
   "A wrapper around the foreign object stored in `FP`.
    Finalizes the foreign object using the function in `:FREE` when `GC-P` is `T` when the wrapper is garbage collected.
    The foreign object is explicitely freed by calling `FREE`."))

(defmethod initialize-instance :around ((self foreign-object)
                                        &key &allow-other-keys)
  (call-next-method)
  (when (gc-p self)
    (let ((foreign-pointer (slot-value self 'foreign-pointer-to-object))
	  (foreign-free (slot-value self 'free-function)))
      (unless foreign-pointer
        (error "`:FP` is NIL. No foreign object defined."))
      (unless foreign-free
        (error "`:FREE` is NIL. No function to free the foreign object defined."))
      (tg:finalize self (lambda () (funcall foreign-free foreign-pointer))))))

(defgeneric fp (foreign-object)
  (:documentation "Returns the foreign object reference."))

(defgeneric free (foreign-object)
  (:documentation "An explicit cleanup method. When freed, `GC-P` will be NIL."))
(defmethod free ((self foreign-object))
  (when (slot-value self 'free-function)
    (funcall (slot-value self 'free-function) (slot-value self 'foreign-pointer-to-object)))
  (setf (slot-value self 'foreign-pointer-to-object) nil
	(slot-value self 'garbage-collect) nil
        (slot-value self 'free-function) nil)
  (tg:cancel-finalization self))

(defgeneric (setf gc-p) (value foreign-object)
  (:documentation "Enables or disables finalization."))
(defmethod (setf gc-p) (value (self foreign-object))
  (if value
    (let ((foreign-pointer (slot-value self 'foreign-pointer-to-object))
          (foreign-free (slot-value self 'free-function)))
      (unless foreign-pointer
        (error "`:FP` is NIL. No foreign object defined."))
      (unless foreign-free
        (error "`:FREE` is NIL. No function to free the foreign object defined."))
      (setf (slot-value self 'garbage-collect) t)
      (tg:cancel-finalization self)
      (tg:finalize self (lambda () (funcall foreign-free foreign-pointer))))
    (progn
      (setf (slot-value self 'garbage-collect) nil)
      (tg:cancel-finalization self))))

(defgeneric this-fp (foreign-object)
  (:documentation "Returns the foreign object reference. This method may not be redefined by a subclass."))
(defmethod this-fp ((self foreign-object))
  (slot-value self 'foreign-pointer-to-object))

(defun simple-free (func-fp type)
  (declare (ignore type))
  #'(lambda (obj-fp)
      (when (and (cffi:pointerp obj-fp) (cffi:null-pointer-p obj-fp))
	(funcall func-fp obj-fp))))
