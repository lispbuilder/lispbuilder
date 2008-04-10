
(in-package #:sdl-mixer-cffi)

(defgeneric this-fp (foreign-object)
  (:documentation "Returns the foreign pointer to FOREIGN-OBJECT"))

(defgeneric free (foreign-object)
  (:documentation "This is the general explicit cleanup method for all FOREIGN-OBJECTs.
Objects that subclass FOREIGN-OBJECT should specify an :AFTER
method on FREE to clean up any additional fields, if necessary."))

(defgeneric (setf gc-p) (value foreign-object)
  (:documentation "Turns garbage collection for the FOREIGN-OBJECT on when T, or off when NIL."))

(defclass foreign-object ()
  ((foreign-pointer-to-object
    :reader fp
    :initform nil
    :initarg :fp)
   (garbage-collect
    :reader gc-p
    :initform t
    :initarg :gc)
   (free-function
    :initform (error "FREE-FUNCTION must not be NIL.")
    :initarg :free)))

(defmethod initialize-instance :around ((self foreign-object)
				       &key)
  (call-next-method)
  (when (gc-p self)
    (let ((foreign-pointer (this-fp self))
	  (foreign-free (slot-value self 'free-function)))
      (tg:finalize self (lambda ()
			  (funcall foreign-free foreign-pointer))))))

(defmethod free ((self foreign-object))
  ;; This is the general explicit cleanup method for all OpenRM objects.
  ;; Objects that subclass FOREIGN-OBJECT should specify an :AFTER
  ;; method on FREE to clean up any additional fields, if necessary.
  (funcall (slot-value self 'free-function) (this-fp self))
  (tg:cancel-finalization self)
  (setf (slot-value self 'foreign-pointer-to-object) nil
	(slot-value self 'garbage-collect) nil))

;; Fixme , TODO. This is so very wrong.
;; Everything inherits from foreign-object, so having
;; an rm-primitive-delete is going to seriously screw things up.
(defmethod (setf gc-p) (value (self foreign-object))
  (if value
      (let ((foreign-pointer (this-fp self))
	    (foreign-free (slot-value self 'free-function)))
	(setf (slot-value self 'garbage-collect) t)
	(tg:cancel-finalization self)
	(tg:finalize self (lambda ()
			    (funcall foreign-free foreign-pointer))))
      (progn
	(setf (slot-value self 'garbage-collect) nil)
	(tg:cancel-finalization self))))

(defmethod this-fp ((self foreign-object))
  "Returns the reference to the foreign object for this FOREIGN-OBJECT.
The difference between FP and THIS-FP, is that FP can be overriden, for example
to refer to the TARGET-NODE of a META-NODE."
  (slot-value self 'foreign-pointer-to-object))

(defun simple-free (func-fp type)
  (declare (ignore type))
  #'(lambda (obj-fp)
      (when (sdl:is-valid-ptr obj-fp)
	(funcall func-fp obj-fp))))
