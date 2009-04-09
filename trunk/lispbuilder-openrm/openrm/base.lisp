
(in-package #:rm)

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
			  (log5:log-for (tg) "FINALIZE: ~A" foreign-pointer)
			  (log5:log-for (tg) "   FINALIZE Attempt: ~A" foreign-pointer)
			  (funcall foreign-free foreign-pointer)
			  (log5:log-for (tg) "   FINALIZE Complete: ~A" foreign-pointer))))))

(defmethod free ((self foreign-object))
  ;; This is the general explicit clenup method for all OpenRM objects.
  ;; Objects that subclass OPENRM-OBJECT should specify an :AFTER
  ;; method on FREE to clean up any additional fields, if necessary.
  (log5:log-for (manual-free) "FREE: \(MANUAL FREE\) ~A" (this-fp self))
  (log5:log-for (manual-free) "   FREE Attempt \(MANUAL FREE\) ~A" (this-fp self))
  (funcall (slot-value self 'free-function) (this-fp self))
  (tg:cancel-finalization self)
  (log5:log-for (manual-free) "   FREE COMPETE \(MANUAL FREE\) ~A" (this-fp self))
  (setf (slot-value self 'foreign-pointer-to-object) nil
	(slot-value self 'garbage-collect) nil))

;; Fixme , TODO. This is so very wrong.
;; Everything inherits from openrm-object, so having
;; an rm-primitive-delete is going to seriously screw things up.
(defmethod (setf gc-p) (value (self foreign-object))
  (if value
      (let ((foreign-pointer (this-fp self))
	    (foreign-free (slot-value self 'free-function)))
	(setf (slot-value self 'garbage-collect) t)
	(tg:cancel-finalization self)
	(tg:finalize self (lambda ()
			    (log5:log-for (tg) "FINALIZE: ~A" foreign-pointer)
			    (log5:log-for (tg) "   FINALIZE Attempt: ~A" foreign-pointer)  
			    (funcall foreign-free foreign-pointer)
			    (log5:log-for (tg) "   FINALIZE Complete: ~A" foreign-pointer))))
      (progn
	(setf (slot-value self 'garbage-collect) nil)
	(log5:log-for (tg) "CANCEL-FINALIZATION: ~A" (this-fp self))
	(log5:log-for (tg) "   CANCEL-FINALIZATION Attempt: ~A" (this-fp self))
	(tg:cancel-finalization self)
	(log5:log-for (tg) "   CANCEL-FINALIZATION Complete: ~A" (this-fp self)))))

(defmethod this-fp ((self foreign-object))
  "Returns the reference to the foreign object for this OPENRM-OBJECT.
The difference between FP and THIS-FP, is that FP can be overriden, for example
to refer to the TARGET-NODE of a META-NODE."
  (slot-value self 'foreign-pointer-to-object))

(defun simple-free (func-fp type)
  #'(lambda (obj-fp)
      (when (is-valid-ptr obj-fp)
	(log5:log-for (free) "      SIMPLE-FREE: ~A ~A" type obj-fp)
	(funcall func-fp obj-fp))))
