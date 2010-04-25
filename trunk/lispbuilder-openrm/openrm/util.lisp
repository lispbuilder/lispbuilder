
(in-package #:rm)

;; From _3b in #lisp
(defmacro cast (type value)
  "Coerces the value `VALUE` to the type `TYPE`."
  `(coerce ,value ',type))

(declaim (inline to-radian))
(defun to-radian (degree)
  "Converts degrees to radians."
  (* degree (/ PI 180)))

(declaim (inline to-degree))
(defun to-degree (radian)
  "Converts radians to degrees."
  (/ radian (/ PI 180)))

(defun fformat (&rest args)
  "Removes the need for stream format argument (always to *standard-output*),
    always puts a newline at the end and forces output immediately, 
    all for ease of debugging."
  (apply #'format t args) (terpri t) (force-output t))

(defmacro zero-mem (object type)
  "Assigns zero to object."
  (let ((i (gensym)))
    `(loop for ,i from 0 below (cffi:foreign-type-size (quote ,type)) do
           (setf (cffi:mem-aref ,object :char ,i) 0))))

(defmacro set-foreign-slots (object type &body slot-value-pair)
  "For an object being a foreign structure of type, the slot-value-pair is an assoc list containing the
    foreign slot name and the value the slot will be set to."
  (let ((elements (loop for i in slot-value-pair collect (car i))))
    `(cffi:with-foreign-slots (,elements ,object ,type)
       ,@(loop for (name value) in slot-value-pair collect `(setf ,name ,value)))))

(let ((id 0))
  (defun generate-id ()
    "Generate a unique ID. Generally used to tag an object."
    (incf id)))

;;#define pixeltovp(p,d) ((float)((p) - ((d) >> 1)) / (float)((d) >> 1))
(declaim (inline pixel-to-viewport))
(defun pixel-to-viewport (pixel dimension)
  "Convert a pixel coordinate to a viewport coordinate."
  (cast single-float (/ (- pixel (ash dimension -1))
			(ash dimension -1))))

(declaim (inline is-valid-ptr))
(defun is-valid-ptr (pointer)
  "Returns T when the object is a valid `CFFI` pointer and not null."
  (and (cffi:pointerp pointer) (not (cffi:null-pointer-p pointer))))