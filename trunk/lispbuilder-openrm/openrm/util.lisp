
(in-package #:rm)

;; From _3b in #lisp
(defmacro cast (type value)
  "Coerces the value `VALUE` to the type `TYPE`."
  `(coerce ,value ',type))

(declaim (inline is-valid-ptr))
(defun is-valid-ptr (pointer)
  "IS-VALID-PTR <CFFI pointer>
  Will return T if 'pointer' is a valid <CFFI pointer> and is non-null."
  (and (cffi:pointerp pointer) (not (cffi:null-pointer-p pointer))))

(declaim (inline to-radian))
(defun to-radian (degree)
  "converts degrees to radians."
  (* degree (/ PI 180)))

(declaim (inline to-degree))
(defun to-degree (radian)
  "converts radians to degrees."
  (/ radian (/ PI 180)))

(defun fformat (&rest args)
    "Removes the need for stream format argument (always to *standard-output*),
    always puts a newline at the end and forces output immediately, 
    all for ease of debugging."
    (apply #'format t args) (terpri t) (force-output t))

(defmacro zero-mem (object type)
  (let ((i (gensym)))
    `(loop for ,i from 0 below (cffi:foreign-type-size (quote ,type)) do
           (setf (cffi:mem-aref ,object :char ,i) 0))))

(defmacro set-struct-members (object type &body values)
  (let ((elements (loop for i in values collect (car i))))
    `(cffi:with-foreign-slots (,elements ,object ,type)
       ,@(loop for (name value) in values collect `(setf ,name ,value)))))

(let ((id 0))
  (defun generate-id ()
    (incf id)))

;;#define pixeltovp(p,d) ((float)((p) - ((d) >> 1)) / (float)((d) >> 1))
(declaim (inline pixel-to-viewport))
(defun pixel-to-viewport (pixel dimension)
  (cast single-float (/ (- pixel (ash dimension -1))
			(ash dimension -1))))


