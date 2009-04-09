
(in-package #:lispbuilder-openrm)

(defclass text-primitive (primitive)
  ()
  (:default-initargs
   :fp (rm-cffi::rm-Primitive-New :rm-text)))

;; (defmethod (setf text) ((str string) (self text-primitive))
;;   (cffi:with-foreign-pointer-as-string (f-string 100 size)
;;     (lisp-string-to-foreign str f-string (length str))
;;     (rm-cffi::rm-primitive-set-text (fp self) 1 f-string)))

(defmethod (setf text) ((str string) (self text-primitive))
  (cffi:with-foreign-object (ptr :pointer)
    (cffi:with-foreign-pointer-as-string (f-string (1+ (length str)) size)
      (setf (cffi:mem-aref ptr :pointer) f-string)
      (lisp-string-to-foreign str f-string size)
      (rm-cffi::rm-primitive-set-text (fp self) 1 ptr))))

(defmethod (setf text) ((strs list) (self text-primitive))
  (let ((num-strs (length strs)))
    (cffi:with-foreign-object (ptr :pointer num-strs)
      (loop
	 for str in strs
	 for i from 0 below num-strs
	 do (setf (cffi:mem-aref ptr :pointer i) (foreign-string-alloc str)))
      (rm-cffi::rm-primitive-set-text (fp self) num-strs ptr)
      (loop
	 for i from 0 below num-strs
	 do (cffi:foreign-string-free (cffi:mem-aref ptr :pointer i))))
    self))

