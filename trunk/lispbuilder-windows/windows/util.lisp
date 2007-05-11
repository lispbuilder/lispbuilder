(in-package :lispbuilder-windows) 
  
(defmacro set-struct-members (object type &body values)
  (let ((elements (loop for i in values collect (car i))))
    `(with-foreign-slots (,elements ,object ,type)
       ,@(loop for (name value) in values collect `(setf ,name ,value)))))

(defmacro zero-mem (object type)
  (let ((i (gensym)))
    `(loop for ,i from 0 below (foreign-type-size (quote ,type)) do
           (setf (mem-aref ,object :char ,i) 0))))

