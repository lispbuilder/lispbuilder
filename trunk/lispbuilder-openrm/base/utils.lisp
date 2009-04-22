
(in-package #:rm-base)


;;;;;
;;;;; Type Conversion

(defun to-radian (degree)
  "converts degrees to radians."
  (* degree (/ PI 180)))


;;;;;
;;;;; Vertex/Color

(declaim (inline  make-rmcolor-3d make-rmcolor-4d make-rmvertex-3d))
(defun make-rmcolor-3d () (cffi:foreign-alloc 'rm-cffi::rm-color-3d))
(defun make-rmcolor-4d () (cffi:foreign-alloc 'rm-cffi::rm-color-4d))
(defun make-rmvertex-3d () (cffi:foreign-alloc 'rm-cffi::rm-vertex-3d))

(defun rmcolor-3d (col-r col-g col-b &key (color (make-rmcolor-3d)))
  (if (and (floatp col-r) (floatp col-g) (floatp col-b))
      (cffi:with-foreign-slots ((rm-cffi::r rm-cffi::g rm-cffi::b) color rm-cffi::RM-color-3D)
	(setf rm-cffi::r col-r
	      rm-cffi::g col-g
	      rm-cffi::b col-b))
      (error "rmcolor-3d :  r, g, b must be of type 'float"))
  color)

(defun rmcolor-4d (col-r col-g col-b col-a &key (color (make-rmcolor-4d)))
  (if (and (floatp col-r) (floatp col-g) (floatp col-b) (floatp col-a))
      (cffi:with-foreign-slots ((rm-cffi::r rm-cffi::g rm-cffi::b rm-cffi::a) color rm-cffi::RM-color-4D)
	(setf rm-cffi::r col-r
	      rm-cffi::g col-g
	      rm-cffi::b col-b
	      rm-cffi::a col-a))
      (error "rmcolor-4d :  r, g, b optional a must be of type 'float"))
  color)

(defun rmvertex-3d (vx vy vz &key (vertex (make-rmvertex-3d)))
  (if (and (floatp vx) (floatp vy) (floatp vz))
      (cffi:with-foreign-slots ((rm-cffi::x rm-cffi::y rm-cffi::z) vertex rm-cffi::RM-vertex-3D)
	(setf rm-cffi::x vx
	      rm-cffi::y vy
	      rm-cffi::z vz))
      (error "rmvertex-3d :  x, y, z must be of type 'float"))
  vertex)


(defun x-vertex-2d (vertex)
  (cffi:foreign-slot-value vertex 'rm-cffi::rm-vertex-2d 'rm-cffi::x))
(defun (setf x-vertex-2d) (val vertex)
  (setf (cffi:foreign-slot-value vertex 'rm-cffi::rm-vertex-2d 'rm-cffi::x)
	val))
(defun y-vertex-2d (vertex)
  (cffi:foreign-slot-value vertex 'rm-cffi::rm-vertex-2d 'rm-cffi::y))
(defun (setf y-vertex-2d) (val vertex)
  (setf (cffi:foreign-slot-value vertex 'rm-cffi::rm-vertex-2d 'rm-cffi::y)
	val))

(defun x-vertex-3d (vertex)
  (cffi:foreign-slot-value vertex 'rm-cffi::rm-vertex-3d 'rm-cffi::x))
(defun (setf x-vertex-3d) (val vertex)
  (setf (cffi:foreign-slot-value vertex 'rm-cffi::rm-vertex-3d 'rm-cffi::x)
	val))
(defun y-vertex-3d (vertex)
  (cffi:foreign-slot-value vertex 'rm-cffi::rm-vertex-3d 'rm-cffi::y))
(defun (setf y-vertex-3d) (val vertex)
  (setf (cffi:foreign-slot-value vertex 'rm-cffi::rm-vertex-3d 'rm-cffi::y)
	val))
(defun z-vertex-3d (vertex)
  (cffi:foreign-slot-value vertex 'rm-cffi::rm-vertex-3d 'rm-cffi::z))
(defun (setf z-vertex-3d) (val vertex)
  (setf (cffi:foreign-slot-value vertex 'rm-cffi::rm-vertex-3d 'rm-cffi::z)
	val))

(defmacro with-c3d ((var &optional (color nil) (free-p nil)) &body body)
  (let ((body-value (gensym "body-value")))
    (if color
      `(let ((,body-value nil)
             (,var ,color))
         (cffi:with-foreign-slots ((r g b) ,var rm-cffi::rm-color-3d)
           (setf ,body-value (progn ,@body))
           (when ,free-p
             (cffi:foreign-free ,var)))
         ,body-value)
      `(cffi:with-foreign-object (,var 'rm-cffi::rm-color-3d)
           (cffi:with-foreign-slots ((r g b) ,var rm-cffi::rm-color-3d)
             ,@body)))))

(defmacro with-c4d ((var &optional color free-p) &body body)
  (let ((body-value (gensym "body-value")))
    (if color
      `(let ((,body-value nil)
             (,var ,color))
         (cffi:with-foreign-slots ((r g b a) ,var rm-cffi::rm-color-4d)
           (setf ,body-value (progn ,@body)))
         (when ,free-p
             (cffi:foreign-free ,var))
         ,body-value)
      `(cffi:with-foreign-object (,var 'rm-cffi::rm-color-4d)
         (cffi:with-foreign-slots ((r g b a) ,var rm-cffi::rm-color-4d)
           ,@body)))))

(defmacro with-v2d ((var &optional (vertex nil) (free-p nil)) &body body)
  (let ((body-value (gensym "body-value")))
    (if vertex
      `(let ((,body-value nil)
             (,var ,vertex))
         (cffi:with-foreign-slots ((x y) ,var rm-cffi::rm-vertex-2d)
           (setf ,body-value (progn ,@body)))
         (when ,free-p
           (cffi:foreign-free ,var))
         ,body-value)
      `(cffi:with-foreign-object (,var 'rm-cffi::rm-vertex-2d)
         (cffi:with-foreign-slots ((x y) ,var rm-cffi::rm-vertex-2d)
           ,@body)))))

(defmacro with-v3d ((var &optional (vertex nil) (free-p nil)) &body body)
  (let ((body-value (gensym "body-value")))
    (if vertex
      `(let ((,body-value nil)
             (,var ,vertex))
         (cffi:with-foreign-slots ((x y z) ,var rm-cffi::rm-vertex-3d)
           (setf ,body-value (progn ,@body)))
         (when ,free-p
           (cffi:foreign-free ,var))
         ,body-value)
      `(cffi:with-foreign-object (,var 'rm-cffi::rm-vertex-3d)
         (cffi:with-foreign-slots ((x y z) ,var rm-cffi::rm-vertex-3d)
           ,@body)))))

(defmacro with-v4d ((var &optional (vertex nil) (free-p nil)) &body body)
  (let ((body-value (gensym "body-value")))
    (if vertex
	`(let ((,body-value nil)
	       (,var ,vertex))
           (cffi:with-foreign-slots ((x y z w) ,var rm-cffi::rm-vertex-4d)
             (setf ,body-value (progn ,@body)))
           (when ,free-p
             (cffi:foreign-free ,var))
	   ,body-value)
	`(let ((,body-value nil))
           (cffi:with-foreign-object (,var 'rm-cffi::rm-vertex-4d)
	     (cffi:with-foreign-slots ((x y z w) ,var rm-cffi::rm-vertex-4d)
	       (setf ,body-value (progn ,@body))))
	   ,body-value))))

(defun v2d+ (addto v2d)
  (incf (cffi:foreign-slot-value addto 'rm-cffi::rm-vertex-3d 'x)
        (cffi:foreign-slot-value v2d 'rm-cffi::rm-vertex-3d 'x))
  (incf (cffi:foreign-slot-value addto 'rm-cffi::rm-vertex-3d 'y)
        (cffi:foreign-slot-value v2d 'rm-cffi::rm-vertex-3d 'y))
  addto)

(defun v3d+ (addto v3d)
  (incf (cffi:foreign-slot-value addto 'rm-cffi::rm-vertex-3d 'x)
        (cffi:foreign-slot-value v3d 'rm-cffi::rm-vertex-3d 'x))
  (incf (cffi:foreign-slot-value addto 'rm-cffi::rm-vertex-3d 'y)
        (cffi:foreign-slot-value v3d 'rm-cffi::rm-vertex-3d 'y))
  (incf (cffi:foreign-slot-value addto 'rm-cffi::rm-vertex-3d 'z)
        (cffi:foreign-slot-value v3d 'rm-cffi::rm-vertex-3d 'z))
  addto)

;;;;;
;;;;; Matrix/Rotate

(defun inverse-rotation (node matrix)
  (cffi:with-foreign-object (old 'rm-cffi::rm-matrix)
    (rm-cffi::rm-Matrix-Inverse matrix matrix)
    (unless (rm-cffi::rm-Node-Get-Rotate-Matrix node old)
      (rm-cffi::rm-Matrix-Identity old))
    (rm-cffi::rm-Matrix-Multiply old matrix old)
    (rm-cffi::rm-Node-Set-Rotate-Matrix node old))
  node)

(defmacro with-matrix-symbols (matrix &body body)
  `(symbol-macrolet (,@(let ((result nil))
			    (dotimes (i 4)
			      (dotimes (j 4)
				(setf result (cons `(,(intern (string-upcase (format nil "r~D.c~D" i j)))
						      (cffi:mem-aref
						       (cffi:foreign-slot-pointer ,matrix 'rm-cffi::rm-matrix 'rm-cffi::m)
						       'rm-cffi::s-float (+ (* 4 ,i) ,j)))
						   result))))
			    result))
     ,@body))

(defun rotate-matrix (matrix direction)
  (let ((x-angle (rm-base::x-vertex-3d direction))
	(y-angle (rm-base::y-vertex-3d direction))
	(z-angle (rm-base::z-vertex-3d direction)))
    (cffi:with-foreign-object (work 'rm-cffi::rm-matrix)
      (rm-cffi::rm-matrix-identity work)
      (with-matrix-symbols work
	;;   Rotation about the X axis by an angle a:
	;;   |1       0        0    0|
	;;   |0  cos(a)  -sin(a)    0|
	;;   |0  sin(a)   cos(a)    0|
	;;   |0       0        0    1|
	
	(let ((cos (cos (to-radian x-angle)))
	      (sin (sin (to-radian x-angle))))
	  (setf r1.c1 cos
		r1.c2 (- sin)
		r2.c1 sin
		r2.c2 cos))
	(rm-cffi::rm-Matrix-Multiply matrix work matrix)
	(rm-cffi::rm-matrix-identity work))
      
      (with-matrix-symbols work
	;;   Rotation about the Y axis by an angle a:
	;;   | cos(a)  0  sin(a)    0|
	;;   |      0  1       0    0|
	;;   |-sin(a)  0  cos(a)    0|
	;;   |      0  0       0    1|
	
	(let ((cos (cos (to-radian y-angle)))
	      (sin (sin (to-radian y-angle))))
	  (setf r0.c0 cos
		r0.c2 sin
		r2.c0 (- sin)
		r2.c2 cos))
	(rm-cffi::rm-Matrix-Multiply matrix work matrix)
	(rm-cffi::rm-matrix-identity work))
      
      (with-matrix-symbols work
	;;   Rotation about the Z axis by an angle a:
	;;   |cos(a)  -sin(a)  0   0|
	;;   |sin(a)   cos(a)  0   0|
	;;   |     0        0  1   0|
	;;   |     0        0  0   1|
	
	(let ((cos (cos (to-radian z-angle)))
	      (sin (sin (to-radian z-angle))))
	  (setf r0.c0 cos
		r0.c1 (- sin)
		r1.c0 sin
		r1.c1 cos))
	(rm-cffi::rm-Matrix-Multiply matrix work matrix))))
  matrix)

(defun rotate-node (node &key (direction nil) (match-node nil) (reverse-nodes nil))
  (let ((dir (if direction direction (rm-base::rmvertex-3d 0.0 0.0 0.0))))
    (cffi:with-foreign-objects ((m 'rm-cffi::rm-matrix)
				(old 'rm-cffi::rm-matrix))
      (if match-node
	  (progn
	    (unless (rm-cffi::rm-Node-Get-Rotate-Matrix match-node old)
	      (rm-cffi::rm-Matrix-Identity old))
	    (unless (rm-cffi::rm-Node-Get-Rotate-Matrix match-node m)
	      (rm-cffi::rm-Matrix-Identity m)))
	  (progn
	    (rm-cffi::rm-matrix-identity m)
	    (rotate-matrix m dir)	;set matrix values
	    (unless (rm-cffi::rm-Node-Get-Rotate-Matrix node old)
	      (rm-cffi::rm-Matrix-Identity old))
	    (rm-cffi::rm-Matrix-Multiply old m old)))
      
      (unless direction (cffi:foreign-free dir))
      
      (rm-cffi::rm-Node-Set-Rotate-Matrix node old)

      (when reverse-nodes
	(if (listp reverse-nodes)
	    ;;Reverse the rotation for all :REVERSE-NODES if a LIST
	    (loop for node in reverse-nodes do
		 (inverse-rotation node m))
	    ;;Reverse the rotation for all child nodes if :REVERSE-NODES is T
	    (dotimes (i (rm-cffi::rm-Node-Get-Num-Children node))
	      (let ((child (rm-cffi::rm-Node-Get-Ith-Child node i)))
		(inverse-rotation child m)))))
      ))
  node)