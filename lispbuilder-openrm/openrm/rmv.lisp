
(in-package #:rm)

(defvar workv (v3d* (v3d 0.0 0.0 0.0) 12))

(defmethod build-orientation-matrix ((tp v3d))
  (let ((x (x tp)) (y (y tp)) (z (z tp)))
    (let ((a2c2 (+ (* x x) (* z z))))
      (if (= a2c2 0.0)
	  (values 1.0 0.0 0.0 0.0 0.0 1.0)
	  (let ((a2c2 (/ 1.0 a2c2)))
	    (values
	     (* a2c2 (+ (* x x y) (* z z)))
	     (* -1.0 x)
	     (* a2c2 (- (* x y z) (* x z)))
	     (* a2c2 (- (* x y z) (* x z)))
	     (* -1.0 z)
	     (* a2c2 (+ (* y z z) (* x x)))))))))

(defmethod orient-and-translate-circle ((base v3d)       ; grid point
					(vect v3d)       ; vector data
					nsides     ; of side 'nsides'
					lscale     ; vector magnitude
					proj-val   ; 0..1
					rscale	   ; radius scale
					ctable
					stable
					&optional
					(dest (v3d 0.0 0.0 0.0)) ; resultant array
					)
  (let* ((scale (* lscale (- 1.0 proj-val)))
	 (t-vect-x (+ (x base) (* (x vect) scale)))
	 (t-vect-y (+ (y base) (* (y vect) scale)))
	 (t-vect-z (+ (z base) (* (z vect) scale))))
    (multiple-value-bind (m11 m12 m13 m31 m32 m33)
	(build-orientation-matrix vect)
      (setf m11 (* m11 rscale)
	    m12 (* m12 rscale)
	    m13 (* m13 rscale)
	    m31 (* m31 rscale)
	    m32 (* m32 rscale)
	    m33 (* m33 rscale))
      (loop for i from 0 below nsides do
	   (setf (x dest) (+ (* (aref ctable i) m11)
			     (* (aref stable i) m31)
			     t-vect-x)
		 (y dest) (+ (* (aref ctable i) m12)
			     (* (aref stable i) m32)
			     t-vect-y)
		 (z dest) (+ (* (aref ctable i) m13)
			     (* (aref stable i) m32)
			     t-vect-z)))))
  vect)

(defmethod build-unit-circle (s)
  (let* ((ctable (make-array s :element-type 'float))
	 (stable (make-array s :element-type 'float)))
    (loop
       for i from 0 below s
       with dt = (/ (* 2.0 rm-cffi::+rm-pi+) s)
       with dscale = 1.0
       sum dt into tt
       do (setf (aref ctable i) (cast float (* dscale (cos tt)))
		(aref stable i) (cast float (* dscale (sin tt)))))
    (values ctable stable)))

(defmethod build-arrow (p1 	        ; The bottom center point
			p2	        ; Normalized vector direction
			mag
			cap-half-angle	; half-angle for apex
			proj-value
			ns		; number of sides per cone
			j		; the j'th cone
			ref-color
			c
			nv		; number of verts per arrow
			ctable
			stable
			&optional
			(v nil)		; the destination array for vertices
			)
  (let* ((i 0)
	 (indx (* j nv))
	 (projection-value proj-value)
	 (d2 (to-radian cap-half-angle))
	 (d (* (sin d2) mag projection-value))
	 (arrowhead (v3d 0.0 0.0 0.0)))

    (orient-and-translate-circle p1 p2 workv ns mag projection-value d
				 ctable
				 stable)
    (setf (x arrowhead) (+ (x p1) (* (x p2) mag))
	  (y arrowhead) (+ (y p1) (* (y p2) mag))
	  (z arrowhead) (+ (z p1) (* (z p2) mag)))

    (loop repeat 2
	 sum 1 into indx do
	 (progn
	   (rm-base::with-vertex-3d (vert (cffi:mem-aref (fp v) 'rm-cffi::rm-vertex-3d indx) nil)
	     (setf rm-base::x (x p1)
		   rm-base::y (y p1)
		   rm-base::z (z p1)))
	   (when c
	     (rm-base::with-color-4d (col (cffi:mem-aref (fp c) 'rm-cffi::rm-color-4d indx) nil)
	       (setf rm-base::r (r ref-color)
		     rm-base::g (g ref-color)
		     rm-base::b (b ref-color)
		     rm-base::a (a ref-color))))))

    (loop for i from 0 below ns)
    
	   )))
    
    




    ))