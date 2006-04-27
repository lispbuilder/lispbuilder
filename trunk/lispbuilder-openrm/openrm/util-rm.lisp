
;; OpenRM library using CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using OpenRM from Common lisp

(in-package #:lispbuilder-openrm)

;;; The following functions should be placed into a general 'util' package.

(defmacro copy-slots (src dest type &rest slots)
  (if (null (listp slots))
      (setf slots (cffi:foreign-slot-names src)))
  `(progn
    ,@(mapcar #'(lambda (slot)
		  `(unless (cffi:pointerp (cffi:foreign-slot-value ,src ,type ,slot))
		    (setf (cffi:foreign-slot-value ,dest ,type ,slot) (cffi:foreign-slot-value ,src ,type ,slot))))
	      slots)))

(defun is-valid-ptr (pointer)
  "IS-VALID-PTR <CFFI pointer>
  Will return T if 'pointer' is a valid <CFFI pointer> and is non-null."
  (and (cffi:pointerp pointer) (not (cffi:null-pointer-p pointer))))

;;; End.

(defun node-add-child (parent-node &rest nodes)
  (if (consp (first nodes))
      (dolist (child-node (first nodes))
	(rm::rmNodeAddChild parent-node child-node))
      (dolist (child-node nodes)
	(rm::rmNodeAddChild parent-node child-node)))
  (if (cffi:pointer-eq parent-node (rm::rmRootNode))
      (progn
	(rm::rmNodeUnionAllBoxes parent-node)
	(rm::rmNodeComputeCenterFromBoundingBox parent-node))
      (rm::rmNodeComputeCenterFromBoundingBox parent-node))
  parent-node)

(cffi:define-foreign-type :handle () `(:pointer :void))

(defun make-rmcolor-3d () (cffi:foreign-alloc 'rm::rmcolor3d))
(defun make-rmcolor-4d () (cffi:foreign-alloc 'rm::rmcolor4d))
(defun make-rmvertex-3d () (cffi:foreign-alloc 'rm::rmvertex3d))

(defun rmcolor-3d (col-r col-g col-b &key (color (rm::make-rmcolor-3d)))
  (if (and (floatp col-r) (floatp col-g) (floatp col-b))
      (cffi:with-foreign-slots ((rm::r rm::g rm::b) color rm::RMcolor3D)
	(setf rm::r col-r
	      rm::g col-g
	      rm::b col-b))
      (error "rmcolor-3d :  r, g, b must be of type 'float"))
  color)

(defun rmcolor-4d (col-r col-g col-b &optional (col-a 1.0) &key (color (rm::make-rmcolor-4d)))
  (if (and (floatp col-r) (floatp col-g) (floatp col-b) (floatp col-a))
      (cffi:with-foreign-slots ((rm::r rm::g rm::b rm::a) color rm::RMcolor4D)
	(setf rm::r col-r
	      rm::g col-g
	      rm::b col-b
	      rm::a col-a))
      (error "rmcolor-4d :  r, g, b optional a must be of type 'float"))
  color)

(defun rmvertex-3d (vx vy vz &key (vertex (rm::make-rmvertex-3d)))
  (if (and (floatp vx) (floatp vy) (floatp vz))
      (cffi:with-foreign-slots ((rm::x rm::y rm::z) vertex rm::RMvertex3D)
	(setf rm::x vx
	      rm::y vy
	      rm::z vz))
      (error "rmvertex-3d :  x, y, z must be of type 'float"))
  vertex)

;; (defun rmvertex-3d+ (x y z &key (vertex (make-rmvertex-3d)))
;;   (if (and (typep x 'float) (typep y 'float) (typep z 'float))
;;       (fli:with-foreign-slots (rm::x rm::y rm::z) vertex
;; 	(setf rm::x (+ rm::x x) rm::y (+ rm::y y) rm::z (+ rm::z z)))
;;       (error "rmvertex-3d+ :  x, y, z must be of type 'float"))
;;   vertex)

(defmacro with-rmcamera-3d (camera &body body)
  `(let ((,camera (rm::rmcamera3dnew)))
    ,@body
    (rm::rmcamera3ddelete ,camera)))

(defmacro with-rmcolor-3d (color &body body)
  (let ((r (gensym)) (g (gensym)) (b (gensym)))
    `(cffi:with-foreign-object (,color 'rm::rmcolor3d)
      (labels ((set-color (,r ,g ,b)
		 (if (and (floatp ,r) (floatp ,g) (floatp ,b))
		     (cffi:with-foreign-slots ((rm::r rm::g rm::b) ,color rm::rmcolor3d)
		       (setf rm::r ,r
			     rm::g ,g
			     rm::b ,b))
		     (error "with-rmcolor-3d.set-color (r g b) must be of types 'float."))))
	,@body))))

(defmacro with-rmcolor-4d (color &body body)
  (let ((r (gensym)) (g (gensym)) (b (gensym)) (a (gensym)))
    `(cffi:with-foreign-object (,color 'rm::rmcolor4d)
      (labels ((set-color (,r ,g ,b &optional (,a 1.0))
		 (if (and (floatp ,r) (floatp ,g) (floatp ,b) (floatp ,a))
		     (cffi:with-foreign-slots ((rm::r rm::g rm::b rm::a) ,color rm::rmcolor4d)
			 (setf rm::r ,r
			       rm::g ,g
			       rm::b ,b
			       rm::a ,a))
		       (error "with-rmcolor-4d.set-color (r g b a) must be of types 'float."))))
	  ,@body))))

(defmacro with-rmvertex-3d (vertex &body body)
  (let ((x (gensym)) (y (gensym)) (z (gensym)))
    `(cffi:with-foreign-object (,vertex 'rm::rmvertex3d)
      (labels ((set-vertex (,x ,y ,z)
		 (if (and (floatp ,x) (floatp ,y) (floatp ,z))
		     (cffi:with-foreign-slots ((rm::x rm::y rm::z) ,vertex rm::rmvertex3d)
			 (setf rm::x ,x
			       rm::y ,y
			       rm::z ,z))
		       (error "with-rmvertex-3d.set-vertex (x y z) must be of types 'float."))))
	  ,@body))))

(defmacro with-rmvertex-3d+ ((&body vertices) &body body)
  (let ((x (gensym)) (y (gensym)) (z (gensym)))
    `(cffi:with-foreign-objects (,@(mapcar #'(lambda (vertex)
						`(,vertex 'rm::rmvertex3d))
					    vertices))
      (labels ((set-vertex (vertex ,x ,y ,z)
		 (if (and (floatp ,x) (floatp ,y) (floatp ,z))
		     (cffi:with-foreign-slots ((rm::x rm::y rm::z) vertex rm::rmvertex3d)
		       (setf rm::x ,x
			     rm::y ,y
			     rm::z ,z))
		     (error "with-rmvertex-3d+.set-vertex (x y z) must be of types 'float."))))
	,@(mapcar #'(lambda (vertex)
		      `(rm::rmvertex-3d 0.0 0.0 0.0 :vertex ,vertex))
		  vertices)
	  ,@body))))

(defmacro with-light (light &body body)
  `(let ((,light (rm::rmLightNew)))
    (labels ((set-light-position (vert)
	       (rm::LightSetXYZ ,light vert))
	     (set-abient-color (color)
	       (rm::LightSetColor ,light color nil nil))
	     (set-diffuse-color (color)
	       (rm::LightSetColor ,light nil color nil))
	     (set-specular-color (color)
	       (rm::LightSetColor ,light nil nil color))
	     (set-type (type)
	       (setf type (case type
			    (:point (cffi:foreign-enum-value 'rm::rmenum :rm_light_point))
			    (:directional (cffi:foreign-enum-value 'rm::rmenum :rm_light_directional))
			    (:spot (cffi:foreign-enum-value 'rm::rmenum :rm_light_spot))))
	       (rm::rmLightSetType ,light type))
	     (set-scene (node type)
	       (setf type (case type
			    (:light0 (cffi:foreign-enum-value 'rm::rmenum :rm_light0))
			    (:light1 (cffi:foreign-enum-value 'rm::rmenum :rm_light1))
			    (:light2 (cffi:foreign-enum-value 'rm::rmenum :rm_light2))
			    (:light3 (cffi:foreign-enum-value 'rm::rmenum :rm_light3))
			    (:light4 (cffi:foreign-enum-value 'rm::rmenum :rm_light4))
			    (:light5 (cffi:foreign-enum-value 'rm::rmenum :rm_light5))
			    (:light6 (cffi:foreign-enum-value 'rm::rmenum :rm_light6))
			    (:light7 (cffi:foreign-enum-value 'rm::rmenum :rm_light7))))
	       (rm::rmNodeSetSceneLight node type ,light)))
      ,@body
      (rm::rmLightDelete ,light))))

(defun build-XZ-Quad-mesh (p vmin vmax subdivisions ysign color)
  "Build a quadmesh parallel to the X-Z plane."
  (let ((ref-normal (vector 0.0 (* 1.0 ysign) 0.0))
	(w (vector (vertex-x vmin) (vertex-y vmin) (vertex-z vmin)))
	(dx (/ (- (vertex-x vmax) (vertex-x vmin))
	       (- subdivisions 1)))
	(dz (/ (- (vertex-z vmax) (vertex-z vmin))
	       (- subdivisions 1)))
	(v nil) (n nil) (c nil))
    
    (rm::rmPrimitiveSetQmeshDims p subdivisions subdivisions)
    
    (setf v (loop for i from 1 upto subdivisions
		  for w-z = (vertex-z w) then (+ w-z dz)
		  append (loop for j from 1 upto subdivisions
				for w-x = (vertex-x w) then (+ w-x dx)
				collect (vector w-x (vertex-y w) w-z))))

    (setf c (loop repeat (* subdivisions subdivisions)
		  collect (vector (color-r color) (color-g color) (color-b color) (color-a color))))

    (setf n (loop repeat (* subdivisions subdivisions)
		  collect ref-normal))

    
    (set-primitive-vertices p v)
    (set-primitive-normals p n)
    (set-primitive-colors p c)
    )
  p)


;; Dolly Functions
(let ((prev-buttony 0.0))
  (defun dolly (node screen-width screen-height mousey)
    (cffi:with-foreign-object (h-pointer :pointer)
      (let ((buttony (* -1.0 (rm::pixel-to-viewport mousey screen-height))))
	(when (equal (rm::rmnodegetscenecamera3d node h-pointer)
		     (cffi:foreign-enum-value 'rm::rmenum :rm_true))
	  (rm::auxdolly (cffi:mem-aref (cffi:mem-aref h-pointer :pointer) 'rm::rmcamera3d)
			0.0 prev-buttony 0.0 buttony)
	  (rm::rmNodeSetSceneCamera3D node (cffi:mem-aref (cffi:mem-aref h-pointer :pointer) 'rm::rmcamera3d))
	  (rm::rmCamera3DDelete (cffi:mem-aref (cffi:mem-aref h-pointer :pointer) 'rm::rmcamera3d))
	  (setf prev-buttony buttony)))))
  (defun reset-dolly (screen-width screen-height y)
    (setf prev-buttony (* -1.0 (rm::pixel-to-viewport y screen-height)))))

;; Arc Functions
(let ((prev-buttonx 0.0)
      (prev-buttony 0.0)
      (initial-transform (cffi:foreign-alloc 'rm::RMmatrix))
      (result-transform (cffi:foreign-alloc 'rm::RMmatrix)))
  (defun arc (node screen-width screen-height mousex mousey)
    (let ((buttonx (rm::pixel-to-viewport mousex screen-width))
	  (buttony (* -1.0 (rm::pixel-to-viewport mousey screen-height))))
      (rm::auxarcball prev-buttonX prev-buttonY buttonx buttony result-transform)
      (rm::rmMatrixMultiply initial-transform result-transform result-transform)
      (rm::rmNodeSetRotateMatrix node result-transform)))
  (defun reset-arc (node screen-width screen-height x y)
    (setf prev-buttonx (rm::pixel-to-viewport x screen-width)
	  prev-buttony (* -1.0 (rm::pixel-to-viewport y screen-height)))
    (if (equal (rm::rmNodeGetRotateMatrix node initial-transform)
	       (cffi:foreign-enum-value 'rm::rmenum :rm_whacked))
	(rm::rmMatrixIdentity initial-transform))))

;; Translate Functions
(let ((prev-buttonx 0.0)
      (prev-buttony 0.0))
  (defun translate (node screen-width screen-height mousex mousey)
    (cffi:with-foreign-object (h-pointer :pointer)
      (let ((buttonx (rm::pixel-to-viewport mousex screen-width))
	    (buttony (* -1.0 (rm::pixel-to-viewport mousey screen-height))))
	(when (equal (rm::rmnodegetscenecamera3d node h-pointer)
		     (cffi:foreign-enum-value 'rm::rmenum :rm_true))
	  (rm::auxtranslate (cffi:mem-aref (cffi:mem-aref h-pointer :pointer) 'rm::rmcamera3d)
			    prev-buttonx prev-buttony buttonx buttony)
	  (rm::rmNodeSetSceneCamera3D node (cffi:mem-aref (cffi:mem-aref h-pointer :pointer) 'rm::rmcamera3d))
	  (rm::rmCamera3DDelete (cffi:mem-aref (cffi:mem-aref h-pointer :pointer) 'rm::rmcamera3d))
	  (setf prev-buttonx buttonx
		prev-buttony buttony)))))
  (defun reset-translate (screen-width screen-height x y)
    (setf prev-buttonx (rm::pixel-to-viewport x screen-width)
	  prev-buttony (* -1.0 (rm::pixel-to-viewport y screen-height)))))

(defun move-node-by (node vert)
  (with-rmvertex-3d m
    (when (equal (cffi:foreign-enum-value 'rm::rmenum :RM_WHACKED)
		 (rm::rmNodeGetTranslateVector node m))
      (copy-to-foreign-vertex (vertex 0.0 0.0 0.0) m))
    (rm::rmNodeSetTranslateVector node (add-to-foreign-vertex vert m)))
  node)
  
(defun move-node-to (node vert)
  (rm::set-node-position node vert)
  node)

;;; Code below is verified as correct.

(defun to-radian (degree)
  "converts degrees to radians."
  (* degree (/ PI 180)))

(defun to-degree (radian)
  "converts radians to degrees."
  (/ radian (/ PI 180)))

(defun single-float (num)
  (coerce num 'single-float))

(defmacro with-matrix-symbols (matrix &body body)
  `(symbol-macrolet (,@(let ((result nil))
			    (dotimes (i 4)
			      (dotimes (j 4)
				(setf result (cons `(,(intern (string-upcase (format nil "r~D.c~D" i j)))
						     (cffi:mem-aref
						      (cffi:foreign-slot-pointer ,matrix 'rm::rmmatrix 'rm::m)
						      :float (+ (* 4 ,i) ,j)))
						   result))))
			    result))
    ,@body))

(defun rotate-matrix (matrix x-angle y-angle z-angle)
  (if (not (and (floatp x-angle) (floatp y-angle) (floatp z-angle)))
      (error "rotate-matrix :  x-angle, y-angle, z-angle must be of type 'float")
      (cffi:with-foreign-object (work 'rm::rmmatrix)
	(rm::rmmatrixidentity work)
	(with-matrix-symbols work
	  ;;   Rotation about the X axis by an angle a:
	  ;;   |1       0        0    0|
	  ;;   |0  cos(a)  -sin(a)    0|
	  ;;   |0  sin(a)   cos(a)    0|
	  ;;   |0       0        0    1|
	  
	  (let ((cos (single-float (cos (to-radian x-angle))))
		(sin (single-float (sin (to-radian x-angle)))))
	    (setf r1.c1 cos
		  r1.c2 (- sin)
		  r2.c1 sin
		  r2.c2 cos))
	  (rm::rmMatrixMultiply matrix work matrix)
	  (rm::rmmatrixidentity work))

	(with-matrix-symbols work
	  ;;   Rotation about the Y axis by an angle a:
	  ;;   | cos(a)  0  sin(a)    0|
	  ;;   |      0  1       0    0|
	  ;;   |-sin(a)  0  cos(a)    0|
	  ;;   |      0  0       0    1|
	  
	  (let ((cos (single-float (cos (to-radian y-angle))))
		(sin (single-float (sin (to-radian y-angle)))))
	    (setf r0.c0 cos
		  r0.c2 sin
		  r2.c0 (- sin)
		  r2.c2 cos))
	  (rm::rmMatrixMultiply matrix work matrix)
	  (rm::rmmatrixidentity work))

	(with-matrix-symbols work
	  ;;   Rotation about the Z axis by an angle a:
	  ;;   |cos(a)  -sin(a)  0   0|
	  ;;   |sin(a)   cos(a)  0   0|
	  ;;   |     0        0  1   0|
	  ;;   |     0        0  0   1|
	  
	  (let ((cos (single-float (cos (to-radian z-angle))))
		(sin (single-float (sin (to-radian z-angle)))))
	    (setf r0.c0 cos
		  r0.c1 (- sin)
		  r1.c0 sin
		  r1.c1 cos))
	  (rm::rmMatrixMultiply matrix work matrix))))
  matrix)

(defmacro with-init (args &body body)
  "Attempts to initialize RM using the flags init-flags. 
    Automatically shuts down SDL using SDL_Quit if unsuccessful.
    Test for failure using the function INIT-SUCCESS.
        INIT-SUCCESS returns:
            T if with-init is successful, 
            NIL for failure."
  `(block nil
    (unwind-protect
	 (progn
	   (rm::rmInit)
	   ,@body)
      ;(rm::rm_finish)
      )))


;;#define pixeltovp(p,d) ((float)((p) - ((d) >> 1)) / (float)((d) >> 1))
(defun pixel-to-viewport (pixel dimension)
  (coerce (/ (- pixel (ash dimension -1))
	     (ash dimension -1))
	  'single-float))

(defmacro error-if-not-chill (&body body)
  `(if (equal ,(cffi:foreign-enum-value 'rm::rmenum :rm_whacked)
	,@body)
    (error (format nil "ERROR: ~A returned :rm_whacked~%" ',(first body)))))

;;; Wrapper functions here


(defun vertex-x (vertex)
  (elt vertex 0))
(defun (setf vertex-x) (x-val vertex)
  (setf (elt vertex 0) x-val))

(defun vertex-y (vertex)
  (elt vertex 1))
(defun (setf vertex-y) (y-val vertex)
  (setf (elt vertex 1) y-val))

(defun vertex-z (vertex)
  (elt vertex 2))
(defun (setf vertex-z) (z-val vertex)
  (setf (elt vertex 2) z-val))

(defun color-r (color)
  (elt color 0))
(defun (setf color-r) (r-val color)
  (setf (elt color 0) r-val))

(defun color-g (color)
  (elt color 1))
(defun (setf color-g) (g-val color)
  (setf (elt color 1) g-val))

(defun color-b (color)
  (elt color 2))
(defun (setf color-b) (b-val color)
  (setf (elt color 2) b-val))

(defun color-a (color)
  (elt color 3))
(defun (setf color-a) (a-val color)
  (setf (elt color 3) a-val))



(defclass engine-object ()
  ((id :accessor id :initform (get-id) :initarg :id)
   (func :accessor func :initform (make-instance 'hold) :initarg :action)
   (update :accessor is-updated :initform t)   
   (visible :accessor visible :initform t :initarg :visible)
   (graph-node :accessor graph-node :initform nil :initarg :graph-node)
   (parent-node :accessor parent-node :initform nil :initarg :parent-node)))

(defclass actor (engine-object)
  ((col :accessor col :initform (color 0.0 0.0 0.0) :initarg :col)
   (pos :accessor pos :initform (vertex 0.0 0.0 0.0) :initarg :pos)
   (rot-x :accessor rot-x :initform 0.0 :initarg :rot-x)
   (rot-y :accessor rot-y :initform 0.0 :initarg :rot-y)
   (rot-z :accessor rot-z :initform 0.0 :initarg :rot-z)
   (geom-id :accessor geom-id :initform nil :initarg :geom-id)
   (body-id :accessor body-id :initform nil :initarg :body-id)
   (mass :accessor mass :initform 1.0 :initarg :mass)
   (density :accessor density :initform 1.0 :initarg :density)
   (m :accessor m :initform nil :initarg :m)))

(defclass cube-actor (actor)
  ((vertices :accessor vertices :initform (list (vertex -1.0 -1.0 -1.0) (vertex 1.0 1.0 1.0)) :initarg :vertices)))

(defun copy-to-foreign-vertex (src dst)
  (cffi:with-foreign-slots ((rm::x rm::y rm::z) dst rm::rmvertex3d)
    (setf rm::x (elt src 0)
	  rm::y (elt src 1)
	  rm::z (elt src 2)))
  dst)

(defun copy-to-foreign-color (src dst)
  (if (eq (length src) 4)
      (cffi:with-foreign-slots ((rm::r rm::g rm::b rm::a) dst rm::rmcolor4d)
	(setf rm::r (elt src 0)
	      rm::g (elt src 1)
	      rm::b (elt src 2)
	      rm::a (elt src 3)))
      (cffi:with-foreign-slots ((rm::r rm::g rm::b) dst rm::rmcolor3d)
	(setf rm::r (elt src 0)
	      rm::g (elt src 1)
	      rm::b (elt src 2))))
  dst)

(defun add-to-foreign-vertex (src dst)
  (cffi:with-foreign-slots ((rm::x rm::y rm::z) dst rm::rmvertex3d)
    (incf rm::x (vertex-x src))
    (incf rm::y (vertex-y src))
    (incf rm::z (vertex-z src)))
  dst)

(defun set-primitive-vertices (primitive vertices)
  (let ((vertices (create-list-if-not vertices)))
    (rm::error-if-not-chill (rm::PrimitiveSetVertex3D primitive
						      (length vertices)
						      vertices
						      :RM_COPY_DATA
						      (cffi:null-pointer))))
  primitive)

(defun set-primitive-normals (primitive normals)
  (let ((normals (create-list-if-not normals)))
    (rm::error-if-not-chill (rm::PrimitiveSetNormal3D primitive
						      (length normals)
						      normals
						      :RM_COPY_DATA
						      (cffi:null-pointer))))
  primitive)


(defun set-primitive-colors (primitive colors)
  (let ((colors (create-list-if-not colors)))
    (if (> (length (first colors)) 3)
	(rm::error-if-not-chill (rm::PrimitiveSetColor4D primitive
							 (length colors)
							 colors
							 :RM_COPY_DATA
							 (cffi:null-pointer)))
	(rm::error-if-not-chill (rm::PrimitiveSetColor3D primitive
							 (length colors)
							 colors
							 :RM_COPY_DATA
							 (cffi:null-pointer)))))
  primitive)

(defun set-primitive-position (primitive vertex)
  (let ((vertex (create-list-if-not vertex)))
    (rm::error-if-not-chill (rm::PrimitiveSetVertex3D primitive
						      (length vertex)
						      vertex
						      :RM_COPY_DATA
						      (cffi:null-pointer))))
  primitive)

;; (defun set-node-position (node vertex)
;;   (with-rmvertex-3d position
;;     (copy-to-foreign-vertex vertex position)
;;     (rm::error-if-not-chill (rm::rmNodeSetTranslateVector node position)))
;;   node)

(defun new-cube-primitive (&key (vertices nil)
			   (dimensions #(0.0 0.0 0.0))
			   (color #(0.0 0.0 0.0))
			   (position #(0.0 0.0 0.0)))
  (let ((primitive (rm::PrimitiveNew :rm_box3d)))
    (if vertices
	(set-primitive-vertices primitive vertices)
	(if dimensions
	    (set-primitive-vertices primitive (list position
						    (vector (+ (elt dimensions 0) (elt position 0))
							    (+ (elt dimensions 1) (elt position 1))
							    (+ (elt dimensions 2) (elt position 2)))))))
    (if color
      (set-primitive-colors primitive color))
    (rm::rmPrimitiveComputeBoundingBox primitive)
    primitive))

(defun set-primitive-radius (primitive radius)
  (let ((radius (create-list-if-not radius)))
    (rm::error-if-not-chill (rm::PrimitiveSetRadii primitive
						   (length radius)
						   radius
						   :RM_COPY_DATA
						   (cffi:null-pointer))))
  primitive)

(defun new-sphere-primitive (&key (radius 1.0) (tesselate 32) (color #(0.0 0.0 0.0 0.0))
			     (position #(0.0 0.0 0.0)))
  (let ((primitive (rm::PrimitiveNew :rm_spheres))
	(tesselate (case tesselate
		     (8 rm::RM_SPHERES_8)
		     (32 rm::RM_SPHERES_32)
		     (128 rm::RM_SPHERES_128)
		     (512 rm::RM_SPHERES_512)
		     (otherwise rm::RM_SPHERES_32))))
    (set-primitive-radius primitive radius)
    (rm::error-if-not-chill (rm::rmPrimitiveSetModelFlag primitive tesselate))
    (set-primitive-colors primitive color)
    (set-primitive-position primitive position)
    (rm::rmPrimitiveComputeBoundingBox primitive)
    primitive))

(defun new-plane-primitive (&key vertices (color #(1.0 1.0 1.0)) (orientation :xz) (subdivisions 20) (sign 1))
  (let ((primitive (rm::primitivenew :rm_quadmesh))
	(vmin (first vertices))
	(vmax (second vertices)))
    (case orientation
      (:xz (build-xz-quad-mesh primitive vmin vmax subdivisions sign color))
      (otherwise (build-xz-quad-mesh primitive vmin vmax subdivisions sign color)))
    (rm::rmPrimitiveComputeBoundingBox primitive)
    primitive))

;; (defun foreign-array-from-vertex (&rest vertices)
;;   (let ((vertex-array (rm::rmVertex3DNew (length vertices))))
;;     (vertex-copy vertices vertex-array)
;;     vertex-array))

(defun vector-from-rmvertex (rmvertex)
  (cffi:with-foreign-slots ((rm::x rm::y rm::z) rmvertex rm::rmvertex3d)
    (vector rm::x rm::y rm::z)))

(defun get-primitive-bounding-box (primitive)
  (let ((vertices nil))
    (with-rmvertex-3d+ (vmin vmax)
      (rm::rmPrimitivegetBoundingBox primitive vmin vmax)
      (setf vertices (list (vector-from-rmvertex vmin)
			   (vector-from-rmvertex vmax))))
    vertices))

(defun compute-bounds-from-primitive (primitive)
  (let ((bbox (rm::primitivenew :RM_BOX3D_WIRE))
	(vertices (get-primitive-bounding-box primitive)))
      (rm::error-if-not-chill (rm::PrimitiveSetVertex3D bbox
							(length vertices)
							vertices
							:RM_COPY_DATA
							(cffi:null-pointer)))
      (rm::rmPrimitiveComputeBoundingBox bbox)
      bbox))

(defun get-node-bounding-box (node)
  (let ((vertices nil))
    (with-rmvertex-3d+ (vmin vmax)
      (rm::rmNodegetBoundingBox node vmin vmax)
      (setf vertices (list (vector-from-rmvertex vmin)
			   (vector-from-rmvertex vmax))))
    vertices))

(defun create-bounds-from-node (node &key primitives)
  (if primitives
      (let ((num (rm::rmNodeGetNumPrims node)))
	(loop for i from 0 to (- num 1)
	      collect (rm::compute-bounds-from-primitive (rm::rmNodeGetPrimitive node i))))
      (progn
	(let ((bbox (rm::primitivenew :RM_BOX3D_WIRE))
	      (vertices (get-node-bounding-box node)))
	  (rm::error-if-not-chill (rm::PrimitiveSetVertex3D bbox
							    (length vertices)
							    vertices
							    :RM_COPY_DATA
							    (cffi:null-pointer)))
	  bbox))))

(defun create-rm-pipe (&key
		       (target-platform :RM_PIPE_NOPLATFORM)
		       (processing-mode :RM_PIPE_MULTISTAGE)
		       (render-opaque3D t)
		       (render-transparent3D nil)
		       (render-all2D nil))
  (let ((target-platform (case target-platform
			   (:RM_PIPE_NOPLATFORM (cffi:foreign-enum-value 'rm::rmenum :RM_PIPE_NOPLATFORM))
			   (:RM_PIPE_GLX (cffi:foreign-enum-value 'rm::rmenum :RM_PIPE_GLX))
			   (:RM_PIPE_WGL (cffi:foreign-enum-value 'rm::rmenum :RM_PIPE_WGL))
			   (:RM_PIPE_CR (cffi:foreign-enum-value 'rm::rmenum :RM_PIPE_CR))
			   (otherwise (cffi:foreign-enum-value 'rm::rmenum :RM_PIPE_NOPLATFORM))))
	(processing-mode (case processing-mode
			   (:RM_PIPE_MULTISTAGE (cffi:foreign-enum-value 'rm::rmenum :RM_PIPE_MULTISTAGE))
			   (:RM_PIPE_MULTISTAGE_PARALLEL
			    (cffi:foreign-enum-value 'rm::rmenum :RM_PIPE_MULTISTAGE_PARALLEL))
			   (:RM_PIPE_MULTISTAGE_VIEW_PARALLEL
			    (cffi:foreign-enum-value 'rm::rmenum :RM_PIPE_MULTISTAGE_VIEW_PARALLEL))
			   (otherwise (cffi:foreign-enum-value 'rm::rmenum :RM_PIPE_MULTISTAGE))))
	(render-opaque3D (if render-opaque3D
			     (cffi:foreign-enum-value 'rm::rmenum :RM_TRUE)
			     (cffi:foreign-enum-value 'rm::rmenum :RM_FALSE)))
	(render-transparent3D (if render-transparent3D
				(cffi:foreign-enum-value 'rm::rmenum :RM_TRUE)
				(cffi:foreign-enum-value 'rm::rmenum :RM_FALSE)))
	(render-all2D (if render-all2D
			  (cffi:foreign-enum-value 'rm::rmenum :RM_TRUE)
			  (cffi:foreign-enum-value 'rm::rmenum :RM_FALSE))))
    (let ((pipe (rm::rmPipeNew target-platform)))
      (rm::rmPipeSetRenderPassEnable pipe render-opaque3D render-transparent3D render-all2D)
      (rm::rmPipeSetProcessingMode pipe processing-mode)
    pipe)))

(defun set-default-scene (look-node width height)
  (rm::rmNodeUnionAllBoxes look-node)
  (rm::rmNodeComputeCenterFromBoundingBox look-node)
    
  (rm::NodeSetSceneBackgroundColor look-node #(0.2 0.1 0.2 0.0))

  (with-rmcamera-3d camera
    (rm::rmDefaultCamera3D camera)
    (rm::rmCamera3DComputeViewFromGeometry camera look-node width height)
    (rm::rmNodeSetSceneCamera3D look-node camera))
    
  (rm::rmDefaultLighting look-node)
  look-node)

(defun new-node (&key (name "") (dims :3d) (opacity :all))
  (setf dims (case dims
	       (:3d (cffi:foreign-enum-value 'rm::rmenum :rm_renderpass_3d))
	       (:2d (cffi:foreign-enum-value 'rm::rmenum :rm_renderpass_2d))
	       (:all (cffi:foreign-enum-value 'rm::rmenum :rm_renderpass_all))
	       (otherwise (cffi:foreign-enum-value 'rm::rmenum :rm_renderpass_3d))))
  (setf opacity (case opacity
		  (:opaque (cffi:foreign-enum-value 'rm::rmenum :rm_renderpass_opaque))
		  (:transparent (cffi:foreign-enum-value 'rm::rmenum :rm_renderpass_transparent))
		  (:all (cffi:foreign-enum-value 'rm::rmenum :rm_renderpass_all))
		  (otherwise (cffi:foreign-enum-value 'rm::rmenum :rm_renderpass_all))))
  (rm::rmNodeNew name dims opacity))

(defun node-add-primitive (node &rest primitives)
  (if (consp (first primitives))
      (dolist (primitive (first primitives))
	(rm::error-if-not-chill (rm::rmNodeAddPrimitive node primitive)))
      (dolist (primitive primitives)
	(rm::error-if-not-chill (rm::rmNodeAddPrimitive node primitive))))
  (rm::error-if-not-chill (rm::rmNodeComputeBoundingBox node))
  (rm::error-if-not-chill (rm::rmNodeComputeCenterFromBoundingBox node))
  node)

(defun inverse-rotation (node matrix)
  (cffi:with-foreign-objects ((old 'rm::rmmatrix))
    (rm::rmMatrixInverse matrix matrix)
    (when (equal (cffi:foreign-enum-value 'rm::rmenum :RM_WHACKED)
		 (rm::rmNodeGetRotateMatrix node old))
      (rm::rmMatrixIdentity old))
    (rm::rmMatrixMultiply old matrix old)
    (rm::rmNodeSetRotateMatrix node old))
  node)

(defun rotate-node (node x y z &key (only-this-node nil))
  (cffi:with-foreign-objects ((m 'rm::rmmatrix)
			      (old 'rm::rmmatrix))
    (rm::rmmatrixidentity m)
    (rotate-matrix m x y z) ;set matrix values
    (when (equal (cffi:foreign-enum-value 'rm::rmenum :RM_WHACKED)
		 (rm::rmNodeGetRotateMatrix node old))
      (rm::rmMatrixIdentity old))
    (rm::rmMatrixMultiply old m old)
    (rm::rmNodeSetRotateMatrix node old)

    ;;Reverse the rotation for all child nodes if :only-this-node is T
    (if only-this-node
	(progn
	  (rm::rmMatrixInverse m m)
	  (dotimes (i (rm::rmNodeGetNumChildren node))
	    (let ((child (rm::rmNodeGetIthChild node i)))
	      (inverse-rotation child m))))))
  node)
    
(defun add-node (parent-node child-node &key (union nil) (compute-center nil))
;;   (if (listp child-node)
;;       (dolist (child child-node)
;; 	(add-node parent-node child)))
  (rm::error-if-not-chill (rm::rmNodeAddChild parent-node child-node))
  (if union
      (rm::error-if-not-chill (rm::rmNodeUnionAllBoxes parent-node)))
  (if compute-center
      (rm::error-if-not-chill (rm::rmNodeComputeCenterFromBoundingBox parent-node)))
  parent-node)


(defmacro with-rmpipe ((hwnd width height) pipe &body body)
  `(block nil
    (let ((,pipe (rm::create-rm-pipe)))
      (if (not (equal ,pipe (cffi:null-pointer)))
	  (unwind-protect
	       (progn
		 (rm::rmPipeSetSwapbuffersFunc ,pipe (cffi:null-pointer))
		 (rm::rmPipeSetWindow ,pipe ,hwnd ,width ,height)
		 (rm::rmPipeMakeCurrent ,pipe) ;Make current only after the OpenGL context is created.
		 ,@body)
	    (rm::rmPipeDelete ,pipe))
	  (error "ERROR: with-rmpipe cannot create pipe")))))



