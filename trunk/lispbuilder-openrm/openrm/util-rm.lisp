
;; OpenRM library using CFFI for foreign function interfacing...
;; OpenRM 1.6.0-2
;; (C)2006 Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using OpenRM from Common lisp

(in-package #:lispbuilder-openrm)


(defvar *default-light* nil)
(defvar *default-camera* nil)
(defvar *default-color* nil)


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

;;; w

(defmacro with-rmcamera-3d ((camera-ptr &optional (free-p t)) &body body)
  "Create a new light using (rm:rmcamera3dnew)"
  (let ((body-value (gensym "body-value")))
    `(let ((*default-camera* ,camera-ptr)
	   (,body-value nil))
      (when (is-valid-ptr *default-camera*)
	(setf ,body-value (progn ,@body))
	(if (and ,free-p (is-valid-ptr *default-camera*))
	    (rmcamera3ddelete *default-camera*)))
      ,body-value)))

(defmacro with-rmcolor-3d ((color-ptr &optional (free-p t)) &body body)
  "Create a new color using (rm:rmcolor-3d)"
  (let ((body-value (gensym "body-value")))
    `(let ((*default-color* ,color-ptr)
	   (,body-value nil))
      (when (is-valid-ptr *default-color*)
	(setf ,body-value (progn ,@body))
	(if (and ,free-p (is-valid-ptr *default-color*))
	    (cffi:foreign-free *default-color*)))
      ,body-value)))

(defmacro with-rmcolor-4d ((color-ptr &optional (free-p t)) &body body)
  "Create a new color using (rm:rmcolor-4d)"
  (let ((body-value (gensym "body-value")))
    `(let ((*default-color* ,color-ptr)
	   (,body-value nil))
      (when (is-valid-ptr *default-color*)
	(setf ,body-value (progn ,@body))
	(if (and ,free-p (is-valid-ptr *default-color*))
	    (cffi:foreign-free *default-color*)))
      ,body-value)))

(defmacro with-light ((light-ptr &optional (free-p t)) &body body)
  "Create a new light using (rm:rmLightNew)"
  (let ((body-value (gensym "body-value")))
    `(let ((*default-light* ,light-ptr)
	   (,body-value nil))
      (when (is-valid-ptr *default-light*)
	(setf ,body-value (progn ,@body))
	(if (and ,free-p (is-valid-ptr *default-light*))
	    (rmLightDelete *default-light*)))
      ,body-value)))

(defmacro with-rmvertex-3d ((vertex-ptr &optional (free-p t)) &body body)
  "Create a new color using (rm:rmvertex-3d)"
  (let ((body-value (gensym "body-value")))
    `(let ((*default-vertex* ,vertex-ptr)
	   (,body-value nil))
      (when (is-valid-ptr *default-vertex*)
	(setf ,body-value (progn ,@body))
	(if (and ,free-p (is-valid-ptr *default-vertex*))
	    (cffi:foreign-free *default-vertex*)))
      ,body-value)))

;;; End.

(let ((id 0))
  (defun get-id ()
    (incf id)))

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


;;; Lighting

(defun set-light-position (vert &key (light *default-light*))
  (LightSetXYZ light vert))

(defun set-light-abient-color (color &key (light *default-light*))
  (LightSetColor light color nil nil))

(defun set-light-diffuse-color (color &key (light *default-light*))
  (LightSetColor light nil color nil))

(defun set-light-specular-color (color &key (light *default-light*))
  (LightSetColor light nil nil color))

(defun set-light-type (type &key (light *default-light*))
  (setf type (case type
	       (:point (cffi:foreign-enum-value 'rm::rmenum :rm_light_point))
	       (:directional (cffi:foreign-enum-value 'rm::rmenum :rm_light_directional))
	       (:spot (cffi:foreign-enum-value 'rm::rmenum :rm_light_spot))))
  (rmLightSetType light type))

(defun set-scene-light (node type &key (light *default-light*))
  (setf type (case type
	       (:light0 (cffi:foreign-enum-value 'rm::rmenum :rm_light0))
	       (:light1 (cffi:foreign-enum-value 'rm::rmenum :rm_light1))
	       (:light2 (cffi:foreign-enum-value 'rm::rmenum :rm_light2))
	       (:light3 (cffi:foreign-enum-value 'rm::rmenum :rm_light3))
	       (:light4 (cffi:foreign-enum-value 'rm::rmenum :rm_light4))
	       (:light5 (cffi:foreign-enum-value 'rm::rmenum :rm_light5))
	       (:light6 (cffi:foreign-enum-value 'rm::rmenum :rm_light6))
	       (:light7 (cffi:foreign-enum-value 'rm::rmenum :rm_light7))))
  (rmNodeSetSceneLight node type light))

(defun set-spot-direction (direction &key (light *default-light*))
  (LightSetSpotDirection light direction))

(defun set-spot-cutoff (cut-off &key (light *default-light*))
  (LightSetSpotCutoff light cut-off))

(defun set-spot-exponent (exponent &key (light *default-light*))
  (LightSetSpotExponent light exponent))

;;;
;;; End Light

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

    (if color
	(setf c (loop repeat (* subdivisions subdivisions)
		      collect (vector (color-r color) (color-g color) (color-b color) (color-a color)))))

    (setf n (loop repeat (* subdivisions subdivisions)
		  collect ref-normal))

    
    (set-primitive-vertices p v)
    (set-primitive-normals p n)
    (if color
	(set-primitive-colors p c))
    (rm::rmPrimitiveComputeBoundingBox p))
  p)

(defun build-XY-Quad-mesh (p vmin vmax subdivisions ysign color)
  "Build a quadmesh parallel to the X-Y plane."
  (let ((ref-normal (vector 0.0 0.0 (* 1.0 ysign)))
	(w (vector (vertex-x vmin) (vertex-y vmin) (vertex-z vmin)))
	(dx (/ (- (vertex-x vmax) (vertex-x vmin))
	       (- subdivisions 1)))
	(dy (/ (- (vertex-y vmax) (vertex-y vmin))
	       (- subdivisions 1)))
	(v nil) (n nil) (c nil))
    
    (rm::rmPrimitiveSetQmeshDims p subdivisions subdivisions)
    
    (setf v (loop for i from 1 upto subdivisions
		  for w-y = (vertex-y w) then (+ w-y dy)
		  append (loop for j from 1 upto subdivisions
				for w-x = (vertex-x w) then (+ w-x dx)
				collect (vector w-x w-y (vertex-z w)))))

    (if color
	(setf c (loop repeat (* subdivisions subdivisions)
		      collect (vector (color-r color) (color-g color) (color-b color) (color-a color)))))

    (setf n (loop repeat (* subdivisions subdivisions)
		  collect ref-normal))

    
    (set-primitive-vertices p v)
    (set-primitive-normals p n)
    (if color
	(set-primitive-colors p c))
    (rm::rmPrimitiveComputeBoundingBox p))
  p)

(defun build-YZ-Quad-mesh (p vmin vmax subdivisions ysign color)
  "Build a quadmesh parallel to the Y-Z plane."
  (let ((ref-normal (vector (* 1.0 ysign) 0.0 0.0))
	(w (vector (vertex-x vmin) (vertex-y vmin) (vertex-z vmin)))
	(dz (/ (- (vertex-z vmax) (vertex-z vmin))
	       (- subdivisions 1)))
	(dy (/ (- (vertex-y vmax) (vertex-y vmin))
	       (- subdivisions 1)))
	(v nil) (n nil) (c nil))
    
    (rm::rmPrimitiveSetQmeshDims p subdivisions subdivisions)
    
    (setf v (loop for i from 1 upto subdivisions
		  for w-y = (vertex-y w) then (+ w-y dy)
		  append (loop for j from 1 upto subdivisions
				for w-z = (vertex-z w) then (+ w-z dz)
				collect (vector (vertex-x w) w-y w-z))))

    (if color
	(setf c (loop repeat (* subdivisions subdivisions)
		      collect (vector (color-r color) (color-g color) (color-b color) (color-a color)))))

    (setf n (loop repeat (* subdivisions subdivisions)
		  collect ref-normal))

    
    (set-primitive-vertices p v)
    (set-primitive-normals p n)
    (if color 
	(set-primitive-colors p c))
        (rm::rmPrimitiveComputeBoundingBox p))
  p)



;; Dolly Functions
(let ((prev-buttony 0.0))
  (defun dolly (node screen-width screen-height mousey)
    (cffi:with-foreign-object (h-pointer :pointer)
      (let ((buttony (* -1.0 (rm::pixel-to-viewport mousey screen-height))))
	(when (equal (rm::nodegetscenecamera3d node h-pointer) :rm_true)
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
      (initial-transform (cffi:foreign-alloc 'rm::matrix))
      (result-transform (cffi:foreign-alloc 'rm::matrix)))
  (defun arc (node screen-width screen-height mousex mousey)
    (let ((buttonx (rm::pixel-to-viewport mousex screen-width))
	  (buttony (* -1.0 (rm::pixel-to-viewport mousey screen-height))))
      (rm::auxarcball prev-buttonX prev-buttonY buttonx buttony result-transform)
      (rm::rmMatrixMultiply initial-transform result-transform result-transform)
      (rm::rmNodeSetRotateMatrix node result-transform)))
  (defun reset-arc (node screen-width screen-height x y)
    (setf prev-buttonx (rm::pixel-to-viewport x screen-width)
	  prev-buttony (* -1.0 (rm::pixel-to-viewport y screen-height)))
    (if (equal (rm::NodeGetRotateMatrix node initial-transform)	:rm_whacked)
	(rm::rmMatrixIdentity initial-transform))))

;; Translate Functions
(let ((prev-buttonx 0.0)
      (prev-buttony 0.0))
  (defun translate (node screen-width screen-height mousex mousey)
    (cffi:with-foreign-object (h-pointer :pointer)
      (let ((buttonx (rm::pixel-to-viewport mousex screen-width))
	    (buttony (* -1.0 (rm::pixel-to-viewport mousey screen-height))))
	(when (equal (rm::nodegetscenecamera3d node h-pointer) :rm_true)
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
  (with-rmvertex-3d ((rmvertex-3d))
    (if (equal (rm::NodeGetTranslateVector node *default-vertex*) :RM_WHACKED)
	(copy-to-foreign-vertex (vertex 0.0 0.0 0.0) *default-vertex*))
    (rm::rmNodeSetTranslateVector node (add-to-foreign-vertex vert *default-vertex*)))
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

;; (defun single-float (num)
;;   (coerce num 'single-float))

(defmacro with-matrix-symbols (matrix &body body)
  `(symbol-macrolet (,@(let ((result nil))
			    (dotimes (i 4)
			      (dotimes (j 4)
				(setf result (cons `(,(intern (string-upcase (format nil "r~D.c~D" i j)))
						     (cffi:mem-aref
						      (cffi:foreign-slot-pointer ,matrix 'rm::matrix 'rm::m)
						      'rm::s-float (+ (* 4 ,i) ,j)))
						   result))))
			    result))
    ,@body))

(defun rotate-matrix (matrix direction)
  (let ((x-angle (vertex-x direction)) (y-angle (vertex-y direction)) (z-angle (vertex-z direction)))
    (cffi:with-foreign-object (work 'rm::matrix)
      (rm::rmmatrixidentity work)
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
	(rm::rmMatrixMultiply matrix work matrix)
	(rm::rmmatrixidentity work))
      
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
	(rm::rmMatrixMultiply matrix work matrix)
	(rm::rmmatrixidentity work))
      
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

(defun vertex (&rest args)
  (apply #'vector args))

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

(defun color (&rest args)
  (apply #'vector args))

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

(defun new-cube-primitive (&key (primitive (rm::PrimitiveNew :rm_box3d)) (vertices nil)
			   (dimensions #(0.0 0.0 0.0))
			   (color #(0.0 0.0 0.0))
			   (position #(0.0 0.0 0.0)))
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
  primitive)

(defun set-primitive-radius (primitive radius)
  (let ((radius (create-list-if-not radius)))
    (rm::error-if-not-chill (rm::PrimitiveSetRadii primitive
						   (length radius)
						   radius
						   :RM_COPY_DATA
						   (cffi:null-pointer))))
  primitive)

(defun new-sphere-primitive (&key (primitive (rm::PrimitiveNew :rm_spheres))
			     (radius 1.0) (tesselate 32) (color #(0.0 0.0 0.0 0.0)) (position #(0.0 0.0 0.0)))
  (let ((tesselate (case tesselate
		     (8 rm::RM_SPHERES_8)
		     (32 rm::RM_SPHERES_32)
		     (128 rm::RM_SPHERES_128)
		     (512 rm::RM_SPHERES_512)
		     (otherwise rm::RM_SPHERES_32))))
    (when radius
      (set-primitive-radius primitive radius))
    (when tesselate
      (rm::error-if-not-chill (rm::rmPrimitiveSetModelFlag primitive tesselate)))
    (when color
      (set-primitive-colors primitive color))
    (when position
      (set-primitive-position primitive position))
    (rm::rmPrimitiveComputeBoundingBox primitive)
    primitive))

(defun new-cone-primitive (&key (primitive (rm::PrimitiveNew :rm_cones))
			   (radius 1.0) (tesselate 8) (color #(0.0 0.0 0.0 0.0))
			   (vertices (list #(0.0 0.0 0.0) #(0.0 1.0 0.0))))
  (let ((tesselate (case tesselate
		     (4 rm::RM_CONES_4)
		     (8 rm::RM_CONES_8)
		     (12 rm::RM_CONES_12)
		     (16 rm::RM_CONES_16)
		     (32 rm::RM_CONES_32)
		     (128 rm::RM_CONES_128)
		     (otherwise rm::RM_CONES_8))))
    (set-primitive-radius primitive radius)
    (rm::error-if-not-chill (rm::rmPrimitiveSetModelFlag primitive tesselate))
    (set-primitive-vertices primitive vertices)
    (set-primitive-colors primitive color)
    (rm::rmPrimitiveComputeBoundingBox primitive)
    primitive))


(defun new-plane-primitive (&key (primitive (rm::primitivenew :rm_quadmesh))
			    vertices (color #(1.0 1.0 1.0)) (orientation :xz) (subdivisions 20) (sign 1))
  (let ((vmin (first vertices))
	(vmax (second vertices)))
    (case orientation
      (:xz (build-xz-quad-mesh primitive vmin vmax subdivisions sign color))
      (:xy (build-xy-quad-mesh primitive vmin vmax subdivisions sign color))
      (:yz (build-yz-quad-mesh primitive vmin vmax subdivisions sign color))
      (otherwise (build-xz-quad-mesh primitive vmin vmax subdivisions sign color)))
    (rm::rmPrimitiveComputeBoundingBox primitive)
    primitive))

(defun new-triangles-primitive (vertices vert-num indices ind-num
				&key (primitive (rm::primitivenew :rm_indexed_triangles)) normals color)
  (rm::rmPrimitiveSetVertex3D primitive
			      vert-num
			      vertices
			      (cffi:foreign-enum-value 'rm::rmenum :RM_COPY_DATA)
			      (cffi:null-pointer))
  (rm::rmPrimitiveSetIndices primitive
			     ind-num
			     indices
			     (cffi:foreign-enum-value 'rm::rmenum :RM_COPY_DATA)
			     (cffi:null-pointer))
  (if normals 
      (rm::rmPrimitiveSetNormal3D primitive
				  vert-num
				  normals
				  (cffi:foreign-enum-value 'rm::rmenum :RM_COPY_DATA)
				  (cffi:null-pointer)))
  (if color
      (let ((colors (cffi:foreign-alloc :pointer :count (* (* (cffi:foreign-type-size :float) 4) vert-num))))
	(dotimes (i vert-num)
	  (setf (cffi:mem-ref colors :pointer (* (* (cffi:foreign-type-size :float) 4) i))
		(cffi:mem-aref color :pointer)))
	(rm::rmPrimitiveSetColor4D primitive vert-num colors
				   (cffi:foreign-enum-value 'rm::rmenum :RM_COPY_DATA)
				   (cffi:null-pointer))
	(cffi:foreign-free colors)))
  (rm::rmPrimitiveComputeBoundingBox primitive)
  primitive)


;; (defun foreign-array-from-vertex (&rest vertices)
;;   (let ((vertex-array (rm::rmVertex3DNew (length vertices))))
;;     (vertex-copy vertices vertex-array)
;;     vertex-array))

(defun vector-from-foreign-vertex (rmvertex)
  (cffi:with-foreign-slots ((rm::x rm::y rm::z) rmvertex rm::rmvertex3d)
    (vector rm::x rm::y rm::z)))

(defun get-primitive-bounding-box (primitive)
  (let ((vertices nil))
    (with-rmvertex-3d+ (vmin vmax)
      (rm::rmPrimitivegetBoundingBox primitive vmin vmax)
      (setf vertices (list (vector-from-foreign-vertex vmin)
			   (vector-from-foreign-vertex vmax))))
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
      (setf vertices (list (vector-from-foreign-vertex vmin)
			   (vector-from-foreign-vertex vmax))))
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
  (let ((pipe (rm::PipeNew target-platform)))
    (rm::PipeSetRenderPassEnable pipe render-opaque3D render-transparent3D render-all2D)
    (rm::PipeSetProcessingMode pipe processing-mode)
    pipe))

(defun set-default-scene (look-node width height)
  (rm::rmNodeUnionAllBoxes look-node)
  (rm::rmNodeComputeCenterFromBoundingBox look-node)
    
  (rm::NodeSetSceneBackgroundColor look-node #(0.2 0.1 0.2 0.0))

  (with-rmcamera-3d ((rm::rmcamera3dnew))
    (rm::rmDefaultCamera3D *default-camera*)
    (rm::rmCamera3DComputeViewFromGeometry *default-camera* look-node width height)
    (rm::rmNodeSetSceneCamera3D look-node *default-camera*))
    
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

(defun add-primitive (node primitive &key (bounds t) (center t))
  (rm::error-if-not-chill (rm::rmNodeAddPrimitive node primitive))
  (if bounds
      (rm::error-if-not-chill (rm::rmNodeComputeBoundingBox node)))
  (if center
      (rm::error-if-not-chill (rm::rmNodeComputeCenterFromBoundingBox node)))
  node)

(defun inverse-rotation (node matrix)
  (cffi:with-foreign-objects ((old 'rm::matrix))
    (rm::rmMatrixInverse matrix matrix)
    (if (equal (rm::NodeGetRotateMatrix node old) :rm_whacked)
	(rm::rmMatrixIdentity old))
    (rm::rmMatrixMultiply old matrix old)
    (rm::rmNodeSetRotateMatrix node old))
  node)

(defun rotate-node (node &key (direction #(0.0 0.0 0.0)) (match-node nil) (only-this-node nil))
  (cffi:with-foreign-objects ((m 'rm::matrix)
			      (old 'rm::matrix))
    (if match-node
	(if (equal (rm::NodeGetRotateMatrix match-node old) :rm_whacked)
	    (rm::rmMatrixIdentity old))
	(progn
	  (rm::rmmatrixidentity m)
	  (rotate-matrix m direction) ;set matrix values
	  (if (equal (rm::NodeGetRotateMatrix node old) :rm_whacked)
	      (rm::rmMatrixIdentity old))
	  (rm::rmMatrixMultiply old m old)))
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

(defun point-matrix-transform (node direction)
  (cffi:with-foreign-objects ((m 'rm::matrix) (dir 'rm::rmvertex3d))
    (rm::copy-to-foreign-vertex direction dir)
    (unless (equal (rm::NodeGetRotateMatrix node m) :rm_whacked)
      (rm::rmPointMatrixTransform dir m dir))
    (rm::vector-from-foreign-vertex dir)))

(defun set-node-property (node &key ambient-color diffuse-color specular-color specular-exponent)
  (if ambient-color
      (rm::NodeSetAmbientColor node ambient-color))
  (if diffuse-color
      (rm::NodeSetDiffuseColor node diffuse-color))
  (if specular-color
      (rm::NodeSetSpecularColor node specular-color))
  (if specular-exponent
      (rm::NodeSetSpecularExponent node specular-exponent)))

