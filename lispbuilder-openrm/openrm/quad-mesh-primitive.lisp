
(in-package #:lispbuilder-openrm)


(defclass quad-mesh-primitive (primitive)()
  (:default-initargs
   :fp (rm-cffi::rm-Primitive-New :rm-quadmesh)))

(defclass plane-primitive (quad-mesh-primitive)
  ((orientation
    :accessor orientation
    :initform :xz
    :initarg :orientation)
   (subdivisions
    :reader subdivisions
    :initform 20
    :initarg :subdivisions)
   (sign
    :accessor sign
    :initform 1
    :initarg :sign)))

(defun build-XZ-Quad-Mesh (primitive vmin vmax subdivisions ysign)
  "Build a quad-mesh parallel to the X-Z plane."
  (let ((ref-normal (vertex 0.0 (* 1.0 ysign) 0.0))
        (w (vertex (x vmin) (y vmin) (z vmin)))
	(dx (/ (- (x vmax) (x vmin))
	       (- subdivisions 1)))
	(dz (/ (- (z vmax) (z vmin))
	       (- subdivisions 1))))
    
    (rm-cffi::rm-Primitive-Set-Qmesh-Dims (fp primitive) subdivisions subdivisions)

    ;; Set the Vertexes
    (cffi:with-foreign-object (v 'rm-cffi::rm-vertex-3d (* subdivisions subdivisions))
      (loop for i from 0 upto (1- subdivisions)
            for w-z = (z w) then (+ w-z dz)
            do (loop for j from 0 upto (1- subdivisions)
                     for w-x = (x w) then (+ w-x dx)
                     do (cffi:with-foreign-slots ((rm-cffi::x rm-cffi::y rm-cffi::z)
                                                  (cffi:mem-aref v 'rm-cffi::rm-vertex-3d (+ (* i subdivisions) j))
                                                  rm-cffi::rm-vertex-3d)
                          (setf rm-cffi::x w-x
                                rm-cffi::y (y w)
                                rm-cffi::z w-z))))
      (rm-cffi::rm-Primitive-Set-Vertex-3D (fp primitive)
					   (* subdivisions subdivisions)
					   v
					   :RM-COPY-DATA
					   (cffi:null-pointer)))
    ;; Set the normals
;(setf (normals primitive) (v3d* (* subdivisions subdivisions) :initial-element ref-normal))
    (setf (normals primitive) (vertex* (* subdivisions subdivisions) :initial-element ref-normal))
    primitive))


(defun build-XY-Quad-mesh (primitive vmin vmax subdivisions ysign)
  "Build a quad-mesh parallel to the X-Y plane."
  (let ((ref-normal (vertex 0.0 0.0 (* 1.0 ysign)))
	(w (v3d (x vmin) (y vmin) (z vmin)))
	(dx (/ (- (x vmax) (x vmin))
	       (- subdivisions 1)))
	(dy (/ (- (y vmax) (y vmin))
	       (- subdivisions 1))))
    
    (rm-cffi::rm-Primitive-Set-Qmesh-Dims (fp primitive) subdivisions subdivisions)

    ;; Set the Vertexes
    (cffi:with-foreign-object (v 'rm-cffi::rm-vertex-3d (* subdivisions subdivisions))
      (loop for i from 0 upto (1- subdivisions)
            for w-y = (y w) then (+ w-y dy)
            do (loop for j from 0 upto (1- subdivisions)
                     for w-x = (x w) then (+ w-x dx)
                     do (cffi:with-foreign-slots ((rm-cffi::x rm-cffi::y rm-cffi::z)
                                                  (cffi:mem-aref v 'rm-cffi::rm-vertex-3d (+ (* i subdivisions) j))
                                                  rm-cffi::rm-vertex-3d)
                          (setf rm-cffi::x w-x
                                rm-cffi::y w-y
                                rm-cffi::z (z w)))))
      (rm-cffi::rm-Primitive-Set-Vertex-3D (fp primitive)
					   (* subdivisions subdivisions)
					   v
					   :RM-COPY-DATA
					   (cffi:null-pointer)))

    ;; Set the Normals
    ;;(setf (normals primitive) (v3d* (* subdivisions subdivisions) :initial-element ref-normal))
    (setf (normals primitive) (vertex* (* subdivisions subdivisions) :initial-element ref-normal))
    primitive))

(defun build-YZ-Quad-mesh (primitive vmin vmax subdivisions ysign)
  "Build a quad-mesh parallel to the Y-Z plane."
  (let ((ref-normal (vertex (* 1.0 ysign) 0.0 0.0))
	(w (v3d (x vmin) (y vmin) (z vmin)))
	(dz (/ (- (z vmax) (z vmin))
	       (- subdivisions 1)))
	(dy (/ (- (y vmax) (y vmin))
	       (- subdivisions 1))))
    
    (rm-cffi::rm-Primitive-Set-Qmesh-Dims (fp primitive) subdivisions subdivisions)
    
    ;; Set the Vertexes
    (cffi:with-foreign-object (v 'rm-cffi::rm-vertex-3d (* subdivisions subdivisions))
      (loop for i from 0 upto (1- subdivisions)
            for w-y = (y w) then (+ w-y dy)
            do (loop for j from 0 upto (1- subdivisions)
                     for w-z = (z w) then (+ w-z dz)
                     do (cffi:with-foreign-slots ((rm-cffi::x rm-cffi::y rm-cffi::z)
                                                  (cffi:mem-aref v 'rm-cffi::rm-vertex-3d (+ (* i subdivisions) j))
                                                  rm-cffi::rm-vertex-3d)
                          (setf rm-cffi::x (x w)
                                rm-cffi::y w-y
                                rm-cffi::z w-z))))
      (rm-cffi::rm-Primitive-Set-Vertex-3D (fp primitive)
					   (* subdivisions subdivisions)
					   v
					   :RM-COPY-DATA
					   (cffi:null-pointer)))

    ;; Set the Normals
    ;;(setf (normals primitive) (v3d* (* subdivisions subdivisions) :initial-element ref-normal))
    (setf (normals primitive) (vertex* (* subdivisions subdivisions) :initial-element ref-normal))
    primitive))

(defmethod (setf xy/z) ((bounds list) (primitive quad-mesh-primitive))
  (setf (xy/z primitive) (coerce bounds 'vector)))
(defmethod (setf xy/z) ((bounds vector) (primitive quad-mesh-primitive))
  (unless (vectorp (svref bounds 0))
    (error "XY/Z with V3D is not allowed for the primitive type QUAD-MESH-PRIMITIVE. Use V3D* instead."))
  (let ((vmin (nth-vertex bounds 0))
        (vmax (nth-vertex bounds 1)))
    (case (orientation primitive)
      (:xz (build-xz-quad-mesh primitive vmin vmax (subdivisions primitive) (sign primitive)))
      (:xy (build-xy-quad-mesh primitive vmin vmax (subdivisions primitive) (sign primitive)))
      (:yz (build-yz-quad-mesh primitive vmin vmax (subdivisions primitive) (sign primitive)))
      (otherwise (build-xz-quad-mesh primitive vmin vmax (subdivisions primitive) (sign primitive))))))
(defmethod (setf xy/z) ((bounds v3d) (primitive quad-mesh-primitive))
  (error "XY/Z with V3D is not allowed for the primitive type QUAD-MESH-PRIMITIVE. Use V3D* instead."))
(defmethod (setf xy/z) ((bounds v3d*) (primitive plane-primitive))
  (let ((vmin (nth-vertex bounds 0))
	(vmax (nth-vertex bounds 1)))
    (case (orientation primitive)
      (:xz (build-xz-quad-mesh primitive vmin vmax (subdivisions primitive) (sign primitive)))
      (:xy (build-xy-quad-mesh primitive vmin vmax (subdivisions primitive) (sign primitive)))
      (:yz (build-yz-quad-mesh primitive vmin vmax (subdivisions primitive) (sign primitive)))
      (otherwise (build-xz-quad-mesh primitive vmin vmax (subdivisions primitive) (sign primitive)))))
  primitive)

(defmethod (setf rgb/a) ((color c4d) (primitive plane-primitive))
  (let ((colors (c4d* color (* (subdivisions primitive) (subdivisions primitive)))))
    (rm-cffi::rm-Primitive-Set-Color-4D (fp primitive)
					(size colors)
					(fp colors)
					(copy-data primitive)
					(cffi:null-pointer)))
  primitive)

