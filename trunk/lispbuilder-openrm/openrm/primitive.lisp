
(in-package #:rm)

(defclass primitive (openrm-object) ()
  (:default-initargs
   :display-list t
   :gc t
   :free (simple-free #'rm-cffi::rm-primitive-delete 'primitive)))

(defclass box-solid-primitive (primitive)()
  (:default-initargs
   :fp (rm-cffi::rm-Primitive-New :rm-box-3d)))

(defclass box-wire-primitive (primitive)()
  (:default-initargs
   :fp (rm-cffi::rm-Primitive-New :rm-box-3d-wire)))

(defclass app-list-primitive (primitive)()
  (:default-initargs
   :fp (rm-cffi::rm-Primitive-New :rm-app-displaylist)))

(defclass sprite-primitive (primitive)()
  (:default-initargs
   :fp (rm-cffi::rm-Primitive-New :rm-sprite)))

(defmethod initialize-instance :around ((self primitive) &key
                                        rgb/a xy/z normals
                                        bounding-box compute-bounding-box
					display-list app-display-list)
  (call-next-method)

  (setf (display-list-p self) display-list)
  
  (when rgb/a
    (setf (rgb/a self) rgb/a))
  (when xy/z
    (setf (xy/z self) xy/z))
  (when normals
    (setf (normals self) normals))
  (when bounding-box
    (setf (bounding-box self) bounding-box))
  (when app-display-list
    (setf (app-display-list self) app-display-list))
   (when compute-bounding-box
     (compute-bounding-box self))

  (log5:log-for (create) "initialize-instance.PRIMITIVE: ~A, ~A, ~A" self (id self) (this-fp self)))

(defmethod initialize-instance :after ((self sprite-primitive) &key
				       (images nil))
  (when images
    (set-sprites self images)))

(defmethod (setf rgb/a) ((color list) (primitive primitive))
  (setf (rgb/a primitive) (coerce color 'vector)))
(defmethod (setf rgb/a) ((color vector) (primitive primitive))
  (if (vectorp (svref color 0))
    ;; A color array
    (if (> (length color) 3)
      ;; RGBA
      (with-copy-color-4d-array-to-foreign (color fp)
        (rm-cffi::rm-Primitive-Set-Color-4D (fp primitive)
                                            (length color)
                                            fp
                                            :rm-copy-data
                                            (cffi:null-pointer)))
      ;; RGB
      (with-copy-color-3d-array-to-foreign (color fp)
        (rm-cffi::rm-Primitive-Set-Color-3D (fp primitive)
                                            (length color)
                                            fp
                                            :rm-copy-data
                                            (cffi:null-pointer))))
    ;; A single color
    (if (> (length color) 3)
      ;; RGBA
      (with-copy-color-4d-to-foreign (color fp)
        (rm-cffi::rm-Primitive-Set-Color-4D (fp primitive)
                                            1
                                            fp
                                            :rm-copy-data
                                            (cffi:null-pointer)))
      ;; RGB
      (with-copy-color-3d-to-foreign (color fp)
        (rm-cffi::rm-Primitive-Set-Color-3D (fp primitive)
                                            1
                                            fp
                                            :rm-copy-data
                                            (cffi:null-pointer)))))
  color)
(defmethod (setf rgb/a) ((color c3d) (primitive primitive))
  (rm-cffi::rm-Primitive-Set-Color-3D (fp primitive)
				      1
				      (fp color)
                                      (copy-data color)
                                      (if (copy-p color)
                                        (cffi:null-pointer)
                                        (cffi:callback color-3d-proc)))
  primitive)
(defmethod (setf rgb/a) ((color c3d*) (primitive primitive))
  (rm-cffi::rm-Primitive-Set-Color-3D (fp primitive)
				      (size color)
				      (fp color)
                                      (copy-data color)
                                      (if (copy-p color)
                                        (cffi:null-pointer)
                                        (cffi:callback color-3d-proc)))
  primitive)

(defmethod (setf rgb/a) ((color c4d) (primitive primitive))
  (rm-cffi::rm-Primitive-Set-Color-4D (fp primitive)
				      1
				      (fp color)
                                      (copy-data color)
                                      (if (copy-p color)
                                        (cffi:null-pointer)
                                        (cffi:callback color-4d-proc)))
  primitive)
(defmethod (setf rgb/a) ((color c4d*) (primitive primitive))
  (rm-cffi::rm-Primitive-Set-Color-4D (fp primitive)
				      (size color)
				      (fp color)
                                      (copy-data color)
                                      (if (copy-p color)
                                        (cffi:null-pointer)
                                        (cffi:callback color-4d-proc)))
  primitive)

(defmethod (setf xy/z) ((vertex list) (primitive primitive))
  (setf (xy/z primitive) (coerce vertex 'vector)))
(defmethod (setf xy/z) ((vertex vector) (primitive primitive))
  (if (vectorp (svref vertex 0))
    ;; A vertex array
    (if (> (length vertex) 2)
      ;; xyz
      (with-copy-vertex-3d-array-to-foreign (vertex fp)
        (rm-cffi::rm-primitive-set-vertex-3d (fp primitive)
                                             (length vertex)
                                             fp
                                             :rm-copy-data
                                             (cffi:null-pointer)))
      ;; xy
      (with-copy-vertex-2d-array-to-foreign (vertex fp)
        (rm-cffi::rm-primitive-set-vertex-2d (fp primitive)
                                             (length vertex)
                                             fp
                                             :rm-copy-data
                                             (cffi:null-pointer))))
    ;; A single vertex
    (if (> (length vertex) 2)
      ;; xyz
      (with-copy-vertex-3d-to-foreign (vertex fp)
        (rm-cffi::rm-primitive-set-vertex-3d (fp primitive)
                                             1
                                             fp
                                             :rm-copy-data
                                             (cffi:null-pointer)))
      ;; xy
      (with-copy-vertex-2d-to-foreign (vertex fp)
        (rm-cffi::rm-primitive-set-vertex-2d (fp primitive)
                                             1
                                             fp
                                             :rm-copy-data
                                             (cffi:null-pointer)))))
  vertex)
(defmethod (setf xy/z) ((position v2d) (primitive primitive))
  (rm-cffi::rm-Primitive-Set-Vertex-2D (fp primitive)
				       1
				       (fp position)
                                       (copy-data position)
                                       (if (copy-p position) 
                                         (cffi:null-pointer)
                                         (cffi:callback vertex-2d-proc)))
  primitive)
(defmethod (setf xy/z) ((position v3d) (primitive primitive))
  (rm-cffi::rm-Primitive-Set-Vertex-3D (fp primitive)
				       1
				       (fp position)
                                       (copy-data position)
                                       (if (copy-p position) 
                                         (cffi:null-pointer)
                                         (cffi:callback vertex-3d-proc)))
  primitive)
(defmethod (setf xy/z) ((position v2d*) (primitive primitive))
  (rm-cffi::rm-Primitive-Set-Vertex-2D (fp primitive)
				       (size position)
				       (fp position)
                                       (copy-data position)
                                       (if (copy-p position) 
                                         (cffi:null-pointer)
                                         (cffi:callback vertex-2d-proc)))
  primitive)
(defmethod (setf xy/z) ((position v3d*) (primitive primitive))
  (rm-cffi::rm-Primitive-Set-Vertex-3D (fp primitive)
				       (size position)
				       (fp position)
                                       (copy-data position)
                                       (if (copy-p position) 
					   (cffi:null-pointer)
					   (cffi:callback vertex-3d-proc)))
  primitive)


(defmethod (setf normals) ((vertex list) (primitive primitive))
  (setf (normals primitive) (coerce vertex 'vector)))
(defmethod (setf normals) ((vertex vector) (primitive primitive))
  (if (vectorp (svref vertex 0))
    ;; A vertex array
    ;; xyz
    (with-copy-vertex-3d-array-to-foreign (vertex fp)
      (rm-cffi::rm-primitive-set-normal-3d (fp primitive)
                                           (length vertex)
                                           fp
                                           :rm-copy-data
                                           (cffi:null-pointer)))
    ;; A single vertex
    ;; xyz
    (with-copy-vertex-3d-to-foreign (vertex fp)
      (rm-cffi::rm-primitive-set-normal-3d (fp primitive)
                                           1
                                           fp
                                           :rm-copy-data
                                           (cffi:null-pointer))))
  vertex)

(defmethod (setf normals) ((normal v3d) (primitive primitive))
  (rm-cffi::rm-Primitive-Set-Normal-3d (fp primitive)
				       1
				       (fp normal)
                                       (copy-data normal)
                                       (if (copy-p normal) 
					   (cffi:null-pointer)
					   (cffi:callback vertex-3d-proc)))
  primitive)
(defmethod (setf normals) ((normal v3d*) (primitive primitive))
  (rm-cffi::rm-Primitive-Set-Normal-3d (fp primitive)
				       (size normal)
				       (fp normal)
                                       (copy-data normal)
                                       (if (copy-p normal) 
					   (cffi:null-pointer)
					   (cffi:callback vertex-3d-proc)))
  primitive)

(defmethod bounding-box-min ((self primitive))
  (rm-base:with-v3d (v)
    (when (rm-cffi::rm-primitive-get-bounding-box (fp self) v (cffi:null-pointer))
      (vertex rm-base::x rm-base::y rm-base::z))))
(defmethod bounding-box-min* ((self primitive))
  (let ((v (c3d nil nil nil)))
    (when (rm-cffi::rm-primitive-get-bounding-box (fp self) (fp v) (cffi:null-pointer))
      v)))

(defmethod bounding-box-max ((self primitive))
  (rm-base:with-v3d (v)
    (when (rm-cffi::rm-primitive-get-bounding-box (fp self) (cffi:null-pointer) v)
      (vertex rm-base::x rm-base::y rm-base::z))))
(defmethod bounding-box-max* ((self primitive))
  (let ((v (v3d nil nil nil)))
    (when (rm-cffi::rm-primitive-get-bounding-box (fp self) (cffi:null-pointer) (fp v))
      v)))

(defmethod bounding-box ((self primitive))
  (cffi:with-foreign-object (bounds 'rm-cffi::rm-vertex-3d 2)
    (if (rm-cffi::rm-primitive-get-bounding-box (fp self)
                                                (cffi:mem-aref bounds 'rm-cffi::rm-vertex-3d 0)
                                                (cffi:mem-aref bounds 'rm-cffi::rm-vertex-3d 1))
      (new-vertex-from-foreign-3d-vertex bounds 2))))
  
(defmethod bounding-box* ((self primitive))
  (let ((bounds (v3d* 2)))
    (if (rm-cffi::rm-primitive-get-bounding-box (fp self)
                                                (cffi:mem-aref (fp bounds) 'rm-cffi::rm-vertex-3d 0)
                                                (cffi:mem-aref (fp bounds) 'rm-cffi::rm-vertex-3d 1))
      bounds
      nil)))

(defmethod (setf bounding-box) ((bounds vector) (self primitive))
  (cffi:with-foreign-object (bounds 'rm-cffi::rm-vertex-3d 2)
    (if (rm-cffi::rm-Primitive-Set-Bounding-Box (fp self)
                                              (cffi:mem-aref bounds 'rm-cffi::rm-vertex-3d 0)
                                              (cffi:mem-aref bounds 'rm-cffi::rm-vertex-3d 1))
      (new-vertex-from-foreign-3d-vertex bounds 2))))
(defmethod (setf bounding-box) ((bounds v3d*) (self primitive))
  (rm-cffi::rm-Primitive-Set-Bounding-Box (fp self) (fp (nth-vertex bounds 0)) (fp (nth-vertex bounds 1)))
  bounds)

(defmethod compute-bounding-box ((self primitive))
  (rm-cffi::rm-Primitive-Compute-Bounding-Box (fp self))
  self)

(defmethod (setf display-list-p) (value (self primitive))
  (rm-cffi::rm-Primitive-Set-Display-List-Enable (fp self) value))

(defmethod (setf app-display-list) (dsp-list (self app-list-primitive))
  (rm-cffi::rm-primitive-set-app-display-list (fp self) dsp-list))

(cffi:defcallback vertex-2d-proc :pointer
    ((data-fp :pointer))
  "Called when a vertex-2d is deleted"
  (log5:log-for (info) "DEFCALLBACK:VERTEX-2D")
  (cffi:foreign-free data-fp)
  ;;(rm-cffi::rm-vertex-2d-delete data-fp)
  (cffi:null-pointer))

(cffi:defcallback vertex-3d-proc :pointer
    ((data-fp :pointer))
  "Called when a vertex-2d is deleted"
  (log5:log-for (info) "DEFCALLBACK:VERTEX-2D")
  (cffi:foreign-free data-fp)
  ;;(rm-cffi::rm-vertex-3d-delete data-fp)
  (cffi:null-pointer))

(cffi:defcallback color-3d-proc :pointer
    ((data-fp :pointer))
  "Called when a color-3d is deleted"
  (log5:log-for (info) "DEFCALLBACK:COLOR-3D")
  (cffi:foreign-free data-fp)
  ;;(rm-cffi::rm-color-3d-delete data-fp)
  (cffi:null-pointer))

(cffi:defcallback color-4d-proc :pointer
    ((data-fp :pointer))
  "Called when a color-4d is deleted"
  (log5:log-for (info) "DEFCALLBACK:COLOR-4D")
  (cffi:foreign-free data-fp)
  ;;(rm-cffi::rm-color-4d-delete data-fp)
  (cffi:null-pointer))


