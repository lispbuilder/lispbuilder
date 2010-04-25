
(in-package #:rm)

(defclass primitive (foreign-object) ()
  (:default-initargs
   :display-list t
   :gc t
   :free (simple-free #'rm-cffi::rm-primitive-delete 'primitive)))

(defclass poly-primitive (primitive)()
  (:default-initargs
   :fp (rm-cffi::rm-Primitive-New :rm-polys)))

(defclass quad-primitive (primitive)()
  (:default-initargs
   :fp (rm-cffi::rm-Primitive-New :rm-quads)))

(defclass box-solid-primitive (primitive)()
  (:default-initargs
   :fp (rm-cffi::rm-Primitive-New :rm-box-3d)))

(defclass box-wire-primitive (primitive)()
  (:default-initargs
   :fp (rm-cffi::rm-Primitive-New :rm-box-3d-wire)))

(defclass app-list-primitive (primitive)()
  (:default-initargs
   :fp (rm-cffi::rm-Primitive-New :rm-app-displaylist)))

(defclass sprite-primitive (primitive)
  ((copy-image
    :reader copy-image-p
    :initarg :copy-image))
  (:default-initargs
   :copy-image t
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
     (compute-bounding-box self)))

(defmethod initialize-instance :after ((self sprite-primitive) &key
				       (images nil))
  (when images
    (set-sprites self images)))

(defmethod (setf rgb/a) ((color vector) (primitive primitive))
  (if (vectorp (svref color 0))
    ;; A color array
    (if (> (length (svref color 0)) 2)
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

(defmethod (setf rbg/a) :after ((color foreign-object) (primitive primitive))
  (if (gc-p color)
    (setf (gc-p color) nil)))

(defmethod (setf rgb/a) ((color color-3d) (primitive primitive))
  (rm-cffi::rm-Primitive-Set-Color-3D (fp primitive)
                                      (size color)
                                      (fp color)
                                      (copy-data color)
                                      (if (copy-p color)
                                        (cffi:null-pointer)
                                        (free-callback color)))
  primitive)

(defmethod (setf rgb/a) ((color color-4d) (primitive primitive))
  (rm-cffi::rm-Primitive-Set-Color-4D (fp primitive)
				      (size color)
				      (fp color)
                                      (copy-data color)
                                      (if (copy-p color)
                                        (cffi:null-pointer)
                                        (free-callback color)))
  primitive)

(defmethod (setf xy/z) ((vertex vector) (primitive primitive))
  (if (vectorp (svref vertex 0))
    ;; A vertex array
    (if (> (length (svref vertex 0)) 2)
      ;; xyz
      (progn
        (with-copy-vertex-3d-array-to-foreign (vertex fp)
          (rm-cffi::rm-primitive-set-vertex-3d (fp primitive)
                                               (length vertex)
                                               fp
                                               :rm-copy-data
                                               (cffi:null-pointer))))
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

(defmethod (setf xy/z) :after ((vertex vertex) (primitive primitive))
  (when (gc-p vertex)
    (setf (gc-p vertex) nil)))

(defmethod (setf xy/z) ((position vertex-2d) (primitive primitive))
  (rm-cffi::rm-Primitive-Set-Vertex-2D (fp primitive)
                                       (size position)
				       (fp position)
                                       (copy-data position)
                                       (if (copy-p position)
                                         (cffi:null-pointer)
                                         (free-callback position)))
  primitive)
(defmethod (setf xy/z) ((position vertex-3d) (primitive primitive))
  (rm-cffi::rm-Primitive-Set-Vertex-3D (fp primitive)
                                       (size position)
				       (fp position)
                                       (copy-data position)
                                       (if (copy-p position)
                                         (cffi:null-pointer)
                                         (free-callback position)))
  primitive)

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

(defmethod (setf normals) :after ((vertex copyable-object) (primitive primitive))
  (when (gc-p vertex)
    (setf (gc-p vertex) nil)))

(defmethod (setf normals) ((normal vertex-3d) (primitive primitive))
  (rm-cffi::rm-Primitive-Set-Normal-3d (fp primitive)
                                       (size normal)
				       (fp normal)
                                       (copy-data normal)
                                       (if (copy-p normal)
                                         (cffi:null-pointer)
                                         (free-callback normal)))
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
