
(in-package #:rm)

(defclass camera (openrm-object) ())

(defclass camera-2D (camera)
  ()
  (:default-initargs
   :fp (rm-cffi::rm-camera-2d-new)
   :free (simple-free #'rm-cffi::rm-camera-2d-delete 'camera-2d)))

(defclass camera-3D (camera)
  ()
  (:default-initargs
   :fp (rm-cffi::rm-camera-3d-new)
   :free (simple-free #'rm-cffi::rm-camera-3d-delete 'camera-3d)))


(defmacro with-camera ((var &optional camera (free-p t))
		       &body body)
  (let ((body-value (gensym "body-value")))
    `(let* ((,@(if camera
		   `(,var ,camera)
		   `(,var ,var)))
	    (*current-camera* ,var)
	    (,body-value nil))
       (symbol-macrolet ((fp-camera (fp ,var)))
	 (setf ,body-value (progn ,@body))
	 (when ,free-p
	   (free ,var))
	 ,body-value))))

(defmethod initialize-instance :around ((self camera-2d) &key
					(extents nil) (aspect-ratio nil)
					(defaults nil))
  (call-next-method)

  (when defaults
    (assign-defaults self))
  (when extents
    (setf (extents self) extents))
  (when aspect-ratio
    (setf (aspect-ratio self) aspect-ratio)))

(defmethod initialize-instance :around ((self camera-3d) &key
					(eye nil) (at nil) (up-vector nil)
                                        (hither nil) (yon nil) (fov nil)
                                        (aspect-ratio nil) (projection nil)
					(stereo nil) (eye-separation nil)
                                        (focal-distance nil)
					(defaults nil))
  (call-next-method)

  (when defaults
    (assign-defaults self))
  (when eye
    (setf (eye self) eye))
  (when at
    (setf (at self) at))
  (when up-vector
    (setf (up-vector self) up-vector))
  (when hither
    (setf (hither self) hither))
  (when yon
    (setf (yon self) yon))
  (when fov
    (setf (fov self) fov))
  (when aspect-ratio
    (setf (aspect-ratio self) aspect-ratio))
  (when projection
    (setf (projection self) projection))
  (when stereo
    (setf (stereo self) stereo))
  (when eye-separation
    (setf (eye-separation self) eye-separation))
  (when focal-distance
    (setf (focal-distance self) focal-distance)))

(defmethod extents ((self camera-2d))
  (cffi:with-foreign-objects ((xmin :float) (ymin :float) (xmax :float) (ymax :float))
    (if (rm-cffi::rm-camera-2d-get-extents (fp self) xmin ymin xmax ymax)
	(vector (cffi:mem-aref xmin :float)
		(cffi:mem-aref ymin :float)
		(cffi:mem-aref xmax :float)
		(cffi:mem-aref ymax :float))
	nil)))

(defmethod (setf extents) ((dims vector float 4) (self camera-2d))
  (if (rm-cffi::rm-camera-2d-set-extents (fp self) (svref dims 0) (svref dims 1) (svref dims 2) (svref dims 3))
      self
      nil))

(defmethod assign-defaults ((self camera-2d))
  (rm-cffi::rm-Default-Camera-2D (fp self))
  self)

(defmethod assign-defaults ((self camera-3d))
  (rm-cffi::rm-Default-Camera-3D (fp self))
  self)

(defmethod aspect-ratio ((camera camera-2d))
  (cffi:with-foreign-object (aspect :float)
    (if (rm-cffi::rm-Camera-2D-Get-Aspect-Ratio (fp camera) aspect)
	(cffi:mem-aref aspect :float)
	nil)))

(defmethod aspect-ratio ((camera camera-3d))
  (cffi:with-foreign-object (aspect :float)
    (if (rm-cffi::rm-Camera-3D-Get-Aspect-Ratio (fp camera))
	(cffi:mem-aref aspect :float)
	nil)))

(defmethod (setf aspect-ratio) ((aspect vector) (camera camera-2d))
  (rm-cffi::rm-Camera-2D-Set-Aspect-Ratio (fp camera) (coerce (/ (svref aspect 0) (svref aspect 1)) 'float)))

(defmethod (setf aspect-ratio) ((aspect vector) (camera camera-3d))
  (rm-cffi::rm-Camera-3D-Set-Aspect-Ratio (fp camera) (coerce (/ (svref aspect 0) (svref aspect 1)) 'float)))

(defmethod reset-aspect-ratio ((self camera-2d) (viewport vector) (dims vector))
  (cffi:with-foreign-object (vp :float 4)
    (setf (cffi:mem-aref vp :float 0) (svref viewport 0)
	  (cffi:mem-aref vp :float 1) (svref viewport 1)
	  (cffi:mem-aref vp :float 2) (svref viewport 2)
	  (cffi:mem-aref vp :float 3) (svref viewport 3))
    (rm-cffi::rm-camera-2d-reset-aspect-ratio (fp self) vp (svref dims 0) (svref dims 1))))

(defmethod reset-aspect-ratio ((self camera-3d) (viewport vector) (dims vector))
  (cffi:with-foreign-object (vp :float 4)
    (setf (cffi:mem-aref vp :float 0) (svref viewport 0)
	  (cffi:mem-aref vp :float 1) (svref viewport 1)
	  (cffi:mem-aref vp :float 2) (svref viewport 2)
	  (cffi:mem-aref vp :float 3) (svref viewport 3))
    (rm-cffi::rm-camera-3d-reset-aspect-ratio (fp self) vp (svref dims 0) (svref dims 1))))

(defmethod eye ((camera camera-3d))
  (rm-base:with-v3d (v)
    (rm-cffi::rm-camera-3d-get-eye (fp camera) v)
    (vertex rm-base::x rm-base::y rm-base::z)))
(defmethod eye* ((camera camera-3d))
  (let ((v (v3d nil nil nil)))
    (rm-cffi::rm-camera-3d-get-eye (fp camera) (fp v))
    v))

(defmethod (setf eye) ((value vector) (camera camera-3d))
  (with-copy-vertex-3d-to-foreign (value fp)
    (rm-cffi::rm-camera-3d-set-eye (fp camera) (fp value)))
  value)
(defmethod (setf eye) ((value v3d) (camera camera-3d))
  (rm-cffi::rm-camera-3d-set-eye (fp camera) (fp value))
  value)

(defmethod at ((camera camera-3d))
  (rm-base:with-v3d (v)
    (rm-cffi::rm-camera-3d-get-at (fp camera) v)
    (vertex rm-base::x rm-base::y rm-base::z)))
(defmethod at* ((camera camera-3d))
  (let ((v (v3d nil nil nil)))
    (rm-cffi::rm-camera-3d-get-at (fp camera) (fp v))
    v))

(defmethod (setf at) ((value vector) (camera camera-3d))
  (with-copy-vertex-3d-to-foreign (value fp)
    (rm-cffi::rm-camera-3d-set-at (fp camera) fp))
  value)
(defmethod (setf at) ((value v3d) (camera camera-3d))
  (rm-cffi::rm-camera-3d-set-at (fp camera) (fp value))
  value)

(defmethod hither ((camera camera-3d))
  (rm-cffi::rm-camera-3d-get-hither (fp camera)))

(defmethod (setf hither) ((value float) (camera camera-3d))
  (rm-cffi::rm-camera-3d-set-hither (fp camera) value))

(defmethod yon ((camera camera-3d))
  (rm-cffi::rm-camera-3d-get-yon (fp camera)))

(defmethod (setf yon) ((value float) (camera camera-3d))
  (rm-cffi::rm-camera-3d-set-yon (fp camera) value))

(defmethod fov ((camera camera-3d))
  (rm-cffi::rm-camera-3d-get-fov (fp camera)))

(defmethod (setf fov) ((value float) (camera camera-3d))
  (rm-cffi::rm-camera-3d-set-fov (fp camera) value))

(defmethod up-vector ((camera camera-3d))
  (rm-base:with-v3d (v)
    (rm-cffi::rm-camera-3d-get-up-vector (fp camera) v)
    (vector rm-base::x rm-base::y rm-base::z)))
(defmethod up-vector* ((camera camera-3d))
  (let ((v (v3d nil nil nil)))
    (rm-cffi::rm-camera-3d-get-up-vector (fp camera) (fp v))
    v))

(defmethod (setf up-vector) ((value vector) (camera camera-3d))
  (with-copy-vertex-3d-to-foreign (value fp)
    (rm-cffi::rm-camera-3d-set-up-vector (fp camera) fp))
  value)
(defmethod (setf up-vector) ((value v3d) (camera camera-3d))
  (rm-cffi::rm-camera-3d-set-up-vector (fp camera) (fp value))
  value)

(defmethod projection ((camera camera-3d))
  (rm-cffi::rm-camera-3d-get-projection (fp camera)))

(defmethod (setf projection) (value (camera camera-3d))
  (rm-cffi::rm-camera-3d-set-projection (fp camera) value))

(defmethod stereo ((camera camera-3d))
  (rm-cffi::rm-camera-3d-get-stereo (fp camera)))
(defmethod (setf stereo) (value (camera camera-3d))
  (rm-cffi::rm-camera-3d-set-stereo (fp camera) value))

(defmethod eye-separation ((camera camera-3d))
  (rm-cffi::rm-camera-3d-get-eye-separation (fp camera)))
(defmethod (setf eye-separation) (value (camera camera-3d))
  (rm-cffi::rm-camera-3d-set-eye-separation (fp camera) value))
(defmethod focal-distance ((camera camera-3d))
  (rm-cffi::rm-camera-3d-get-focal-distance (fp camera)))
(defmethod (setf focal-distance) (value (camera camera-3d))
  (rm-cffi::rm-camera-3d-set-focal-distance (fp camera) value))
