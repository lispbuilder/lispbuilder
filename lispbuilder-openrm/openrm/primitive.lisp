
(in-package #:rm)

(defclass primitive (openrm-object) ()
  (:default-initargs
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
					(rgb/a nil) (xy/z nil) (normals nil)
                                        (bounding-box nil) (compute-bounding-box nil)
					(display-list-p nil) (app-display-list nil))
  (call-next-method)
 
  (when rgb/a
    (setf (rgb/a self) rgb/a))
  (when xy/z
    (setf (xy/z self) xy/z))
  (when normals
    (setf (normals self) normals))
  (when bounding-box
    (setf (bounding-box self) bounding-box))
  (when display-list-p
    (setf (display-list-p self) display-list-p))
  (when app-display-list
    (setf (app-display-list self) app-display-list))
   (when compute-bounding-box
     (compute-bounding-box self))

  (log5:log-for (create) "initialize-instance.PRIMITIVE: ~A, ~A, ~A" self (id self) (this-fp self)))

(defmethod initialize-instance :after ((self sprite-primitive) &key
				       (images nil))
  (when images
    (set-sprites self images)))

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

(defmethod (setf bounding-box) ((bounds v3d*) (self primitive))
  (rm-cffi::rm-Primitive-Set-Bounding-Box (fp self) (fp (nth-vertex bounds 0)) (fp (nth-vertex bounds 1)))
  self)
(defmethod bounding-box ((self primitive))
  (let ((bounds (v3d* nil 2)))
    (if (rm-cffi::rm-Primitive-Get-Bounding-Box (fp self) (fp (nth-vertex bounds 0)) (fp (nth-vertex bounds 1)))
	bounds
	nil)))

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

(defmacro with-default-primitive ((prim) &body body)
  (let ((primitive (gensym "primitive-"))
	(add-to-parent? (gensym "add-to-parent?-")))
    `(progn
       (unless *parent-node*
	 (error "WITH-DEFAULT-PRIMITIVE valid only within scope of WITH-DEFAULT-NODE."))
       (let ((,primitive ,prim)
	     (,add-to-parent? t))
	 (let ((*default-primitive* ,primitive))
	   (labels ((set-xy/z (vertex &optional (primitive *default-primitive*))
		      (setf (xy/z primitive) vertex))
		    (set-rgb/a (color &optional (primitive *default-primitive*))
		      (setf (rgb/a primitive) color))
		    (set-normals (normals &optional (primitive *default-primitive*))
		      (setf (normals primitive) normals))
		    (set-radius (radius &optional (primitive *default-primitive*))
		      (setf (radius primitive) radius))
		    (set-tesselate (tesselate &optional (primitive *default-primitive*))
		      (setf (tesselate primitive) tesselate))
		    (set-bounding-box (bounds &optional (primitive *default-primitive*))
		      (setf (bounding-box primitive) bounds))
		    (get-bounding-box (&optional (primitive *default-primitive*))
		      (bounding-box primitive))
		    (set-compute-bounding-box (&optional (primitive *default-primitive*))
		      (compute-bounding-box primitive))
		    (set-text (value &optional (primitive *default-primitive*))
		      (setf (text primitive) value))
		    (set-display-list-p (value &optional (primitive *default-primitive*))
		      (setf (display-list-p primitive) value))
		    (set-app-display-list (value &optional (primitive *default-primitive*))
		      (setf (app-display-list primitive) value))
		    (add-this-to-parent (add?)
		      (if add?
			  (setf ,add-to-parent? t)
			  (setf ,add-to-parent? nil))))
	     (declare (ignorable #'set-xy/z #'set-rgb/a #'set-normals #'set-radius #'set-tesselate
				 #'set-bounding-box #'get-bounding-box #'set-compute-bounding-box
				 #'set-copy-data #'set-text #'set-display-list-p #'set-app-display-list
				 #'add-this-to-parent))
	     ,@body
	     (when (and *parent-node*
			,add-to-parent?)
	       (add-to-node *parent-node* ,primitive))))))))

