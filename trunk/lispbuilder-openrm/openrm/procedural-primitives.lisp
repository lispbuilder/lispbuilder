
(in-package #:rm)

(defclass procedural-primitive (primitive)())

(defclass sphere-primitive (procedural-primitive)()
  (:default-initargs
   :fp (rm-cffi::rm-Primitive-New :rm-spheres)))

(defclass cone-primitive (procedural-primitive)()
  (:default-initargs
   :fp (rm-cffi::rm-Primitive-New :rm-cones)))

(defclass cylinder-primitive (procedural-primitive)()
  (:default-initargs
   :fp (rm-cffi::rm-Primitive-New :rm-cylinders)))

(defmethod initialize-instance :around ((self procedural-primitive) &key
					(radius nil) (tesselate nil))
  (call-next-method)
  (when radius
    (setf (radius self) radius))
  (when tesselate
    (setf (tesselate self) tesselate)))

(defmethod (setf radius) ((radius float) (self procedural-primitive))
  (cffi:with-foreign-object (radii :float)
    (setf (cffi:mem-aref radii :float) radius)
    (rm-cffi::rm-Primitive-Set-Radii (fp self)
                                     1 radii
                                     :RM-COPY-DATA
                                     (cffi:null-pointer)))
  self)
(defmethod (setf radius) ((radius list) (self procedural-primitive))
  (let ((size (length radius))
	(radii (cffi:foreign-alloc :float :initial-contents radius)))
    (rm-cffi::rm-Primitive-Set-Radii (fp self)
				     size radii
                                     :RM-COPY-DATA
				     (cffi:null-pointer))
    (cffi:foreign-free radii))
  self)

(defmethod (setf tesselate) (tesselate (self sphere-primitive))
  (rm-cffi::rm-Primitive-Set-Model-Flag (fp self) (case tesselate
						    (8 rm-cffi::+RM-SPHERES-8+)
						    (32 rm-cffi::+RM-SPHERES-32+)
						    (128 rm-cffi::+RM-SPHERES-128+)
						    (512 rm-cffi::+RM-SPHERES-512+)
						    (otherwise rm-cffi::+RM-SPHERES-32+)))
  self)
(defmethod (setf tesselate) (tesselate (self cone-primitive))
  (rm-cffi::rm-Primitive-Set-Model-Flag (fp self) (case tesselate
						    (4 rm-cffi::+RM-CONES-4+)
						    (8 rm-cffi::+RM-CONES-8+)
						    (12 rm-cffi::+RM-CONES-12+)
						    (16 rm-cffi::+RM-CONES-16+)
						    (32 rm-cffi::+RM-CONES-32+)
						    (64 rm-cffi::+RM-CONES-64+)
						    (128 rm-cffi::+RM-CONES-128+)
						    (otherwise rm-cffi::+RM-CONES-32+)))
  self)
(defmethod (setf tesselate) (tesselate (self cylinder-primitive))
  (rm-cffi::rm-Primitive-Set-Model-Flag (fp self) (case tesselate
						    (4 rm-cffi::+RM-CYLINDERS-4+)
						    (8 rm-cffi::+RM-CYLINDERS-8+)
						    (12 rm-cffi::+RM-CYLINDERS-12+)
						    (16 rm-cffi::+RM-CYLINDERS-16+)
						    (32 rm-cffi::+RM-CYLINDERS-32+)
						    (64 rm-cffi::+RM-CYLINDERS-64+)
						    (128 rm-cffi::+RM-CYLINDERS-128+)
						    (otherwise rm-cffi::+RM-CONES-16+)))
  self)

