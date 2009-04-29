
(in-package #:rm)

(defclass image (foreign-object copyable-object) ()
  (:default-initargs
   :fp nil
   :free (simple-free #'rm-cffi::rm-image-delete 'rm-image)))

(defmethod initialize-instance :after ((self image) &key
                                       type dims depth format data-type image-data)
  (unless (this-fp self)
    (unless (or type dims depth format type)
      (error ":TYPE, :DIMS, :DEPTH, :FORMAT or :DATA-TYPE must not be NIL."))
    (setf (slot-value self 'foreign-pointer-to-object)
          (rm-cffi::rm-image-new type (elt dims 0) (elt dims 1) depth format data-type (copy-data self))))
  (when image-data
    (setf (image-data self) image-data)))

(defmethod image-dimensions ((self image))
  (cffi:with-foreign-object (v :int)
    (if (rm-cffi::rm-image-get-image-size (fp self) v
                                          (cffi:null-pointer) (cffi:null-pointer)
                                          (cffi:null-pointer) (cffi:null-pointer)
                                          (cffi:null-pointer))
      (cffi:mem-aref v :int)
      nil)))

(defmethod image-dims ((self image))
  (cffi:with-foreign-objects ((width :int) (height :int))
    (if (rm-cffi::rm-image-get-image-size (fp self) (cffi:null-pointer)
                                          width height
                                          (cffi:null-pointer) (cffi:null-pointer)
                                          (cffi:null-pointer))	
	(vector (cffi:mem-aref width :int) (cffi:mem-aref height :int))
	nil)))

(defmethod image-depth ((self image))
  (cffi:with-foreign-object (depth :int)
    (if (rm-cffi::rm-image-get-image-size (fp self) (cffi:null-pointer)
                                          (cffi:null-pointer) (cffi:null-pointer)
                                          depth (cffi:null-pointer)
                                          (cffi:null-pointer))
	(cffi:mem-aref depth :int)
	nil)))

(defmethod image-elements ((self image))
  (cffi:with-foreign-object (elements :int)
    (if (rm-cffi::rm-image-get-image-size (fp self) (cffi:null-pointer)
                                          (cffi:null-pointer) (cffi:null-pointer)
                                          (cffi:null-pointer) elements
                                          (cffi:null-pointer))
	(cffi:mem-aref elements :int)
	nil)))

(defmethod image-bps ((self image))
  (cffi:with-foreign-object (bps :int)
    (if (rm-cffi::rm-image-get-image-size (fp self) (cffi:null-pointer)
                                          (cffi:null-pointer) (cffi:null-pointer)
                                          (cffi:null-pointer) (cffi:null-pointer)
                                          bps)
	(cffi:mem-aref bps :int)
	nil)))

(defmethod image-zoom ((self image))
  (cffi:with-foreign-objects ((x-zoom :float) (y-zoom :float))
    (if (rm-cffi::rm-image-get-pixel-zoom (fp self) x-zoom y-zoom)
	(vector (cffi:mem-aref x-zoom :float) (cffi:mem-aref y-zoom :float))
	nil)))
(defmethod (setf image-zoom) ((zoom vector) (self image))
  (rm-cffi::rm-image-set-pixel-zoom (fp self) (svref zoom 0) (svref zoom 1)))

(defmethod image-scale ((self image))
  (cffi:with-foreign-object (scale :float)
    (if (rm-cffi::rm-image-get-scale (fp self) scale)
	(cffi:mem-aref scale :float)
	nil)))
(defmethod (setf image-scale) (scale (self image))
  (rm-cffi::rm-image-set-scale (fp self) scale))

(defmethod image-bias ((self image))
  (cffi:with-foreign-object (bias :float)
    (if (rm-cffi::rm-image-get-bias (fp self) bias)
	(cffi:mem-aref bias :float)
	nil)))
(defmethod (setf image-bias) (bias (self image))
  (rm-cffi::rm-image-set-bias (fp self) bias))

(defmethod image-duplicate ((self image))
  (let ((dup (rm-cffi::rm-image-dup (fp self))))
    (if (cffi:null-pointer-p dup)
	nil)
	(make-instance 'image :fp dup)))

(defmethod image-resize ((src image) (dst image) (pipe pipe) hardware)
  (if (rm-cffi::rm-image-resize (fp src) (fp dst) hardware (fp pipe))
      dst
      nil))

(defmethod image-mirror ((self image) axis)
  (if (rm-cffi::rm-image-mirror (fp self) axis)
      self
      nil))

(defmethod image-data ((self image))
  (rm-cffi::rm-image-get-pixel-data (fp self)))

(defmethod (setf image-data) :after ((pixels foreign-object) (self image))
  (if (gc-p pixels)
    (setf (gc-p pixels) nil)))

(defmethod (setf image-data) (pixels (self image))
  (error "Don't know how to handle this type of PIXEL data."))

(defmethod (setf image-data) ((pixels color-array) (self image))
  (rm-cffi::rm-image-set-pixel-data (fp self)
                                    (fp pixels)
                                    (copy-data self)
                                    (if (copy-p self)
                                      (cffi:null-pointer)
                                      (free-callback self))))

(defmethod (setf image-data) ((pixels vector) (self image))
  (if (> (length (aref pixels 0)) 3)
    (cffi:with-foreign-object (pixel-array 'rm-cffi::rm-color-4d (length pixels))
      (let ((size (cffi:foreign-type-size 'rm-cffi::rm-color-4d))
            (color-fp pixel-array))
        (loop
         for vector across pixels
         for i from 0 upto (1- (length pixels))
         do (cffi:with-foreign-slots ((rm-cffi::r rm-cffi::g rm-cffi::b rm-cffi::a)
                                      color-fp rm-cffi::rm-color-4d)
              (setf rm-cffi::r (r vector)
                    rm-cffi::g (g vector)
                    rm-cffi::b (b vector)
                    rm-cffi::a (a vector))
              (setf color-fp (cffi:inc-pointer color-fp size)))))
      
      (rm-cffi::rm-image-set-pixel-data (fp self)
                                        pixel-array
                                        :rm-copy-data
                                        (cffi:null-pointer)))
    (cffi:with-foreign-object (pixel-array 'rm-cffi::rm-color-3d (length pixels))
      (let ((size (cffi:foreign-type-size 'rm-cffi::rm-color-3d))
            (color-fp pixel-array))
        (loop
         for vector across pixels
         for i from 0 upto (1- (length pixels))
         do (cffi:with-foreign-slots ((rm-cffi::r rm-cffi::g rm-cffi::b)
                                      color-fp rm-cffi::rm-color-3d)
              (setf rm-cffi::r (r vector)
                    rm-cffi::g (g vector)
                    rm-cffi::b (b vector))
              (setf color-fp (cffi:inc-pointer color-fp size)))))
      (rm-cffi::rm-image-set-pixel-data (fp self)
                                        pixel-array
                                        :rm-copy-data
                                        (cffi:null-pointer)))))

(defmethod width ((dims vector))
  (svref dims 0))
(defmethod height ((dims vector))
  (svref dims 1))

(defmethod width ((self image))
  (width (image-dims self)))
(defmethod height ((self image))
  (height (image-dims self)))

(defmethod set-sprites ((self sprite-primitive) (image image))
  (set-sprites self (list image)))

;; Cancel finalization for images added to a sprite. 
(defmethod set-sprites :after ((self sprite-primitive) (images list))
  (loop for image in images
        do (when (gc-p image) (setf (gc-p image) nil))))

(defmethod set-sprites ((self sprite-primitive) (images list))
  (cffi:with-foreign-object (handle :pointer)
    (let ((result nil)
          (image-array (cffi:foreign-alloc 'rm-cffi::rm-image
                                           :initial-contents (loop for image in images
                                                                   collect (fp image)))))
      ;;(setf (cffi:mem-aref handle :pointer) image-array)
      (if (rm-cffi::rm-primitive-set-sprites (fp self) (length images)
                                             image-array
                                             ;;handle
					     )
        (setf result t)
        (setf result nil))
      (cffi:foreign-free image-array)
      result)))
