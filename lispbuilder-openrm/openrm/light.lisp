
(in-package #:rm)

(defvar *default-spot-cutoff* 45.0)

(defclass light (foreign-object) 
  ((source
    :accessor light-source
    :initform :rm-light-0
    :initarg :light-source))
  (:default-initargs
   :fp (rm-cffi::rm-light-new)
    :gc t
    :free (simple-free #'rm-cffi::rm-Light-Delete 'light)))

(defclass light-model (foreign-object) ()
  (:default-initargs
   :fp (rm-cffi::rm-light-Model-New)
    :gc t
    :free (simple-free #'rm-cffi::rm-Light-Model-Delete 'light-model)))

(defclass spot-light (light)())
(defclass point-light (light)())
(defclass directional-light (light)())

(defclass arena-light (point-light)
  ((light-model
    :accessor light-model
    :initform (make-instance 'light-model)
    :initarg :light-model))
  (:default-initargs
   :ambient-color (color 0.3 0.3 0.3 1.0)
   :diffuse-color (color 0.3 0.3 0.3 1.0)
   :two-sided nil
   :local-viewer t))

(defmethod initialize-instance :after ((self spot-light) &key)
  (rm-cffi::rm-Light-Set-Type (fp self) :RM-LIGHT-SPOT))

(defmethod initialize-instance :after ((self point-light) &key)
  (rm-cffi::rm-Light-Set-Type (fp self) :RM-LIGHT-POINT))

(defmethod initialize-instance :after ((self directional-light) &key)
  (rm-cffi::rm-Light-Set-Type (fp self) :RM-LIGHT-DIRECTIONAL))

(defmethod initialize-instance :around ((self light) &key 
					ambient-color diffuse-color
                                        specular-color xy/z exponent
                                        cutoff direction)
  ;; Give the decendent classes the opportunity to assign a new light to :FP
  (call-next-method)
  (when ambient-color
    (setf (ambient-color self) ambient-color))
  (when diffuse-color
    (setf (diffuse-color self) diffuse-color))
  (when specular-color
    (setf (specular-color self) specular-color))
  (when xy/z
    (setf (xy/z self) xy/z))
  (when exponent
    (setf (exponent self) exponent))
  (when cutoff
    (setf (cutoff self) cutoff))
  (when direction
    (setf (direction self) direction)))

(defmethod initialize-instance :around ((self light-model) &key
                                        ambient-color two-sided local-viewer)
  ;; Give the decendent classes the opportunity to assign a new light-model to :FP
  (call-next-method)
  (when ambient-color
    (setf (ambient-color self) ambient-color))
  (when two-sided
    (setf (two-sided self) two-sided))
  (when local-viewer
    (setf (local-viewer self) two-sided)))

(defmethod initialize-instance :after ((self arena-light) &key
                                       ambient-color two-sided local-viewer
				       &allow-other-keys)
  ;; Configure the light-model
  (setf (two-sided (light-model self)) two-sided
	(local-viewer (light-model self)) local-viewer
	(ambient-color (light-model self)) ambient-color))

(defmethod enable ((self light))
  (rm-cffi::rm-light-set-enable (fp self) t))

(defmethod disable ((self light))
  (rm-cffi::rm-light-set-enable (fp self) nil))

(defmethod enable-p ((self light))
  (rm-cffi::rm-light-get-enable (fp self)))

;;;
;;; Ambient Color
(defmethod (setf ambient-color) ((color vector) (light light))
  (with-copy-color-4d-to-foreign (color fp)
    (rm-cffi::rm-Light-Set-Color (fp light) fp (cffi:null-pointer) (cffi:null-pointer))))
(defmethod (setf ambient-color) ((color c4d) (light light))
  (rm-cffi::rm-Light-Set-Color (fp light) (fp color) (cffi:null-pointer) (cffi:null-pointer)))

(defmethod ambient-color ((light light))
  (rm-base:with-c4d (col)
    (rm-cffi::rm-Light-Get-Color (fp light) col (cffi:null-pointer) (cffi:null-pointer))
    (color rm-base:r rm-base:g rm-base:b rm-base:a)))
(defmethod ambient-color* ((light light))
  (let ((col (c4d nil nil nil nil)))
    (rm-cffi::rm-Light-Get-Color (fp light) (fp col) (cffi:null-pointer) (cffi:null-pointer))
    col))

(defmethod (setf ambient-color) ((color vector) (self light-model))
  (with-copy-color-4d-to-foreign (color fp)
    (rm-cffi::rm-Light-Model-Set-ambient (fp self) fp)))
(defmethod (setf ambient-color) ((color c4d) (self light-model))
  (rm-cffi::rm-Light-Model-Set-ambient (fp self) (fp color)))

(defmethod ambient-color ((self light-model))
  (rm-base:with-c4d (col)
    (rm-cffi::rm-Light-Model-Get-ambient (fp self) col)
    (color rm-base:r rm-base:g rm-base:b rm-base:a)))
(defmethod ambient-color* ((self light-model))
  (let ((col (c4d nil nil nil nil)))
    (rm-cffi::rm-Light-Model-Get-ambient (fp self) (fp col))
    col))

(defmethod (setf ambient-color) ((color vector) (light arena-light))
  ;; Reset the light ambient color. The ambient color is only applied to the
  ;; light-model for the ARENA-LIGHT
  (rm-base:with-c4d (col)
    (setf rm-base:r 0.0
          rm-base:g 0.0
          rm-base:b 0.0
          rm-base:a 1.0)
    (rm-cffi::rm-Light-Set-Color (fp light) col (cffi:null-pointer) (cffi:null-pointer)))
  (setf (ambient-color (light-model light)) color))
(defmethod (setf ambient-color) ((color c4d) (light arena-light))
    ;; Reset the light ambient color. The ambient color is only applied to the
  ;; light-model for the ARENA-LIGHT
  (rm-base:with-c4d (col)
    (setf rm-base:r 0.0
          rm-base:g 0.0
          rm-base:b 0.0
          rm-base:a 1.0)
    (rm-cffi::rm-Light-Set-Color (fp light) col (cffi:null-pointer) (cffi:null-pointer))))

(defmethod ambient-color ((light arena-light))
  (ambient-color (light-model light)))
(defmethod ambient-color* ((light arena-light))
  (ambient-color* (light-model light)))

;;;
;;; Diffuse Color
(defmethod (setf diffuse-color) ((color vector) (light light))
  (with-copy-color-4d-to-foreign (color fp)
    (rm-cffi::rm-Light-Set-Color (fp light) (cffi:null-pointer) fp (cffi:null-pointer))))
(defmethod (setf diffuse-color) ((color c4d) (light light))
  (rm-cffi::rm-Light-Set-Color (fp light) (cffi:null-pointer) (fp color) (cffi:null-pointer)))

(defmethod diffuse-color ((light light))
  (rm-base:with-c4d (col)
    (rm-cffi::rm-Light-Get-Color (fp light) (cffi:null-pointer) col (cffi:null-pointer))
    (color rm-base:r rm-base:g rm-base:b rm-base:a)))
(defmethod diffuse-color* ((light light))
  (let ((col (c4d nil nil nil nil)))
    (rm-cffi::rm-Light-Get-Color (fp light) (cffi:null-pointer) (fp col) (cffi:null-pointer))
    col))
  
;;;
;;; Specular Color

(defmethod (setf specular-color) ((color vector) (light light))
  (with-copy-color-4d-to-foreign (color fp)
    (rm-cffi::rm-Light-Set-Color (fp light) (cffi:null-pointer) (cffi:null-pointer) fp)))
(defmethod (setf specular-color) ((color c4d) (light light))
  (rm-cffi::rm-Light-Set-Color (fp light) (cffi:null-pointer) (cffi:null-pointer) (fp color)))


(defmethod specular-color ((light light))
  (rm-base:with-c4d (col)
    (rm-cffi::rm-Light-Get-Color (fp light) (cffi:null-pointer) (cffi:null-pointer) col)
    (color rm-base:r rm-base:g rm-base:b rm-base:a)))
(defmethod specular-color* ((light light))
  (let ((col (c4d nil nil nil nil)))
    (rm-cffi::rm-Light-Get-Color (fp light) (cffi:null-pointer) (cffi:null-pointer) (fp col))
    col))

(defmethod (setf direction) ((vertex vector) (self spot-light))
  (with-copy-vertex-3d-to-foreign (vertex fp)
    (rm-cffi::rm-Light-Set-Spot-Direction (fp self) fp)))
(defmethod (setf direction) ((vertex v3d) (self spot-light))
  (rm-cffi::rm-Light-Set-Spot-Direction (fp self) (fp vertex))
  self)

(defmethod direction ((self spot-light))
  (rm-base:with-v3d (v)
    (rm-cffi::rm-Light-Get-Spot-Direction (fp self) v)
    (vertex rm-base::x rm-base::y rm-base::z)))
(defmethod direction* ((self spot-light))
  (let ((v (v3d nil nil nil)))
    (when (rm-cffi::rm-Light-Get-Spot-Direction (fp self) (fp v))
      v)))

(defmethod (setf cutoff) (value (self spot-light))
  (rm-cffi::rm-Light-Set-Spot-Cutoff (fp self) value)
  self)
(defmethod cutoff ((self spot-light))
  (cffi:with-foreign-object (v :float)
    (rm-cffi::rm-Light-Get-Spot-Cutoff (fp self) v)
    (cffi:mem-aref v :float)))

(defmethod (setf exponent) (value (self light))
  (rm-cffi::rm-Light-Set-Spot-Exponent (fp self) value)
  self)
(defmethod exponent ((self light))
  (cffi:with-foreign-object (v :float)
    (rm-cffi::rm-Light-Get-Spot-Exponent (fp self) v)
    (cffi:mem-aref v :float)))

(defmethod xy/z ((self light))
  (rm-base:with-v3d (v)
    (when (rm-cffi::rm-Light-Get-XYZ (fp self) v)
      (vertex rm-base::x rm-base::y rm-base::z))))
(defmethod xy/z* ((self light))
  (let ((v (v3d nil nil nil)))
    (when (rm-cffi::rm-Light-Get-XYZ (fp self) (fp v))
      v)))

(defmethod (setf xy/z) ((vertex vector) (self light))
  ;; A single vertex
  (if (> (length vertex) 2)
    ;; xyz
    (with-copy-vertex-3d-to-foreign (vertex fp)
      (rm-cffi::rm-Light-Set-XYZ (fp self) fp))
    ;; xy
    (with-copy-vertex-2d-to-foreign (vertex fp)
      (rm-cffi::rm-Light-Set-XYZ (fp self) fp))))
(defmethod (setf xy/z) ((vertex v3d) (self light))
  (rm-cffi::rm-Light-Set-XYZ (fp self) (fp vertex)))

(defmethod two-sided ((self light-model))
  (rm-cffi::rm-Light-Model-Get-Two-Sided (fp self)))
(defmethod (setf two-sided) (value (self light-model))
  (rm-cffi::rm-Light-Model-Set-Two-Sided (fp self) value))

(defmethod local-viewer ((self light-model))
  (rm-cffi::rm-Light-Model-Get-Local-Viewer (fp self)))
(defmethod (setf local-viewer) (value (self light-model))
  (rm-cffi::rm-Light-Model-Set-Local-Viewer (fp self) value))


