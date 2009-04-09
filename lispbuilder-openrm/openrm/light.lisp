
(in-package #:rm)

(defvar *default-spot-cutoff* 45.0)

(defclass light (openrm-object) 
  ((source
    :accessor light-source
    :initform :rm-light-0
    :initarg :light-source))
  (:default-initargs
   :fp (rm-cffi::rm-light-new)
    :gc t
    :free (simple-free #'rm-cffi::rm-Light-Delete 'light)))

(defclass light-model (openrm-object) ()
  (:default-initargs
   :fp (rm-cffi::rm-light-Model-New)
    :gc t
    :free (simple-free #'rm-cffi::rm-Light-Model-Delete 'light-model)))

(defclass spotlight (light)())
(defclass point-light (light)())
(defclass directional-light (light)())

(defclass arena-light (point-light)
  ((light-model
    :accessor light-model
    :initform (make-instance 'light-model)
    :initarg :light-model))
  (:default-initargs
   :ambient-color (c4d 0.3 0.3 0.3 1.0)
    :diffuse-color (c4d 0.3 0.3 0.3 1.0)
    :two-sided nil
    :local-viewer t))

(defmethod initialize-instance :after ((self spotlight) &key)
  (rm-cffi::rm-Light-Set-Type (fp self) :RM-LIGHT-SPOT))

(defmethod initialize-instance :after ((self point-light) &key)
  (rm-cffi::rm-Light-Set-Type (fp self) :RM-LIGHT-POINT))

(defmethod initialize-instance :after ((self directional-light) &key)
  (rm-cffi::rm-Light-Set-Type (fp self) :RM-LIGHT-DIRECTIONAL))

(defmethod initialize-instance :around ((self light) &key 
					(ambient-color nil) (diffuse-color nil)
                                        (specular-color nil) (xy/z nil) (exponent nil)
                                        (cutoff nil) (direction nil))
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
					(ambient-color nil) (two-sided nil) (local-viewer nil))
  ;; Give the decendent classes the opportunity to assign a new light-model to :FP
  (call-next-method)
  (when ambient-color
    (setf (ambient-color self) ambient-color))
  (when two-sided
    (setf (two-sided self) two-sided))
  (when local-viewer
    (setf (local-viewer self) two-sided)))

(defmethod initialize-instance :after ((self arena-light) &key
				       (ambient-color nil) (two-sided nil) (local-viewer nil)
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
(defmethod (setf ambient-color) ((color c4d) (light light))
  (rm-cffi::rm-Light-Set-Color (fp light) (fp color) (cffi:null-pointer) (cffi:null-pointer)))
(defmethod ambient-color ((light light))
  (rm-base:with-c4d (col)
    (rm-cffi::rm-Light-Get-Color (fp light) col (cffi:null-pointer) (cffi:null-pointer))
    (color rm-base:r rm-base:g rm-base:b rm-base:a)))

(defmethod (setf ambient-color) ((color c4d) (self light-model))
  (rm-cffi::rm-Light-Model-Set-ambient (fp self) (fp color)))
(defmethod ambient-color ((self light-model))
  (rm-base:with-c4d (col)
    (rm-cffi::rm-Light-Model-Get-ambient (fp self) col)
    (color rm-base:r rm-base:g rm-base:b rm-base:a)))

(defmethod (setf ambient-color) ((color c4d) (light arena-light))
  ;; Reset the light ambient color. The ambient color is only applied to the
  ;; light-model for the ARENA-LIGHT
  (rm-cffi::rm-Light-Set-Color (fp light) (fp (c4d 0.0 0.0 0.0 1.0)) (cffi:null-pointer) (cffi:null-pointer))
  (setf (ambient-color (light-model light)) color))
(defmethod ambient-color ((light arena-light))
  (ambient-color (light-model light)))


;;;
;;; Diffuse Color

(defmethod (setf diffuse-color) ((color c4d) (light light))
  (rm-cffi::rm-Light-Set-Color (fp light) (cffi:null-pointer) (fp color) (cffi:null-pointer)))
(defmethod diffuse-color ((light light))
  (rm-base:with-c4d (col)
    (rm-cffi::rm-Light-Get-Color (fp light) (cffi:null-pointer) col (cffi:null-pointer))
    (color rm-base:r rm-base:g rm-base:b rm-base:a)))

;;;
;;; Specular Color

(defmethod (setf specular-color) ((color c4d) (light light))
  (rm-cffi::rm-Light-Set-Color (fp light) (cffi:null-pointer) (cffi:null-pointer) (fp color)))
(defmethod specular-color ((light light))
  (rm-base:with-c4d (col)
    (rm-cffi::rm-Light-Get-Color (fp light) (cffi:null-pointer) (cffi:null-pointer) col)
    (color rm-base:r rm-base:g rm-base:b rm-base:a)))


(defmethod (setf direction) ((vertex v3d) (self spotlight))
  (rm-cffi::rm-Light-Set-Spot-Direction (fp self) (fp vertex))
  self)
(defmethod direction ((self spotlight))
  (let ((v (rm-cffi::rm-vertex-3d-new 0)))
    (rm-cffi::rm-Light-Get-Spot-Direction (fp self) v)
    (v3d nil nil nil v)))

(defmethod (setf cutoff) (value (self spotlight))
  (rm-cffi::rm-Light-Set-Spot-Cutoff (fp self) value)
  self)
(defmethod cutoff ((self spotlight))
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
  (let ((v (rm-cffi::rm-vertex-3d-new 0)))
    (rm-cffi::rm-Light-Get-XYZ (fp self) v)
    (v3d nil nil nil v)))
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


