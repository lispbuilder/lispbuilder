
(in-package #:lispbuilder-openrm)

(defclass trackball ()
  ((dolly
    :accessor dolly
    :initform (make-instance 'dolly))
   (arc
    :accessor arc
    :initform (make-instance 'arc))
   (translate
    :accessor translate
    :initform (make-instance 'translate))))

(defclass aux-trackball (generic-scene)
  ((trackball
    :accessor trackball
    :initform (make-instance 'rm::trackball)
    :initarg :trackball)))

(defclass aux-window (generic-window)())

(defclass aux-scene (generic-scene)())

(defgeneric dolly-update (aux-trackball y))
(defmethod dolly-update ((aux-trackball aux-trackball) y)
  (update-dolly (dolly (trackball aux-trackball))
	       aux-trackball
	       (height aux-trackball)
	       y))

(defgeneric dolly-reset (aux-trackball y))
(defmethod dolly-reset ((aux-trackball aux-trackball) y)
  (reset-dolly (dolly (trackball aux-trackball))
	       (height aux-trackball)
	       y))

(defgeneric arc-update (aux-trackball x y))
(defmethod arc-update ((aux-trackball aux-trackball) x y)
  (update-arc (arc (trackball aux-trackball))
	     aux-trackball
	     (width aux-trackball)
	     (height aux-trackball)
	     x y))

(defgeneric arc-reset (aux-trackball x y))
(defmethod arc-reset ((aux-trackball aux-trackball) x y)
  (reset-arc (arc (trackball aux-trackball))
	     aux-trackball
	     (width aux-trackball)
	     (height aux-trackball)
	     x y))

(defgeneric translate-update (aux-trackball x y))
(defmethod translate-update ((aux-trackball aux-trackball) x y)
  (udpate-translate (translate (trackball aux-trackball))
		   aux-trackball
		   (width aux-trackball)
		   (height aux-trackball)
		    x y))

(defgeneric translate-reset (aux-trackball x y))
(defmethod translate-reset ((aux-trackball aux-trackball) x y)
  (reset-translate (translate (trackball aux-trackball))
		   (width aux-trackball)
		   (height aux-trackball)
		   x y))

(defclass dolly ()
  ((prev-button-y :accessor y-of :initform 0.0)))

(defclass arc ()
  ((prev-button-x :accessor x-of :initform 0.0)
   (prev-button-y :accessor y-of :initform 0.0)
   (initial-transform :accessor initial-transform-of :initform (rm-cffi::rm-matrix-new))
   (result-transform :accessor result-transform-of :initform (rm-cffi::rm-matrix-new))))

(defclass translate ()
  ((prev-button-x :accessor x-of :initform 0.0)
   (prev-button-y :accessor y-of :initform 0.0)))

(defgeneric update-dolly (dolly node screen-height mousey))
(defmethod update-dolly ((dolly dolly) (node node) screen-height mousey)
  (cffi:with-foreign-object (h-pointer :pointer)
    (let ((current-y (* -1.0 (pixel-to-viewport mousey screen-height))))
      (when (rm-cffi::rm-node-get-scene-camera-3d (fp node) h-pointer)
	(let ((camera-pointer (cffi:mem-aref h-pointer :pointer)))
	  (cffi:with-foreign-objects ((x :float) (y :float) (prev-x :float) (prev-y :float))
	    (setf (cffi:mem-aref x :float) 0.0
		  (cffi:mem-aref y :float) (y-of dolly)
		  (cffi:mem-aref prev-x :float) 0.0
		  (cffi:mem-aref prev-y :float) current-y)
	    (rm-cffi::rmaux-dolly camera-pointer x y prev-x prev-y))
	  (rm-cffi::rm-Node-Set-Scene-Camera-3D (fp node) camera-pointer)
	  (rm-cffi::rm-Camera-3D-Delete camera-pointer)
	  (setf (y-of dolly) current-y))))))

(defgeneric reset-dolly (dolly screen-height y))
(defmethod reset-dolly ((dolly dolly) screen-height y)
  (setf (y-of dolly) (* -1.0 (rm::pixel-to-viewport y screen-height))))

(defgeneric update-arc (arc node screen-width screen-height mousex mousey))
(defmethod update-arc ((arc arc) (node node) screen-width screen-height mousex mousey)
  (let ((current-x (pixel-to-viewport mousex screen-width))
	(current-y (* -1.0 (pixel-to-viewport mousey screen-height)))
	(initial-transform (initial-transform-of arc))
	(result-transform (result-transform-of arc)))
    (cffi:with-foreign-objects ((x :float) (y :float) (prev-x :float) (prev-y :float))
      (setf (cffi:mem-aref x :float) (x-of arc)
	    (cffi:mem-aref y :float) (y-of arc)
	    (cffi:mem-aref prev-x :float) current-x
	    (cffi:mem-aref prev-y :float) current-y)
      (rm-cffi::rmaux-arc-ball x y prev-x prev-y result-transform))
    (rm-cffi::rm-Matrix-Multiply initial-transform result-transform result-transform)
    (rm-cffi::rm-Node-Set-Rotate-Matrix (fp node) result-transform)))

(defgeneric reset-arc (arc node screen-width screen-height x y))
(defmethod reset-arc ((arc arc) (node node) screen-width screen-height x y)
  (setf (x-of arc) (pixel-to-viewport x screen-width)
	(y-of arc) (* -1.0 (pixel-to-viewport y screen-height)))
  (when (eq (rm-cffi::rm-Node-Get-Rotate-Matrix (fp node) (initial-transform-of arc)) :rm-false)
    (rm-cffi::rm-Matrix-Identity (initial-transform-of arc))))

(defgeneric udpate-translate (translate node screen-width screen-height mousex mousey))
(defmethod udpate-translate ((translate translate) (node node) screen-width screen-height mousex mousey)
  (cffi:with-foreign-object (h-pointer :pointer)
    (let ((current-x (pixel-to-viewport mousex screen-width))
	  (current-y (* -1.0 (pixel-to-viewport mousey screen-height))))
      (when (rm-cffi::rm-node-get-scene-camera-3d (fp node) h-pointer)
	(let ((camera-pointer (cffi:mem-aref h-pointer :pointer)))
	  (cffi:with-foreign-objects ((x :float) (y :float) (prev-x :float) (prev-y :float))
	    (setf (cffi:mem-aref x :float) (x-of translate)
		  (cffi:mem-aref y :float) (y-of translate)
		  (cffi:mem-aref prev-x :float) current-x
		  (cffi:mem-aref prev-y :float) current-y)		      
	    (rm-cffi::rmaux-translate camera-pointer x y prev-x prev-y))
	  (rm-cffi::rm-Node-Set-Scene-Camera-3D (fp node) camera-pointer)
	  (rm-cffi::rm-Camera-3D-Delete camera-pointer)
	  (setf (x-of translate) current-x
		(y-of translate) current-y))))))

(defgeneric reset-translate (translate screen-width screen-height x y))
(defmethod reset-translate ((translate translate) screen-width screen-height x y)
  (setf (x-of translate) (pixel-to-viewport x screen-width)
	(y-of translate) (* -1.0 (pixel-to-viewport y screen-height))))


;;;
;;; Events

(defmethod on-mouse-down :around ((self aux-trackball) button x y)
  (when (button= button :button-right)
    (dolly-reset self y))
  (when (button= button :button-middle)
    (arc-reset self x y))
  (when (button= button :button-left)
    (translate-reset self x y))
  (call-next-method))

(defmethod on-mouse-move :around ((self aux-trackball) button-state x y x-rel y-rel)
  (when (button= button-state :button-right) 
    (dolly-update self y))
  (when (button= button-state :button-middle) 
    (arc-update self x y))
  (when (button= button-state :button-left) 
    (translate-update self x y))
  (call-next-method))

