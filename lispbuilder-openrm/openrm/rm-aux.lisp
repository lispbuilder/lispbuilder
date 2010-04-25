
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
    :initform (make-instance 'translate))
   (enable-arc
    :accessor enable-arc-p
    :initarg :enable-arc)
   (enable-dolly
    :accessor enable-dolly-p
    :initarg :enable-dolly)
   (enable-translate
    :accessor enable-translate-p
    :initarg :enable-translate))
  (:default-initargs
   :enable-arc :button-middle
   :enable-dolly :button-right
   :enable-translate :button-left
   :scene nil))

(defclass aux-trackball (generic-scene)
  ((trackball
    :reader trackball)))

(defclass aux-pipe (pipe)()
  (:default-initargs
   :target :PIPE-WGL ;; This needs to be dependent on OS
   :processing-mode :PIPE-MULTISTAGE
   :opaque-3d t
   :transparent-3d t
   :opaque-2d t
   :swap-buffers nil
   :notify-level :notify-full))

(defclass aux-window (window)
  ((window :initarg window :accessor window))
  (:default-initargs
   :pipe (make-instance 'aux-pipe)))

(defclass aux-scene (generic-scene)())

;;(defmethod initialize-instance :after ((window aux-window)
;;                                       &key
;;                                       title-caption width height
;;                                       &allow-other-keys)
;;  
;;      ;; 2.2 create an OpenGL context
;;      ;; 3. Create the window
;;      (setf (window self)
;;            (rm-cffi::rmaux-create-w-32window (fp (pipe self))
;;                                              (cffi:null-pointer)
;;                                              0 0 width height
;;                                              title-caption
;;                                               
;;                                              sdl:window width height
;;                                              :bpp bpp
;;                                              :flags window-flags
;;                                              :title-caption title-caption
;;                                              :icon-caption icon-caption)))


(defmethod initialize-instance :after ((self trackball)
                                       &key scene target dimensions &allow-other-keys)
  (when scene
    (setf (scene (dolly self)) scene
          (scene (translate self)) scene
          (scene (arc self)) scene))
  (when target
    (setf (target-node (arc self)) target))
  (when dimensions
    (setf (dimensions (dolly self)) dimensions
          (dimensions (translate self)) dimensions
          (dimensions (arc self)) dimensions)))

(defmethod initialize-instance :after ((self aux-trackball)
                                       &key &allow-other-keys)
  (setf (slot-value self 'trackball)
        (make-instance 'trackball
                       :scene self
                       :target self)))

(defgeneric dolly-update (trackball y))
(defgeneric dolly-reset (trackball y))
(defgeneric arc-update (trackball x y))
(defgeneric arc-reset (trackball x y))
(defgeneric translate-update (trackball x y))
(defgeneric translate-reset (trackball x y))

(defmethod dolly-update ((self trackball) y)
  (update-dolly (dolly self)
                (y-to-viewport (dolly self) y)))

(defmethod dolly-reset ((self trackball) y)
  (reset-dolly (dolly self)
               (y-to-viewport (dolly self) y)))

(defmethod arc-update ((self trackball) x y)
  (update-arc (arc self)
              (x-to-viewport (arc self) x)
              (y-to-viewport (arc self) y)))

(defmethod arc-reset ((self trackball) x y)
  (reset-arc (arc self)
             (x-to-viewport (arc self) x)
             (y-to-viewport (arc self) y)))

(defmethod translate-update ((self trackball) x y)
  (update-translate (translate self)
                    (x-to-viewport (translate self) x)
                    (y-to-viewport (translate self) y)))

(defmethod translate-reset ((self trackball) x y)
  (reset-translate (translate self)
                   (x-to-viewport (translate self) x)
                   (y-to-viewport (translate self) y)))

(defclass arcball ()
  ((scene-node :accessor scene :initarg :scene :initform nil)
   (dimensions :accessor dimensions :initarg :dimensions :initform nil)))

(defclass dolly (arcball)
  ((prev-button-y :accessor y-of :initform 0.0)))

(defclass arc (arcball)
  ((prev-button-x :accessor x-of :initform 0.0)
   (prev-button-y :accessor y-of :initform 0.0)
   (initial-transform :accessor initial-transform-of :initform (rm-cffi::rm-matrix-new))
   (result-transform :accessor result-transform-of :initform (rm-cffi::rm-matrix-new))
   (dummy-node :reader dummy-node :initform (make-instance 'node))
   (target-node :accessor target-node :initarg :target :initform (make-instance 'node))))

(defclass translate (arcball)
  ((prev-button-x :accessor x-of :initform 0.0)
   (prev-button-y :accessor y-of :initform 0.0)))

(defmethod width ((self arcball))
  (if (scene self)
    (width (scene self))
    (width (dimensions self))))

(defmethod height ((self arcball))
  (if (scene self)
    (height (scene self))
    (height (dimensions self))))

(defmethod x-to-viewport ((self arcball) x)
  (pixel-to-viewport x (width self)))

(defmethod y-to-viewport ((self arcball) y)
  (* -1.0 (pixel-to-viewport y (height self))))

(defgeneric update-dolly (dolly y))
(defmethod update-dolly ((dolly dolly) current-y)
  (let ((node (scene dolly)))
    (cffi:with-foreign-object (h-pointer :pointer)
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

(defgeneric reset-dolly (dolly y))
(defmethod reset-dolly ((dolly dolly) y)
  (setf (y-of dolly) y))

(defgeneric update-arc (arc current-x current-y))
(defmethod update-arc ((arc arc) current-x current-y)
  (let ((node (dummy-node arc))
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

(defgeneric reset-arc (arc x y))
(defmethod reset-arc ((arc arc) x y)
  (setf (x-of arc) x
	(y-of arc) y)
  (when (eq (rm-cffi::rm-Node-Get-Rotate-Matrix (fp (dummy-node arc))
                                                (initial-transform-of arc)) :rm-false)
    (rm-cffi::rm-Matrix-Identity (initial-transform-of arc))))

(defgeneric update-translate (translate current-x current-y))
(defmethod update-translate ((translate translate) current-x current-y)
  (let ((node (scene translate)))
    (cffi:with-foreign-object (h-pointer :pointer)
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

(defgeneric reset-translate (translate x y))
(defmethod reset-translate ((translate translate) x y)
  (setf (x-of translate) x
	(y-of translate) y))

(defmethod trackball-mouse-down ((self trackball) button x y)
  (when (button= button (enable-dolly-p self))
    (dolly-reset self y))
  (when (button= button (enable-arc-p self))
    (arc-reset self x y))
  (when (button= button (enable-translate-p self))
    (translate-reset self x y)))

(defmethod trackball-mouse-move ((self trackball) button x y)
  (when (button= button (enable-dolly-p self))
    (dolly-update self y))
  (when (button= button (enable-arc-p self)) 
    (arc-update self x y)
    (rotate (target-node (arc self)) (dummy-node (arc self))))
  (when (button= button (enable-translate-p self)) 
    (translate-update self x y)))

;;;
;;; Events

(defmethod on-mouse-down :around ((self aux-trackball) button x y)
  (trackball-mouse-down (trackball self) button x y)
  (call-next-method))

(defmethod on-mouse-move :around ((self aux-trackball) button-state x y x-rel y-rel)
  (trackball-mouse-move (trackball self) button-state x y)
  (call-next-method))

