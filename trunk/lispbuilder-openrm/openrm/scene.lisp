
(in-package #:rm)

(defclass generic-scene (node)
  ((parent-window
    :reader window
    :initform nil)))

(defclass scene (generic-scene event-handler)()
  (:default-initargs
   :events '(on-idle on-create on-destroy on-paint on-resize on-expose
	     on-quit on-wm-event on-user on-mouse-move on-mouse-up on-mouse-down
	     on-key-down on-key-up on-active on-joy-axis-motion on-joy-button-down
	     on-joy-button-up on-joy-hat-motion on-joy-ball-motion)
   :compute-view-from-geometry t
   :compute-bounding-box t
   :default-lighting t
   :compute-center t
   :union-all t
   :camera (make-instance 'camera-3d :defaults t)))

(defmethod initialize-instance :after ((self scene)
				       &key (window nil))

  ;; Cominations of NODE and WINDOW
  ;; #1 - FP only
  ;; #2 - NODE only
  ;; #3 - FP == root-node
  ;; #4 - WINDOW
  ;; $4.1 - WINDOW == SELF
  ;; #4.2 - WINDOW != SELF

  ;; #1 - FP only
  ;; Do nothing.
  
  ;; #2 - NODE only
  ;; Do nothing.

  ;; #3 - FP == root-node
  (when (root-node-p self)
    (log5:log-for (info) "SCENE set to ROOT-NODE")
    (setf (slot-value self 'parent-window) (rm::rm-root-node)))

  ;; #4 - WINDOW
  (when window
    (add-scene self window nil)
    ;; #4.1 - WINDOW == SELF
    (if (node-eq window self)
	(log5:log-for (info) "SCENE node set to WINDOW node")
	;; #4.2 - WINDOW != SELF
	(unless (root-node-p self)
          (log5:log-for (info) "SCENE node added to WINDOW node")
          (add-to-node window self)))))

(defmethod width ((self scene))
  (width (window self)))
(defmethod height ((self scene))
  (height (window self)))

(defmethod add-to-scene ((self scene) (child node))
  (add-to-node self child))

(defmacro with-scene ((scene scene-var)
                      &body body)
  (let ((body-value (gensym "body-value")))
    `(let* ((,body-value nil)
            (,scene-var ,scene))
       (setf ,body-value (progn
                           ,@body))
       ,body-value)))

;;;
;;; Camera

(defmethod compute-view-from-geometry ((camera camera-3d) (scene scene))
  (rm-cffi::rm-Camera-3D-Compute-View-From-Geometry (fp camera) (fp scene) (width scene) (height scene)))

(defmethod compute-view-from-geometry ((camera camera-2d) (scene scene))
  (rm-cffi::rm-Camera-2D-Compute-View-From-Geometry (fp camera) (fp scene)))

(defmacro with-scene-camera ((scene camera-var)
		       &body body)
  (let ((body-value (gensym "body-value"))
        (scene-value (gensym "scene-value"))
        (camera-value (gensym "camera-value")))
    `(let* ((,body-value nil)
            (,scene-value ,scene)
            (,camera-value (camera-p ,scene)))
       (if ,camera-value
         (let ((,camera-var ,camera-value))
           (setf ,body-value (progn
                               ,@body))
           (setf (camera ,scene-value) ,camera-value)
           (free ,camera-value)
           ,body-value)
         nil))))

;;;
;;; Events

(defmethod on-resize :around ((self scene) width height)
  "Automatically adjusts the camera aspect ratio when the window is resized.
Note that the viewport will only be adjusted when set, i.e. not NIL."
  (let ((camera (camera-p self)))
    (when camera
      (setf (aspect-ratio camera) (vector width height))
      (setf (camera self) camera)))
  ;; (setf (viewport self) (viewport self))
  (call-next-method))
