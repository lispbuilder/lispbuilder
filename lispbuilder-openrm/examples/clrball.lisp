
(in-package #:rm-examples)

(defvar *window* nil)

(defclass clrball-window (rm::native-window rm::aux-window) ()
  (:default-initargs
   :width 400
   :height 300
   :name "window"
   :events '(rm::on-idle)))

(defclass clrball-scene (rm::scene rm::aux-trackball rm::aux-scene)()
  (:default-initargs
   :name "scene"
   :dims :renderpass-3D
   :opacity :renderpass-all
   :compute-view-from-geometry t
   :default-lighting t
   :viewport t
   :compute-bounding-box t
   :compute-center t
   :union-all t
   :camera (make-instance 'rm::camera-3d :defaults t)))

(defclass transparent-node (rm::node) ()
  (:default-initargs
   :dims :renderpass-3d
   :opacity :renderpass-transparent))

(defclass opaque-node (rm::node) ()
  (:default-initargs
   :dims :renderpass-3d
   :opacity :renderpass-opaque))

(defclass transparent-sphere (rm::sphere-primitive) ()
  (:default-initargs
   :xy/z (rm::v3d 0.0 0.0 0.0)
    :radius 3.0
    :tesselate 512))

(defclass opaque-sphere (rm::sphere-primitive) ()
  (:default-initargs
   :radius 1.0
    :xy/z (rm::v3d 0.0 0.0 0.0)
    :tesselate 32
    :rgb/a (rm::c4d 1.0 0.0 0.0 1.0)))

;;(defmethod rm::on-idle ((window clrball-window))
;;  (rm::dispatch-event *window* rm::on-paint))

(defun clrball ()
  (setf *window* (rm::make-instance 'clrball-window))
  (let* ((opaque-node (rm::make-instance 'opaque-node :name "opaque"
					 :background-color (rm::c4d 0.0 0.0 0.0 0.0)
					 :children (list (make-instance 'opaque-node
									:compute-bounding-box t
									:primitives (list (make-instance 'opaque-sphere))))))
	 (transparent-node (rm::make-instance 'transparent-node :name "transparent"
					      :children (list (make-instance 'transparent-node
									     :diffuse-color (rm::c4d 0.1 0.1 1.0 0.75)
									     :ambient-color (rm::c4d 1.0 1.0 1.0 0.5)
									     :specular-color (rm::c4d 0.1 0.1 1.0 1.0)
									     :specular-exponent 20.0
									     :compute-bounding-box t
									     :primitives (list (make-instance 'transparent-sphere)))))))
    (rm::make-instance 'clrball-scene
		       :window *window*
		       :children (list transparent-node
				       opaque-node))
    
    (rm::show-window *window*)
    (rm::process-events :poll)
    ;; Attempt to clean up any stray windows
    (rm::clean-up)))

