
(in-package #:rm-examples)

(defvar *window* nil)

(defclass cones-window (rm::native-window rm::aux-window) ()
  (:default-initargs
   :width 400
    :height 300
    :name "window"
    :events '(rm::on-idle)
    :pipe (make-instance 'rm::native-pipe :channel-format :REDBLUE-STEREO-CHANNEL)))

(defclass cones-scene (rm::scene rm::aux-trackball rm::aux-scene)()
  (:default-initargs
   :name "scene"
    :dims :renderpass-3D
    :opacity :renderpass-opaque
    :compute-view-from-geometry t
    :default-lighting t
    :viewport t
    :union-all t
    :background-color (rm::c4d 0.4 0.4 0.4 1.0)
    :unlit-color (rm::c4d 1.0 1.0 1.0 1.0)
    :camera (make-instance 'rm::camera-3d :stereo t :eye-separation 2.5 :focal-distance 0.707 :defaults t)))

(defmethod rm::on-idle ((window cones-window))
  (rm::dispatch-event *window* rm::on-paint))

(defun cones ()
  (setf *window* (rm::make-instance 'cones-window))
  (let* ((cone-node (rm::make-instance 'rm::node :name "cone-node"
				       :compute-bounding-box t
				       :primitives (list (make-instance 'rm::cone-primitive
									:xy/z (rm::v3d* '((5.0 0.0 0.0)
											  (8.0 0.0 0.0)
											  (0.0 5.0 0.0)
											  (0.0 8.0 0.0)
											  (0.0 0.0 5.0)
											  (0.0 0.0 8.0)))
									:radius '(1.0 1.0 1.0)
									:rgb/a (rm::c4d* '((1.0 0.0 0.0 1.0)
											   (0.0 1.0 0.0 1.0)
											   (0.0 0.0 1.0 0.0)))))))
	 (cylinder-node (rm::make-instance 'rm::node :name "cylinder-node"
					   :compute-bounding-box t
					   :primitives (list (make-instance 'rm::cylinder-primitive
									    :xy/z (rm::v3d* '((0.0 0.0 0.0) (4.0 0.0 0.0)
											      (0.0 0.0 0.0) (0.0 4.0 0.0)
											      (0.0 0.0 0.0) (0.0 0.0 4.0)))
									    :radius '(0.3 0.3 0.3)
									    :rgb/a (rm::c4d* '((1.0 1.0 1.0 1.0)
											       (1.0 1.0 1.0 1.0)
											       (1.0 1.0 1.0 1.0)))))))
	 (sphere-node (rm::make-instance 'rm::node :name "sphere-node"
					 :compute-bounding-box t
					 :primitives (list (make-instance 'rm::sphere-primitive
									    :xy/z (rm::v3d 0.0 0.0 0.0)
									    :radius 0.3
									    :rgb/a (rm::c4d 1.0 1.0 1.0 1.0))))))
    (rm::make-instance 'cones-scene
		       :window *window*
		       :children (list cone-node cylinder-node sphere-node))

    (rm::show-window *window*)
    (rm::process-events :poll)
    ;; Attempt to clean up any stray windows
    (rm::clean-up)))

