

(defclass window (rm::native-window rm::aux-window)()
  (:default-initargs
   :width 320
    :height 240
    :name "window"
    :events '(rm::on-idle rm::on-mouse-move rm::on-mouse-down)))

(defclass camera (rm::camera-3d) ()
  (:default-initargs
   :eye (rm::v3d 5.0 5.0 22.0)
    :at (rm::v3d 5.0 5.0 0.0)
    :up-vector (rm::v3d 0.0 1.0 0.0)
    :hither 5.0
    :yon 40.0
    :fov 45.0
    :aspect-ratio (vector 320 240)
    :projection :rm-projection-perspective))

(defclass scene (rm::scene rm::aux-scene) ()
  (:default-initargs
   :name "scene"
    :viewport t
    :default-camera t
    :dims :rm-renderpass-3d
    :opacity :rm-renderpass-opaque))

(defun test ()
  (let* ((window (make-instance 'window
				:node (rm::rm-root-node)))
	 (node (make-instance 'rm::node
			      :compute-bounding-box t
			      :compute-center t
			      ;; :xy/z (rm::v3d 10.0 20.0 30.0)
			      :primitives (list	(make-instance 'rm::sphere-primitive
					   							       :radius 10.0
					   							       :tesselate 512
					   							       :xy/z (rm::v3d 10.0 0.0 0.0))
					   ;; (make-instance 'rm::cone-primitive
;; 							  :radius 1.0
;; 							  :xy/z (rm::v3d* (list (list 1.0 0.0 0.0)
;; 										(list 0.0 1.0 0.0)))
;; 							  :tesselate 32)
					   )
	   ))
	 (scene (make-instance 'rm::scene
			       :window window
			       :compute-bounding-box t
			       :union-all t
			       :children (list node))))
    (rm::assign-default-lighting scene)
    (rm::assign-default-camera scene)
    
    (let* ((bounds (rm::bounding-box node))
	   (min (rm::nth-vertex bounds 0))
	   (max (rm::nth-vertex bounds 1))
	   (center (rm::center node)))
      (rm::fformat "node: bounds: \(~A, ~A, ~A\)\(~A, ~A, ~A\)~%" (rm::x min) (rm::y min) (rm::z min) (rm::x max) (rm::y max) (rm::z max))
      (rm::fformat "node: center: \(~A, ~A, ~A\)" (rm::x center) (rm::y center) (rm::z center)))

    (rm::install-timer window 10
		       #'(lambda (window)
			   (rm::dispatch-event window rm::on-paint)
			   t))
    
    (rm::show-window window)
    (rm::process-events-wait))

  (rm::clean-up))