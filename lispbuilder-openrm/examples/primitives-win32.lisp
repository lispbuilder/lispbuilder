;;;;; A simple example to verify the correctness of the Win32 and OpenRM packages.
;;;;; Author: Luke J Crook, luke@balooga.com
;;;;; 

(in-package #:rm-examples)

(defun wire-box-example-1 ()
  (make-instance 'rm::native-window
                 :title-caption "Wire Box Example 1" :icon-caption "Wire Box Example 1"
                 :width 320 :height 240)
  
  (let ((box (rm::new-box :type :solid
                          :rgb/a (rm:color 1.0 1.0 0.0 1.0)
                          :xy/z #(#(-1.0 -1.0 -1.0)
                                  #(1.0 1.0 1.0))))
        (scenes (list (make-instance 'rm::scene
                                     :window (rm::default-window)
                                     :viewport #(0.0 0.0 0.5 0.5))
                      (make-instance 'rm::scene
                                     :window (rm::default-window)
                                     :viewport #(0.5 0.0 1.0 0.5))
                      (make-instance 'rm::scene
                                     :window (rm::default-window)
                                     :viewport #(0.0 0.5 0.5 1.0))
                      (make-instance 'rm::scene
                                     :window (rm::default-window)
                                     :viewport #(0.5 0.5 1.0 1.0)))))

    (mapcar #'(lambda (scene) (rm:add-scene scene (rm::default-window))) scenes)
    (mapcar #'(lambda (scene) (rm::add-to-scene scene box)) scenes)
    (mapcar #'rm::compute-bounding-box scenes)
    (rm::union-all-boxes (rm::default-window))

    (mapcar #'(lambda (scene)
                (rm:with-camera (cam (make-instance 'rm::camera-3d))
                  (rm::compute-view-from-geometry cam scene)
                  (setf (rm::camera scene) cam))) scenes))
  
  (rm::process-events)
  (rm::clean-up))

(defun camera-wireframe ()
  (make-instance 'rm::native-window
                 :title-caption "Camera Wireframe" :icon-caption "Camera Wireframe"
                 :width 320 :height 240)
  
  (let ((chasis-primitive (make-instance 'rm::box-wire-primitive
                                      :rgb/a (rm:color 1.0 1.0 0.0 1.0)
                                      :xy/z #(#(-1.0 -1.0 -1.0)
                                              #(1.0 1.0 1.0))
                                      :compute-bounding-box t))
        (lense-primitive (make-instance 'cone-primitive
                                        :rgb/a rgb/a
                                        :xy/z (vector direction #(0.0 0.0 0.0))
                                        :normals normals
                                        :bounding-box p-bounding-box
                                        :compute-bounding-box p-compute-bounding-box
                                        :tesselate tesselate :radius radius
                                        :display-list display-list
                                        :app-display-list app-display-list))
        (scenes (list (make-instance 'rm::scene
                                     :window (rm::default-window)
                                     :viewport #(0.0 0.0 0.5 0.5))
                      (make-instance 'rm::scene
                                     :window (rm::default-window)
                                     :viewport #(0.5 0.0 1.0 0.5))
                      (make-instance 'rm::scene
                                     :window (rm::default-window)
                                     :viewport #(0.0 0.5 0.5 1.0))
                      (make-instance 'rm::scene
                                     :window (rm::default-window)
                                     :viewport #(0.5 0.5 1.0 1.0)))))

    (mapcar #'(lambda (scene) (rm:add-scene scene (rm::default-window))) scenes)
    (mapcar #'(lambda (scene) (rm::add-to-scene scene box)) scenes)
    (mapcar #'rm::compute-bounding-box scenes)
    (rm::union-all-boxes (rm::default-window))

    (mapcar #'(lambda (scene)
                (rm:with-camera (cam (make-instance 'rm::camera-3d))
                  (rm::compute-view-from-geometry cam scene)
                  (setf (rm::camera scene) cam))) scenes))
  
  (rm::process-events)
  (rm::clean-up))




(defvar *window* nil)
(defvar *scene* nil)
(defvar *node* nil)
(defvar *prim* nil)

(setf *window* (make-instance 'rm::native-window :name "window"
                              :width 320 :height 240))

(setf *prim* (make-instance 'rm::sphere-primitive
                            :radius 1.0
                            :tesselate 512
                            :rgb/a #(1.0 1.0 0.0 1.0);;(rm::c4d 1.0 1.0 0.0 1.0)
                            :xy/z #(1.0 1.0 0.0 1.0);;(rm::v3d 0.0 0.0 0.0)
                            ))

(setf *scene* (make-instance 'rm::scene :name "scene"
                             :window *window*
                             :default-camera t
                             :default-lighting t
                             :dims :renderpass-3d
                             :opacity :renderpass-opaque))

(setf *node* (make-instance 'rm::node
                            :compute-bounding-box t
                            :name "sphere"))

(rm:add-primitive *prim* *node*)
(rm::add-to-scene *scene* *node*)
(rm::compute-bounding-box *node*)
(rm:add-scene *scene* *window*)
(rm::assign-defaults (rm::camera *scene*))
(rm::assign-default-lighting *scene*)
(rm::process-events)
(rm::clean-up)

(defun sphere-example-1 ()
  (let* ((window (make-instance 'rm::native-window :name "window"
				:width 320 :height 240))
	 (scene (make-instance 'rm::scene :name "scene"
                               :window window
                               :default-camera t
                               :default-lighting t
                               :dims :renderpass-3d
                               :opacity :renderpass-opaque
                               :children (list
                                          (make-instance 'rm::node
                                                         :compute-bounding-box t
                                                         :name "sphere"
                                                         :primitives (list
                                                                      (make-instance 'rm::sphere-primitive
                                                                                     :radius 1.0
                                                                                     :tesselate 512
                                                                                     :rgb/a (rm::c4d 1.0 1.0 0.0 1.0)
                                                                                     :xy/z (rm::v3d 0.0 0.0 0.0))))))))
    
    (rm::show-window window)
    (rm::process-events-wait)
    (rm::clean-up)))

;;;
;;;

(defclass window (rm::native-window) ()
  (:default-initargs
   :name "window" :width 320 :height 240))

(defclass scene (rm::scene) ()
  (:default-initargs
   :name "scene"
    :dims :renderpass-3d
    :opacity :renderpass-opaque
    :default-camera t
    :default-lighting t))

(defclass sphere-prim (rm::sphere-primitive) ()
  (:default-initargs
   :tesselate 512
    :radius 1.0
    :rgb/a (rm::c4d 1.0 1.0 0.0 1.0)
    :xy/z (rm::v3d 0.0 0.0 0.0)))

(defun sphere-example-2 ()
  (let* ((window (make-instance 'window))
	 (sphere (make-instance 'rm::node :name "sphere"
				:compute-bounding-box t
				:children (list (make-instance 'sphere-prim)))))

    (make-instance 'scene :window window
		   :children (list sphere))
  
    (rm::show-window window)
    (rm::process-events-wait)
    (rm::clean-up)))

;;;
;;;

(defun sphere-example-3 ()
  (let* ((window (make-instance 'rm::native-window :name "window"
				:width 320 :height 240))
	 (scene (make-instance 'rm::scene :name "scene"
		   :dims :renderpass-3d
		   :opacity :renderpass-opaque))
	 (sphere (make-instance 'rm::node :name "sphere")))

    ;; Sphere Primitive
    (let ((sphere-prim (make-instance 'rm::sphere-primitive)))
      (setf (rm::radius sphere-prim) 1.0
	    (rm::tesselate sphere-prim) 512
	    (rm::rgb/a sphere-prim) (rm::c4d 1.0 1.0 0.0 1.0)
	    (rm::xy/z sphere-prim) (rm::v3d 0.0 0.0 0.0))
      (rm::add-to-node sphere sphere-prim))

    ;; Add to scene graph
    (rm::add-to-node scene sphere)
    (rm::compute-bounding-box sphere)

    (rm::add-to-window window scene)

    ;; Default camera and lighting
    (rm::assign-default-camera scene)
    (rm::assign-default-lighting scene)

    ;; Show window
    (rm::show-window window)

    ;; Process events
    (rm::process-events-wait)

    ;; Delete the Scene Graph. Attempt to close stray windows
    (rm::clean-up)))





;; (defmethod rm::on-mouse-down ((window rm::window) button x y wparam)
;;   (rm::fformat "on-mouse-down")
;;   ;; (when (rm::buttonp button :left)
;; ;;     (let ((picked-list (rm::get-picked window
;; ;; 				       (rm::rm-root-node)
;; ;; 				       x y
;; ;; 				       :multiple t)))
;; ;;       (dolist (var picked-list)
;; ;; 	(rm::fformat "   name: ~A" (rm::name var)))      
;; ;;       (rm::clear-picked)
;; ;;       (when picked-list
;; ;; 	(loop for i in picked-list
;; ;; 	   do (rm::add-pick i :multiple)))))

;;   ;; (setf *x* x
;; ;; 	*y* y)
;; ;;   (rm-cffi::rm-print-scene-graph (rm::fp (rm::rm-root-node))
;; ;; 				     :rm-true
;; ;; 				     "scene-graph.txt")
  
;;   (rm::dispatch-event window rm::on-paint))

;; (defmethod rm::on-mouse-move ((window rm::window) button x y wparam)
;;   (rm::dispatch-event window rm::on-paint))

(defclass window (rm::native-window rm::aux-window) ()
  (:default-initargs
   :name "window"
    :width 320
    :height 240
    :events '(rm::on-mouse-move rm::on-mouse-down)
    :node (rm::rm-root-node)))

(defclass scene (rm::scene rm::aux-scene) ()
  (:default-initargs
   :name "scene"
    :dims :rm-renderpass-3D
    :opacity :rm-renderpass-opaque
    :default-camera t
    :viewport t
    :default-lighting t
    :compute-bounding-box t
    :union-all t
    :compute-center t))

(defclass node (rm::node)()
  (:default-initargs
    :compute-bounding-box t
    :dims :rm-renderpass-3D
    :opacity :rm-renderpass-opaque))

(defclass sphere-node (node) ()
  (:default-initargs
    :compute-bounding-box t
    :dims :rm-renderpass-3D
    :opacity :rm-renderpass-opaque))

(defclass sphere (rm::sphere-primitive) ()
  (:default-initargs
    :radius 1.0
    :tesselate 512
    :rgb/a (rm::c4d 1.0 1.0 0.0 1.0)
    :xy/z (rm::v3d 0.0 0.0 0.0)))



;; (defun primitives-plane-sphere ()
;;   (let* ((window (make-instance 'rm::native-window :width 320 :height 240))
;; 	 (scene (make-instance 'rm::scene :name "default-scene"
;; 			       :parent window :dims :rm-renderpass-3d :opacity :rm-renderpass-opaque
;; 			       :background-color (rm::c4d 0.2 0.2 0.3 1.0))))

;;     (rm::with-default-node (scene)
;;       (rm::with-default-node ((make-instance 'rm::node :name "sphere-node"))
;; 	(rm::with-default-primitive ((make-instance 'rm::sphere-primitive))
;; 	  (rm::set-rgb/a (rm::c4d* (loop for x below 5
;; 				      append (loop for y below 5
;; 						collecting (list (random 1.0) (random 1.0) (random 1.0) 1.0)))))
;; 	  (rm::set-tesselate 128)
;; 	  (rm::set-radius (loop repeat (* 5 5) collecting (1+ (random 5.0))))
;; 	  (rm::set-xy/z (rm::v3d* (loop for x below 5
;; 				     append (loop for y below 5
;; 					       collecting (list (* x 10.0) (* y 10.0) (random 50.0))))))
;; 	  (rm::set-compute-bounding-box))
;; 	(rm::set-compute-bounding-box))
;;       (rm::set-compute-bounding-box))

;;     (rm::assign-default-lighting window)
;;     (rm::assign-default-camera scene)
            
;;     (rm::show-window window)
;;     (rm::process-events-wait)
;;     (rm::delete-scene-graph)))

;; (defun primitives-meta ()
;;   (rm:with-init ()
;;     (let* ((window (make-instance 'rm::native-window
;; 				  :width 320 :height 240
;; 				  :parent-node (rm::rm-root-node)))
;; 	   (scene-1 (make-instance 'rm::scene :name "default-scene-1" :window window))
;; 	   (scene-2 (make-instance 'rm::scene :name "default-scene-2" :window window))
;; 	   (scene-3 (make-instance 'rm::scene :name "default-scene-3" :window window))
;; 	   (scene-4 (make-instance 'rm::scene :name "default-scene-4" :window window))
;; 	   ;; (scene-1 (make-instance 'rm::scene
;; ;; 				   :name "default-scene-1"
;; ;; 				   :window window
;; ;; 				   :node (rm::node window)))
;; 	   (base (make-instance 'rm::node :name "base"))
;; 	   (sphere (make-instance 'rm::sphere-primitive))
;; 	   (wire (make-instance 'rm::box-wire-primitive)))
      
;;       (rm::add-to-node (rm::node scene-1)
;; 		       base)
;;       (rm::add-to-node (rm::node scene-2)
;;  		       base)
;;       (rm::add-to-node (rm::node scene-3)
;;  		       base)
;;       (rm::add-to-node (rm::node scene-4)
;;  		       base)
      

;;       (rm::add-to-node base sphere)
;;       (rm::add-to-node base wire)

;;       (setf (rm::rgb/a sphere) (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0)
;; 	    (rm::tesselate sphere) 128
;; 	    (rm::radius sphere) 10.0
;; 	    (rm::xy/z sphere) (rm::v3d 0.0 0.0 -20.0))
;;       (rm::compute-bounding-box sphere)

;;       (setf (rm::xy/z wire) (rm::bounding-box sphere))
;;       (rm::compute-bounding-box wire)

;;       (rm::compute-bounding-box base)
      

;;       ;; (rm::with-default-node (base)
;; ;; 	(rm::fformat "parent-node: ~A" rm::*parent-node*)
;; ;; 	(rm::with-default-primitive (sphere)
;; ;; 	  (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; ;; 	  (rm::set-tesselate 128)
;; ;; 	  (rm::set-radius 10.0)
;; ;; 	  (rm::set-xy/z (rm::v3d 0.0 0.0 -20.0))
;; ;; 	  (rm::set-compute-bounding-box))
;; ;; 	(rm::with-default-primitive ((make-instance 'rm::box-wire-primitive))
;; ;; 	  (rm::set-xy/z (rm::bounding-box sphere)))
;; ;; 	(rm::set-compute-bounding-box))

;;       (rm::assign-default-lighting window)
;; ;;       (rm::assign-default-lighting scene-1)
;; ;;       (rm::assign-default-lighting scene-2)
;; ;;       (rm::assign-default-lighting scene-3)
;; ;;       (rm::assign-default-lighting scene-4)
;;       (rm::assign-default-camera scene-1)
;;       (rm::assign-default-camera scene-2)
;;       (rm::assign-default-camera scene-3)
;;       (rm::assign-default-camera scene-4)
      
;;       (setf (rm::viewport scene-1) #(0.0 0.5 0.5 1.0)
;;  	    (rm::viewport scene-2) #(0.5 0.0 1.0 0.5)
;;  	    (rm::viewport scene-3) #(0.0 0.0 0.5 0.5)
;;  	    (rm::viewport scene-4) #(0.5 0.5 1.0 1.0))
      
;;       (rm::show-window window)

;;       ;; Start the Windows event loop.
;;       (rm::process-events)

;;       (rm-cffi::rm-print-scene-graph (rm::fp (rm::rm-root-node))
;; 				     :rm-true
;; 				     "scene-graph.txt")

;;       (rm::delete-scene-graph))))

;; (defun primitives-meta-2 ()
;;   (rm:with-init ()
;;     (let* ((window (make-instance 'rm::native-window
;; 				  :width 320 :height 240
;; 				  :parent-node (rm::rm-root-node)
;; 				  :node (make-instance 'rm::node :name "default-native-window"
;; 						       ;; :dims :rm-renderpass-3D
;; 						       ;; :opacity :RM-RENDERPASS-OPAQUE
;; 						       :dims :rm-renderpass-3D
;; 						       :opacity :RM-RENDERPASS-OPAQUE						       
;; 						       )))
;; 	   (scene (make-instance 'rm::scene
;; 				 :name "default-scene"
;; 				 :window window
;; 				 :node (rm::node window))))

;;       (rm::enable-event window 'rm::on-mouse-down)
;;       (rm::enable-event window 'rm::on-mouse-move)
      
;;       ;;       (setf node1 (make-instance 'rm::meta-node :parent (rm::node scene-1) :name "Node-1"))
;;       ;;       (setf node2 (make-instance 'rm::meta-node :parent node1 :name "Node-2"))
      
;;       (let ((prim (make-instance 'rm::sphere-primitive)))
;; 	(setf (rm::rgb/a prim) (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0)
;; 	      (rm::xy/z prim) (rm::v3d 0.0 0.0 0.0)
;; 	      (rm::tesselate prim) 128
;; 	      (rm::radius prim) 10.0)
;; 	(rm::compute-bounding-box prim)
;; 	(rm::add-to-node (rm::node scene) prim))

;;       (rm::compute-bounding-box (rm::node scene))
            
;;       (rm::install-timer window 2000
;; 			 #'(lambda (window)
;; 			     (rm::dispatch-event window rm::on-paint)
;; 			     t))

;;       (rm::assign-default-lighting window)
;;       (rm::assign-default-camera scene)

;;       (rm::show-window window)

;;       ;; Start the Windows event loop.
;;       (rm::process-events)
;;       (rm-cffi::rm-print-scene-graph (rm::fp (rm::rm-root-node))
;; 				     :rm-true
;; 				     "scene-graph.txt")
;;       (rm::delete-scene-graph))))


;; (defun primitives-text ()
;;   (rm:with-init ()
;;     (let* ((window (make-instance 'rm::native-window
;; 				  :width 320 :height 240
;; 				  :parent-node (rm::rm-root-node)))
;; 	   (scene-1 (make-instance 'rm::scene
;; 				   :name "default-scene-1"
;; 				   :window window
;; 				   :node (rm::node window))))

;;       (rm::enable-event window 'rm::on-mouse-down)
;;       (rm::enable-event window 'rm::on-mouse-move)

;;       (setf *meta-group* (make-instance 'rm::node :name "meta:random-sphere-node"))
      
;;       (rm::with-default-node ((rm::node scene-1))
;; 	(rm::with-default-node ((make-instance 'rm::meta-node :name "meta:group-node"))
;; 	  (rm::with-default-node (*meta-group*)
;; 	    (rm::with-default-primitive ((make-instance 'rm::sphere-primitive))
;; 	      (rm::set-rgb/a (rm::c4d* (loop for x below 5
;; 					  append (loop for y below 5
;; 						    collecting (list (random 1.0) (random 1.0) (random 1.0) 1.0)))))
;; 	      (rm::set-tesselate 128)
;; 	      (rm::set-radius (loop repeat (* 5 5) collecting (1+ (random 2.0))))
;; 	      (rm::set-xy/z (rm::v3d* (loop for x below 5
;; 					 append (loop for y below 5
;; 						   collecting (list (* x 10.0) (* y 10.0) (random 50.0))))))
;; 	      (rm::set-compute-bounding-box))
;; 	    (rm::set-compute-bounding-box))
;; 	  (rm::with-default-node ((make-instance 'rm::meta-node :name "meta:sphere-node"))
;; 	    (rm::with-default-primitive ((make-instance 'rm::sphere-primitive))
;; 	      (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; 	      (rm::set-tesselate 128)
;; 	      (rm::set-radius 10.0)
;; 	      (rm::set-xy/z (rm::v3d 0.0 0.0 -20.0))
;; 	      (rm::set-compute-bounding-box))
;; 	    ;; (rm::with-default-primitive ((make-instance 'rm::text-primitive))
;; 	    ;; 	      (rm::set-xy/z (rm::v3d 0.0 20.0 -20.0))
;; 	    ;; 	      (rm::set-text "**SPHERE**")
;; 	    ;; 	      (rm::set-compute-bounding-box))
;; 	    (rm::set-compute-bounding-box))
;; 	  	;; Create a plane (a quad-mesh along the xz plane)
;; 	  (rm::with-default-node ((make-instance 'rm::meta-node :name "plane-node"))
;; 	    (rm::with-default-primitive ((make-instance 'rm::plane-primitive :orientation :xz))
;; 	      (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; 	      (rm::set-xy/z (rm::v3d* '((-130.0 -30.0 -50.0) (40.0 -30.0 50.0)))))
;; 	    (rm::set-compute-bounding-box)))

;; 	(rm::set-compute-bounding-box))
           
;;       (rm::assign-default-lighting window)
;;       (rm::assign-default-camera scene-1)
      
;;       (rm::install-timer window 1000 #'(lambda (window)
;; 					 (rm::dispatch-event window rm::on-paint)
;; 					 t))
;;       (rm::show-window window)

;;       ;; Start the Windows event loop.
;;       (rm::process-events)
;;       (rm::delete-scene-graph))))

;; (defun primitives-4-cameras ()
;;   (rm:with-init ()
;;     (let* ((window (make-instance 'rm::native-window :width 320 :height 240
;; 				  :parent-node (rm::rm-root-node)))
;; 	   (base-node (make-instance 'rm::node :name "base-node"))
;; 	   (scene-1 (make-instance 'rm::scene :name "default-scene-1" :window window))
;; 	   (scene-2 (make-instance 'rm::scene :name "default-scene-2" :window window))
;; 	   ;(scene-3 (make-instance 'rm::scene :name "default-scene-3" :window window))
;; 	   ;(scene-4 (make-instance 'rm::scene :name "default-scene-4" :window window))
;; 	   )

;; ;;       (setf (rm::background-color scene-1) (rm::c4d 1.0 0.5 0.5 1.0)
;; ;; 	    (rm::background-color scene-2) (rm::c4d 0.5 1.0 0.5 1.0)
;; ;; 	    (rm::background-color scene-3) (rm::c4d 0.5 0.5 1.0 1.0)
;; ;; 	    (rm::background-color scene-4) (rm::c4d 1.0 1.0 0.5 1.0))

;;       (rm::enable-event window 'rm::on-mouse-down)
;;       (rm::enable-event window 'rm::on-mouse-move)
      
;;       (rm::with-default-node (base-node)
;; 	;; Create the random spheres
;; 	(rm::with-default-node ((make-instance 'rm::meta-node :name "random-sphere-node"))
;; 	  (rm::with-default-primitive ((make-instance 'rm::sphere-primitive))
;; 	    (rm::set-rgb/a (rm::c4d* (loop for x below 5
;; 					append (loop for y below 5
;; 						  collecting (list (random 1.0) (random 1.0) (random 1.0) 1.0)))))
;; 	    (rm::set-tesselate 128)
;; 	    (rm::set-radius (loop repeat (* 5 5) collecting (1+ (random 5.0))))
;; 	    (rm::set-xy/z (rm::v3d* (loop for x below 5
;; 				       append (loop for y below 5
;; 						 collecting (list (* x 10.0) (* y 10.0) (random 50.0)))))))
;;  	  (rm::set-compute-bounding-box)))

;;       (rm::add-to-node (rm::node scene-1)
;; 		       base-node)
;;       (rm::add-to-node (rm::node scene-2)
;;  		       base-node)
;; ;;       (rm::add-to-node (rm::node scene-3)
;; ;;  		       base-node)
;; ;;       (rm::add-to-node (rm::node scene-4)
;; ;;  		       base-node)

;;       (rm::assign-default-lighting window)

;;       (setf (rm::viewport scene-1) #(0.0 0.5 0.5 1.0)
;; 	    (rm::viewport scene-2) #(0.5 0.0 1.0 0.5)
;; 	    ;; (rm::viewport scene-3) #(0.0 0.0 0.5 0.5)
;; 	    ;; (rm::viewport scene-4) #(0.5 0.5 1.0 1.0)
;; 	    )
	    
;;       (rm::assign-default-camera scene-1)
;;       (rm::assign-default-camera scene-2)
;; ;;       (rm::assign-default-camera scene-3)
;; ;;       (rm::assign-default-camera scene-4)
      
;;       (rm::install-timer window 500 #'(lambda (window)
;; 					(rm::dispatch-event window rm::on-paint)
;; 					t))
 
;;       ;; Not the same as rendering an OpenGL frame.
;;       ;;(rm::update-window window)
;;       ;; Force the Window to appear, or it will look like the app has hung.
;;       (rm::show-window window)

;;       ;; Start the Windows event loop.
;;       (rm::process-events)
;;       (rm::delete-scene-graph))))

;; (defun primitives-win32 ()
;;   (rm:with-init ()
;;     (let* ((window (make-instance 'rm::native-window :width 320 :height 240
;; 				  :parent-node (rm::rm-root-node)))
;; 	   (base-node (make-instance 'rm::node :name "base-node"))
;; 	   (scene-1 (make-instance 'rm::scene :name "default-scene-1" :window window))
;; 	   (scene-2 (make-instance 'rm::scene :name "default-scene-2" :window window))
;; 	   (scene-3 (make-instance 'rm::scene :name "default-scene-3" :window window))
;; 	   (scene-4 (make-instance 'rm::scene :name "default-scene-4" :window window))
;; 	   (plane (make-instance 'rm::meta-node :name "plane-node"))
;; 	   (meta (make-instance 'rm::node :name "meta-node"))
;; 	   (target (make-instance 'rm::node :name "target-node"))
;; 	   (info (make-instance 'rm::node :name "info-node"))
;; 	   (box (make-instance 'rm::box-wire-primitive)))

;;       ;; (setf (rm::background-color window) (rm::c4d 0.5 1.0 0.0 1.0)
;;       ;; 	    (rm::background-color scene-1) (rm::c4d 1.0 0.5 0.5 1.0)
;;       ;; 	    (rm::background-color scene-2) (rm::c4d 0.5 1.0 0.5 1.0)
;;       ;; 	    (rm::background-color scene-3) (rm::c4d 0.5 0.5 1.0 1.0)
;;       ;; 	    (rm::background-color scene-4) (rm::c4d 1.0 1.0 0.5 1.0))

;;       (rm::enable-event window 'rm::on-mouse-down)
;;       (rm::enable-event window 'rm::on-mouse-move)

;;       (setf (rm::traverse plane) nil)
      
;;       (rm::with-default-node (base-node)
;; 	;; Create the random spheres
;; 	(rm::with-default-node ((make-instance 'rm::meta-node :name "random-sphere-node"))
;; 	  (rm::with-default-primitive ((make-instance 'rm::sphere-primitive))
;; 	    (rm::set-rgb/a (rm::c4d* (loop for x below 5
;; 					append (loop for y below 5
;; 						  collecting (list (random 1.0) (random 1.0) (random 1.0) 1.0)))))
;; 	    (rm::set-tesselate 128)
;; 	    (rm::set-radius (loop repeat (* 5 5) collecting (1+ (random 5.0))))
;; 	    (rm::set-xy/z (rm::v3d* (loop for x below 5
;; 				       append (loop for y below 5
;; 						 collecting (list (* x 10.0) (* y 10.0) (random 50.0)))))))
;;  	  (rm::set-compute-bounding-box))

;; 	;; Create a cone
;; 	;; (rm::with-default-node ((make-instance 'rm::meta-node :name "cone-node"))
;; ;; 	  (rm::with-default-primitive ((make-instance 'rm::cone-primitive))
;; ;; 	    (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; ;; 	    (rm::set-radius 10.0)
;; ;; 	    (rm::set-xy/z (rm::v3d* '((-30.0 0.0 0.0) (-30.0 50.0 0.0)))))
;; ;; 	  (rm::set-compute-bounding-box))

;; 	;; Create a cylinder
;; 	(rm::with-default-node ((make-instance 'rm::meta-node :name "cylinder-node"))
;; 	  (rm::with-default-primitive ((make-instance 'rm::cylinder-primitive))
;; 	    (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; 	    (rm::set-radius 10.0)
;; 	    (rm::set-xy/z (rm::v3d* '((-60.0 0.0 0.0) (-60.0 50.0 0.0)))))
;; 	  (rm::set-compute-bounding-box))

;; 	;; Create a solid box
;; 	(rm::with-default-node ((make-instance 'rm::meta-node :name "box-solid-node"))
;; 	  (rm::with-default-primitive ((make-instance 'rm::box-solid-primitive))
;; 	    (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; 	    (rm::set-xy/z (rm::v3d* '((-130.0 0.0 0.0) (-80.0 50.0 50.0)))))
;; 	  (rm::with-default-primitive ((make-instance 'rm::box-wire-primitive))
;; 	    (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; 	    (rm::set-xy/z (rm::v3d* '((-130.0 0.0 0.0) (-80.0 50.0 50.0)))))
;; 	  (rm::set-compute-bounding-box))

;; 	;; Create a plane (a quad-mesh along the xz plane)
;; 	(rm::with-default-node (plane)
;; 	  (rm::with-default-primitive ((make-instance 'rm::plane-primitive :orientation :xz))
;; 	    (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; 	    (rm::set-xy/z (rm::v3d* '((-130.0 -30.0 -50.0) (40.0 -30.0 50.0)))))
;; 	  (rm::set-compute-bounding-box))

;; 	;; (rm::with-default-node (meta)
;; ;; 	  (rm::with-default-primitive ((make-instance 'rm::box-wire-primitive))
;; ;; 	    (rm::set-xy/z (rm::v3d* '((-30.0 0.0 0.0) (30.0 50.0 50.0))))
;; ;; 	      ;; (rm::set-xy/z (rm::bounding-box target))
;; ;; 	    )
;; ;; 	  (rm::set-compute-bounding-box))
;; 	)

;;       (rm::add-to-node (rm::node scene-1)
;; 		       base-node)
;;       (rm::add-to-node (rm::node scene-2)
;;  		       base-node)
;;       (rm::add-to-node (rm::node scene-3)
;;  		       base-node)
;;       (rm::add-to-node (rm::node scene-4)
;;  		       base-node)

;;       ;;      (rm::assign-default-lighting base-node)


;;       ;; (rm::with-default-node (base-node)
;; ;; 	(rm::with-default-node (meta)
;; ;; 	  (rm::with-default-node (target)
;; ;; 	    (rm::with-default-primitive ((make-instance 'rm::cone-primitive))
;; ;; 	      (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; ;; 	      (rm::set-radius 10.0)
;; ;; 	      (rm::set-xy/z (rm::v3d* '((-30.0 0.0 0.0) (-30.0 50.0 0.0)))))
;; ;; 	    (rm::set-compute-bounding-box))
;; ;; 	  ;; (rm::with-default-node (info)
;; ;; ;; 	    (rm::with-default-primitive ((make-instance 'rm::box-wire-primitive))
;; ;; ;; 	      (rm::set-xy/z (rm::v3d* '((-30.0 0.0 0.0) (30.0 50.0 50.0))))
;; ;; ;; 	      ;; (rm::set-xy/z (rm::bounding-box target))
;; ;; ;; 	      )
;; ;; ;; 	    (rm::set-compute-bounding-box))
;; ;; 	  ))

;;       (rm::assign-default-lighting window)

      
;;       (rm::assign-default-camera scene-1)
;;       (rm::assign-default-camera scene-2)
;;       (rm::assign-default-camera scene-3)
;;       (rm::assign-default-camera scene-4)
      
;;       (setf (rm::viewport scene-1) #(0.0 0.5 0.5 1.0)
;;  	    (rm::viewport scene-2) #(0.5 0.0 1.0 0.5)
;;  	    (rm::viewport scene-3) #(0.0 0.0 0.5 0.5)
;;  	    (rm::viewport scene-4) #(0.5 0.5 1.0 1.0))

      

;; ;;       ;(setf (rm::traverse info) t)
;; ;;       (rm::with-default-node (meta)
;; ;; 	(rm::with-default-node (info)
;; ;; 	  (rm::with-default-primitive (box)
;; ;; 	    ;;(rm::set-xy/z (rm::bounding-box target))
;; ;; 	    (rm::set-xy/z (rm::v3d* '((-30.0 0.0 0.0) (30.0 50.0 50.0)))))))

;;       ;;(rm::add-to-node meta info)
;;       ;;(setf (rm::display-info-p plane) t)
      
;;       (rm::install-timer window 4000 #'(lambda (window)
;; 					 ;; 					 (if (rm::display-info-p plane)
;; 					 ;; 					     (setf (rm::display-info-p plane) nil)
;; 					 ;; 					     (setf (rm::display-info-p plane) t))
;; 					 ;; (if (rm::traverse info) 
;; ;; 					     (progn
;; ;; 					       (setf (rm::traverse info) nil))
;; ;; 					     (progn
;; ;; 					       (setf (rm::traverse info) t)
;; ;; 					       (setf (rm::xy/z box) (rm::bounding-box target)))
;; ;; 					     )
;; 					 ;; 					 (if (rm::traverse (rm::info-node plane)) 
;; 					 ;; 					   (setf (rm::traverse (rm::info-node plane)) nil)
;; 					 ;; 					   (setf (rm::traverse (rm::info-node plane)) t))
;; 					 (rm::dispatch-event window rm::on-paint)
;; 					 t))
 
;;       ;; Not the same as rendering an OpenGL frame.
;;       ;;(rm::update-window window)
;;       ;; Force the Window to appear, or it will look like the app has hung.
;;       (rm::show-window window)

;;       ;; Start the Windows event loop.
;;       (rm::process-events)
;;       (rm::delete-scene-graph))))



;; (defun primitives-win32-no-meta ()
;;   (rm:with-init ()
;;     (let* ((window (make-instance 'rm::native-window :width 320 :height 240
;; 				  :parent-node (rm::rm-root-node)))
;; 	   (scene-1 (make-instance 'rm::scene :name "default-scene-1" :window window))
;; 	   (scene-2 (make-instance 'rm::scene :name "default-scene-2" :window window))
;; 	   (scene-3 (make-instance 'rm::scene :name "default-scene-3" :window window))
;; 	   (scene-4 (make-instance 'rm::scene :name "default-scene-4" :window window))
;; 	   (base-node (make-instance 'rm::node :name "base-node"))
;; 	   (plane (make-instance 'rm::node :name "plane-node"))
;; 	   (traverse? t))

;;       (setf ;; (rm::background-color window) (rm::c4d 0.5 1.0 0.0 1.0)
;; 	    (rm::background-color scene-1) (rm::c4d 1.0 0.5 0.5 1.0)
;; 	    (rm::background-color scene-2) (rm::c4d 0.5 1.0 0.5 1.0)
;; 	    (rm::background-color scene-3) (rm::c4d 0.5 0.5 1.0 1.0)
;; 	    (rm::background-color scene-4) (rm::c4d 1.0 1.0 0.5 1.0))

;;       (rm::enable-event window 'rm::on-mouse-down)
;;       (rm::enable-event window 'rm::on-mouse-move)
      
;;       (rm::with-default-node (base-node)
;; 	;; Create the random spheres
;; 	(rm::with-default-node ((make-instance 'rm::node :name "random-sphere-node"))
;; 	  (rm::with-default-primitive ((make-instance 'rm::sphere-primitive))
;; 	    (rm::set-rgb/a (rm::c4d* (loop for x below 5
;; 					append (loop for y below 5
;; 						  collecting (list (random 1.0) (random 1.0) (random 1.0) 1.0)))))
;; 	    (rm::set-tesselate 128)
;; 	    (rm::set-radius (loop repeat (* 5 5) collecting (1+ (random 5.0))))
;; 	    (rm::set-xy/z (rm::v3d* (loop for x below 5
;; 				       append (loop for y below 5
;; 						 collecting (list (* x 10.0) (* y 10.0) (random 50.0)))))))
;;  	  (rm::set-compute-bounding-box))

;; 	;; Create a cone
;; 	(rm::with-default-node ((make-instance 'rm::node :name "cone-node"))
;; 	  (rm::with-default-primitive ((make-instance 'rm::cone-primitive))
;; 	    (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; 	    (rm::set-radius 10.0)
;; 	    (rm::set-xy/z (rm::v3d* '((-30.0 0.0 0.0) (-30.0 50.0 0.0)))))
;; 	  (rm::set-compute-bounding-box))

;; 	;; Create a cylinder
;; 	(rm::with-default-node ((make-instance 'rm::node :name "cylinder-node"))
;; 	  (rm::with-default-primitive ((make-instance 'rm::cylinder-primitive))
;; 	    (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; 	    (rm::set-radius 10.0)
;; 	    (rm::set-xy/z (rm::v3d* '((-60.0 0.0 0.0) (-60.0 50.0 0.0)))))
;; 	  (rm::set-compute-bounding-box))

;; 	;; Create a solid box
;; 	(rm::with-default-node ((make-instance 'rm::node :name "box-solid-node"))
;; 	  (rm::with-default-primitive ((make-instance 'rm::box-solid-primitive))
;; 	    (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; 	    (rm::set-xy/z (rm::v3d* '((-130.0 0.0 0.0) (-80.0 50.0 50.0)))))
;; 	  (rm::set-compute-bounding-box))

;; 	;; Create a plane (a quad-mesh along the xz plane)
;; 	(rm::with-default-node (plane)
;; 	  (rm::with-default-primitive ((make-instance 'rm::plane-primitive :orientation :xz))
;; 	    (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; 	    (rm::set-xy/z (rm::v3d* '((-130.0 -30.0 -50.0) (40.0 -30.0 50.0)))))
;; 	  (rm::set-compute-bounding-box))
;; 	)

;;       (rm::add-to-node (rm::node scene-1)
;; 		       base-node)
;;       (rm::add-to-node (rm::node scene-2)
;;  		       base-node)
;;       (rm::add-to-node (rm::node scene-3)
;;  		       base-node)
;;       (rm::add-to-node (rm::node scene-4)
;;  		       base-node)

;;       (rm::assign-default-lighting window)

;;       (rm::assign-default-camera scene-1)
;;       (rm::assign-default-camera scene-2)
;;       (rm::assign-default-camera scene-3)
;;       (rm::assign-default-camera scene-4)
      
;;       (setf (rm::viewport scene-1) #(0.0 0.5 0.5 1.0)
;; 	    (rm::viewport scene-2) #(0.5 0.0 1.0 0.5)
;; 	    (rm::viewport scene-3) #(0.0 0.0 0.5 0.5)
;; 	    (rm::viewport scene-4) #(0.5 0.5 1.0 1.0))
     
;;       (rm::install-timer window 5000 #'(lambda (window)
;; 					 (if (rm::traverse plane) 
;; 					   (setf (rm::traverse plane) nil)
;; 					   (setf (rm::traverse plane) t))

;; 					(rm::dispatch-event window rm::on-paint)
;; 					t))
 
;;       ;; Not the same as rendering an OpenGL frame.
;;       ;;(rm::update-window window)
;;       ;; Force the Window to appear, or it will look like the app has hung.
;;       (rm::show-window window)

;;       (rm-cffi::rm-print-scene-graph (rm::fp (rm::rm-root-node))
;; 				     :rm-true
;; 				     "scene-graph.txt")

;;       ;; Start the Windows event loop.
;;       (rm::process-events)
;;       (rm::delete-scene-graph))))

;; (defun primitives-win32-2 ()
;;   (rm:with-init ()
;;     (let* ((window (make-instance 'rm::native-window :width 320 :height 240))
;; 	   (scene-1 (make-instance 'rm::scene :id "default-scene-1" :window window)))

;;       (setf (rm::background-color window) (rm::c4d 0.5 1.0 0.0 1.0)
;; 	    (rm::background-color scene-1) (rm::c4d 1.0 0.5 0.5 1.0))
      
;;       (rm::with-default-node ((rm::node scene-1))
;; 	;; Create a cone
;; 	(rm::with-default-node ((make-instance 'rm::meta-node :name "cone-node"))
;; 	  (rm::with-default-primitive ((make-instance 'rm::cone-primitive))
;; 	    (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; 	    (rm::set-radius 10.0)
;; 	    (rm::set-xy/z (rm::v3d* '((-30.0 0.0 0.0) (-30.0 50.0 0.0))))
;; 	    (rm::set-compute-bounding-box)
;; 	    (rm::set-compute-center-from-bounding-box))
;; 	  (rm::set-compute-bounding-box))
;; 	(rm::set-compute-bounding-box)
;; 		;; Create a plane (a quad-mesh along the xz plane)
;; 	(rm::with-default-node ((plane (make-instance 'rm::node :name "plane-node")))
;; 	  (rm::with-default-primitive ((make-instance 'rm::plane-primitive :orientation :xz))
;; 	    (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; 	    (rm::set-xy/z (rm::v3d* '((-130.0 -30.0 -50.0) (40.0 -30.0 50.0)))))
;; 	  (rm::set-compute-bounding-box))
;; )
    
;;       (rm::set-defaults scene-1)
     
;;       (rm::install-timer window 500 #'(lambda (window)
;; 					(rm::render window)
;; 					t))
 
;;       ;; Not the same as rendering an OpenGL frame.
;;       ;;(rm::update-window window)
;;       ;; Force the Window to appear, or it will look like the app has hung.
;;       (rm::show-window window)

;;       (rm::dump-scene-graph (rm::rm-root-node))
;;       ;; Start the Windows event loop.
;;       (rm::process-events)
;;       (rm::delete-scene-graph))))

;; (defun primitives-win32-3 ()
;;   (rm:with-init ()
;;     (let* ((window (make-instance 'rm::native-window :width 320 :height 240))
;; 	   (scene-1 (make-instance 'rm::scene :id "default-scene-1" :window window))
;; 	   (scene-2 (make-instance 'rm::scene :id "default-scene-2" :window window))
;; 	   (base-node (make-instance 'rm::node)))

;;       (setf (rm::background-color window) (rm::c4d 0.5 1.0 0.0 1.0)
;; 	    (rm::background-color scene-1) (rm::c4d 1.0 0.5 0.5 1.0)
;; 	    (rm::background-color scene-2) (rm::c4d 0.5 1.0 0.5 1.0))

;;       (rm::add-scene window scene-1)
;;       (rm::add-scene window scene-2)
      
;;       (rm::with-default-node (base-node)
;; 	;; Create the random spheres
;; 	(rm::with-default-node ((make-instance 'rm::meta-node :name "random-sphere-node"))
;; 	  (rm::with-default-primitive ((make-instance 'rm::sphere-primitive))
;; 	    (rm::set-rgb/a (rm::c4d* (loop for x below 5
;; 					append (loop for y below 5
;; 						  collecting (list (random 1.0) (random 1.0) (random 1.0) 1.0)))))
;; 	    (rm::set-tesselate 128)
;; 	    (rm::set-radius (loop repeat (* 5 5) collecting (1+ (random 5.0))))
;; 	    (rm::set-xy/z (rm::v3d* (loop for x below 5
;; 				       append (loop for y below 5
;; 						 collecting (list (* x 10.0) (* y 10.0) (random 50.0)))))))
;;  	  (rm::set-compute-bounding-box))

;; 	;; Create a cone
;; 	(rm::with-default-node ((make-instance 'rm::meta-node :name "cone-node"))
;; 	  (rm::with-default-primitive ((make-instance 'rm::cone-primitive))
;; 	    (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; 	    (rm::set-radius 10.0)
;; 	    (rm::set-xy/z (rm::v3d* '((-30.0 0.0 0.0) (-30.0 50.0 0.0)))))
;; 	  (rm::set-compute-bounding-box))

;; 	;; Create a cylinder
;; 	(rm::with-default-node ((make-instance 'rm::meta-node :name "cylinder-node"))
;; 	  (rm::with-default-primitive ((make-instance 'rm::cylinder-primitive))
;; 	    (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; 	    (rm::set-radius 10.0)
;; 	    (rm::set-xy/z (rm::v3d* '((-60.0 0.0 0.0) (-60.0 50.0 0.0)))))
;; 	  (rm::set-compute-bounding-box))

;; 	;; Create a solid box
;; 	(rm::with-default-node ((make-instance 'rm::meta-node :name "box-solid-node"))
;; 	  (rm::with-default-primitive ((make-instance 'rm::box-solid-primitive))
;; 	    (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; 	    (rm::set-xy/z (rm::v3d* '((-130.0 0.0 0.0) (-80.0 50.0 50.0)))))
;; 	  (rm::set-compute-bounding-box))

;; 	;; Create a plane (a quad-mesh along the xz plane)
;; 	(rm::with-default-node ((make-instance 'rm::meta-node :name "plane-node"))
;; 	  (rm::with-default-primitive ((make-instance 'rm::plane-primitive :orientation :xz))
;; 	    (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; 	    (rm::set-xy/z (rm::v3d* '((-130.0 -30.0 -50.0) (40.0 -30.0 50.0)))))
;; 	  (rm::set-compute-bounding-box)))

;;       (rm::add-to-node (rm::node scene-1)
;; 		       base-node)
;;       (rm::add-to-node (rm::node scene-2)
;;  		       base-node)
      
;;       (rm::set-defaults scene-1)
;;       (rm::set-defaults scene-2)

;;       (setf (rm::viewport scene-1) #(0.0 0.5 0.5 1.0)
;; 	    (rm::viewport scene-2) #(0.5 0.0 1.0 0.5))
     
;;       (rm::install-timer window 500 #'(lambda (window)
;; 					(rm::render window)
;; 					t))
 
;;       ;; Not the same as rendering an OpenGL frame.
;;       ;;(rm::update-window window)
;;       ;; Force the Window to appear, or it will look like the app has hung.
;;       (rm::show-window window)

;;       ;; Start the Windows event loop.
;;       (rm::process-events)
;;       (rm::delete-scene-graph))))


;; (defun primitives-win32-4 ()
;;   (rm:with-init ()
;;     (let* ((window (make-instance 'rm::native-window :width 320 :height 240))
;; 	   (scene-1 (make-instance 'rm::scene :name "default-scene-1" :window window))
;; 	   (scene-2 (make-instance 'rm::scene :name "default-scene-2" :window window))
;; 	   (scene-3 (make-instance 'rm::scene :name "default-scene-3" :window window))
;; 	   (base-node (make-instance 'rm::node :name "base-node")))

;;       (setf (rm::background-color window) (rm::c4d 0.5 1.0 0.0 1.0)
;; 	    (rm::background-color scene-1) (rm::c4d 1.0 0.5 0.5 1.0)
;; 	    (rm::background-color scene-2) (rm::c4d 0.5 1.0 0.5 1.0)
;; 	    (rm::background-color scene-3) (rm::c4d 0.5 0.5 1.0 1.0))
     
;;       (rm::with-default-node (base-node)
;; 	;; Create the random spheres
;; 	(rm::with-default-node ((make-instance 'rm::meta-node :name "random-sphere-node"))
;; 	  (rm::with-default-primitive ((make-instance 'rm::sphere-primitive))
;; 	    (rm::set-rgb/a (rm::c4d* (loop for x below 5
;; 					append (loop for y below 5
;; 						  collecting (list (random 1.0) (random 1.0) (random 1.0) 1.0)))))
;; 	    (rm::set-tesselate 128)
;; 	    (rm::set-radius (loop repeat (* 5 5) collecting (1+ (random 5.0))))
;; 	    (rm::set-xy/z (rm::v3d* (loop for x below 5
;; 				       append (loop for y below 5
;; 						 collecting (list (* x 10.0) (* y 10.0) (random 50.0)))))))
;;  	  (rm::set-compute-bounding-box))

;; 	;; Create a cone
;; 	(rm::with-default-node ((make-instance 'rm::meta-node :name "cone-node"))
;; 	  (rm::with-default-primitive ((make-instance 'rm::cone-primitive))
;; 	    (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; 	    (rm::set-radius 10.0)
;; 	    (rm::set-xy/z (rm::v3d* '((-30.0 0.0 0.0) (-30.0 50.0 0.0)))))
;; 	  (rm::set-compute-bounding-box))

;; 	;; Create a cylinder
;; 	(rm::with-default-node ((make-instance 'rm::meta-node :name "cylinder-node"))
;; 	  (rm::with-default-primitive ((make-instance 'rm::cylinder-primitive))
;; 	    (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; 	    (rm::set-radius 10.0)
;; 	    (rm::set-xy/z (rm::v3d* '((-60.0 0.0 0.0) (-60.0 50.0 0.0)))))
;; 	  (rm::set-compute-bounding-box))

;; 	;; Create a solid box
;; 	(rm::with-default-node ((make-instance 'rm::meta-node :name "box-solid-node"))
;; 	  (rm::with-default-primitive ((make-instance 'rm::box-solid-primitive))
;; 	    (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; 	    (rm::set-xy/z (rm::v3d* '((-130.0 0.0 0.0) (-80.0 50.0 50.0)))))
;; 	  (rm::set-compute-bounding-box))

;; 	;; Create a plane (a quad-mesh along the xz plane)
;; 	(rm::with-default-node ((make-instance 'rm::meta-node :name "plane-node"))
;; 	  (rm::with-default-primitive ((make-instance 'rm::plane-primitive :orientation :xz))
;; 	    (rm::set-rgb/a (rm::c4d (random 1.0) (random 1.0) (random 1.0) 1.0))
;; 	    (rm::set-xy/z (rm::v3d* '((-130.0 -30.0 -50.0) (40.0 -30.0 50.0)))))
;; 	  (rm::set-compute-bounding-box)))

;;       (rm::add-to-node (rm::node scene-1)
;; 		       base-node)
;;       (rm::add-to-node (rm::node scene-2)
;;  		       base-node)
;;       (rm::add-to-node (rm::node scene-3)
;;  		       base-node)
            
;;       (rm::set-defaults scene-1)
;;       (rm::set-defaults scene-2)
;;       (rm::set-defaults scene-3)
      
;;       (setf (rm::viewport scene-1) #(0.0 0.5 0.5 1.0)
;; 	    (rm::viewport scene-2) #(0.5 0.0 1.0 0.5)
;; 	    (rm::viewport scene-3) #(0.0 0.0 0.5 0.5))
     
;;       (rm::install-timer window 500 #'(lambda (window)
;; 					(rm::render window)
;; 					t))
 
;;       ;; Not the same as rendering an OpenGL frame.
;;       ;;(rm::update-window window)
;;       ;; Force the Window to appear, or it will look like the app has hung.
;;       (rm::show-window window)

;;       ;; Start the Windows event loop.
;;       (rm::process-events)
;;       (rm::delete-scene-graph))))