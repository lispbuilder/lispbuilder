;;;;; A version of the JBALLS example in rmdemos.
;;;;; Author: Luke J Crook, luke@balooga.com
;;;;; 02/25/2008
;;;;; 

(in-package #:rm-examples)

(defvar *moving-group* nil)
(defvar *stationary-group* nil)
(defvar *rotation-direction* nil)

(defclass jballs-window (rm::native-window)()
  (:default-initargs
   :width 320
    :height 240
    :name "window"))



(defclass jballs-scene (rm::scene rm::aux-trackball)()
  (:default-initargs
   :name "default-scene"
    :dims :rm-renderpass-3D
    :opacity :rm-renderpass-opaque
    :compute-view-from-geometry t
    :viewport t
    :compute-bounding-box t
    :compute-center t
    :union-all t
    :camera (make-instance 'rm::camera-3d :defaults t)))

(defclass node (rm::node)()
  (:default-initargs
    :compute-bounding-box t
    :dims :rm-renderpass-3D
    :opacity :rm-renderpass-opaque))

(defclass sloth (node)()
  (:default-initargs
   :name "sloth"
    :primitives (list (make-instance 'rm::sphere-primitive
				     :xy/z (rm::v3d 0.0 0.0 0.0)
				     :radius 1.0
				     :tesselate 512))))

(defclass floor-quad (node)()
  (:default-initargs
   :name "floor"
    :primitives (list (make-instance 'rm::plane-primitive
				     :orientation :xz
				     :xy/z (rm::v3d* '((-3.0 -2.0 -3.0)
						       (3.0 -2.0 3.0)))))))

(defclass zippy (node)()
  (:default-initargs
   :name "zippy"
    :specular-exponent 10.0
    :specular-color (rm::c4d 0.9 0.25 0.25 1.0)
    :primitives (list (make-instance 'rm::sphere-primitive
				     :rgb/a (rm::c4d 0.9 0.25 0.25 1.0)
				     :xy/z (rm::v3d 1.5 -1.0 0.0)
				     :radius 0.25
				     :tesselate 128))))

(defmethod rm::on-idle ((window jballs-window))
  (rm::rotate *moving-group*
	      :direction *rotation-direction*
	      :reverse (list *stationary-group*))
  (rm::dispatch-event window rm::on-paint))

(defun jballs ()
  (let* ((window (make-instance 'jballs-window))
	 (stationary-group (make-instance 'node :name "stationary-group"
					  :children (list (make-instance 'sloth)
							  (make-instance 'floor-quad))))
	 (moving-group (make-instance 'node :name "moving-group"
				      :center (rm::v3d 0.0 0.0 0.0)
				      :lights (list (make-instance 'rm::point-light
								   :light-source :rm-light-3
								   :specular-color (rm::c4d 1.0 0.0 0.0 1.0)
								   :diffuse-color (rm::c4d 1.0 0.0 0.0 1.0)
								   :xy/z (rm::v3d 1.5 -1.0 0.0)))
				      :children (list stationary-group
						      (make-instance 'zippy))))
	 (rotation-direction (rm::v3d 0.0 -2.0 0.0)))
    (setf *moving-group* moving-group
          *stationary-group* stationary-group
	  *rotation-direction* rotation-direction)


    (make-instance 'jballs-scene
		   :window window
		   :children (list moving-group)
		   :lights (list (make-instance 'rm::directional-light
						:light-source :rm-light-0
						:specular-color (rm::c4d 0.7 0.7 0.7 1.0))))
    
   (rm::install-timer window 10
		       #'(lambda (window)
			   (rm::rotate moving-group
				       :direction rotation-direction
				       :reverse (list stationary-group))
			   (rm::dispatch-event window rm::on-paint)
			   t))
    
    (rm::show-window window)
    ;; (rm::process-events :poll)
    (rm::process-events :wait)
    ;; Attempt to clean up any stray windows
    (rm::clean-up)))



