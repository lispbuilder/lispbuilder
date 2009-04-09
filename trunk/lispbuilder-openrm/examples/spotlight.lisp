;;;;; A version of the SPOTLIGHT demo in rmdemos.
;;;;; Author: Luke J Crook, luke@balooga.com
;;;;; 02/25/2008
;;;;;

(in-package #:rm-examples)

(defvar *width* 320)
(defvar *height* 240)
(defvar *background-color* (rm::c4d 0.2 0.2 0.3 1.0))

(defvar *spot-color* (rm::c4d 0.9 0.5 0.5 1.0))
(defvar *spot-exponent* 4.0)
(defvar *spot-xy/z* (rm::v3d 5.0 5.0 5.0))
(defvar *spot-direction* (rm::v3d 0.0 -1.0 0.0))

(defvar *arc* nil)
(defvar *dummy* nil)
(defvar *spotlight-root* nil)
(defvar *spotlight-icon* nil)
(defvar *spotlight* nil)

(defclass spotlight-window (rm::native-window rm::aux-window)()
  (:default-initargs
   :width 320
    :height 240
    :name "window"
    :events '(rm::on-mouse-move rm::on-mouse-down)))

(defclass spotlight-scene (rm::scene rm::aux-scene) ()
  (:default-initargs
   :name "scene"
    :dims :rm-renderpass-3d
    :opacity :rm-renderpass-opaque
    :background-color *background-color*
    :camera (make-instance 'camera)
    :lights (list (make-instance 'rm::arena-light :xy/z (rm::v3d 5.0 7.5 5.0)))))
  
(defclass camera (rm::camera-3d) ()
  (:default-initargs
   :eye (rm::v3d 5.0 5.0 22.0)
    :at (rm::v3d 5.0 5.0 0.0)
    :up-vector (rm::v3d 0.0 1.0 0.0)
    :hither 5.0
    :yon 40.0
    :fov 45.0
    :aspect-ratio (vector *width* *height*)
    :projection :rm-projection-perspective))

(defclass spotlight (rm::spotlight) ()
  (:default-initargs
   :light-source :rm-light-1
    :diffuse-color *spot-color*
    :specular-color *spot-color*
    :cutoff rm::*default-spot-cutoff*
    :exponent *spot-exponent*
    :xy/z *spot-xy/z*
    :direction *spot-direction*))

(defmethod rm::on-mouse-down ((window spotlight-window) button x y)
  (when (rm::button= button :button-left)
    (rm::reset-arc *arc* *dummy* *width* *height* x y))
  (rm::dispatch-event window rm::on-paint))

(defmethod rm::on-mouse-move ((window spotlight-window) button x y x-rel y-rel)
  (when (rm::button= button :button-left)
    (rm::update-arc *arc* *dummy* *width* *height* x y)
    ;; Rotate spotlight
    (setf (rm::light *spotlight-root*) (make-instance 'spotlight
						      :direction (rm::point-direction *dummy* *spot-direction*)))
    ;; Rotate spotlight-icon
    (rm::rotate *spotlight-icon* :match *dummy*))
  (rm::dispatch-event window rm::on-paint))

(defun spotlight ()
  (rm::with-init ()
    (setf *spot-direction* (rm::v3d 0.0 -1.0 0.0))
    (let* ((window (make-instance 'spotlight-window))
	   (*arc* (make-instance 'rm::arc))
	   (*dummy* (make-instance 'rm::node))
	   (spotlight (make-instance 'spotlight))
	   (quads '((((0.0 0.0 0.0)  (10.0 0.0 10.0))  :xz  1)
		    (((0.0 10.0 0.0) (10.0 10.0 10.0)) :xz -1)
		    (((0.0 0.0 0.0)  (0.0 10.0 10.0))  :yz  1)
		    (((10.0 0.0 0.0) (10.0 10.0 10.0)) :yz -1)
		    (((0.0 0.0 0.0)  (10.0 10.0 0.0))  :xy  1)))
	   (spotlight-icon (make-instance 'rm::node :name "spotlight-icon"
					  :opacity :rm-renderpass-opaque
					  :xy/z (rm::xy/z spotlight)
					  :diffuse-color (rm::diffuse-color spotlight)
					  :specular-color (rm::specular-color spotlight)
					  :ambient-color (rm::ambient-color spotlight)
					  :compute-bounding-box t
					  :primitives (list (make-instance 'rm::cone-primitive
									   :rgb/a *spot-color*
									   :radius (rm::cast float (rm::to-radian rm::*default-spot-cutoff*))
									   :xy/z (rm::v3d* (list *spot-direction*
												 (rm::v3d 0.0 0.0 0.0)))
									   :tesselate 32))))
	   (spotlight-root (make-instance 'rm::node :name "spotlight"
					  :children (list spotlight-icon
							  (make-instance 'rm::node :name "quad"
									 :primitives (loop for (xy/z orientation sign) in quads
											collecting (make-instance 'rm::plane-primitive
														  :xy/z (rm::v3d* xy/z)
														  :orientation orientation
														  :sign sign
														  :subdivisions 100))))
					  :lights (list spotlight))))
      (make-instance 'spotlight-scene
		     :window window
		     :children (list spotlight-root))
      
      (setf *spotlight-root* spotlight-root)
      (setf *spotlight-icon* spotlight-icon)
      (setf *spotlight* spotlight)

      (rm::show-window window)
      (rm::process-events :wait))))
