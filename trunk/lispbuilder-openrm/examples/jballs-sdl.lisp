;;;;; A version of the JBALLS example in rmdemos.
;;;;; Author: Luke J Crook, luke@balooga.com
;;;;; 02/25/2008
;;;;; 

(in-package #:rm-examples)

(defvar *moving-group* nil)
(defvar *stationary-group* nil)
(defvar *rotation-direction* nil)

(defclass jballs-sdl-window (rm::sdl-window rm::aux-window)()
  (:default-initargs
   :resizable t
   :width 800 :height 600
   :title-caption "Jballs" :icon-caption "Jballs"
   :name "window"))

(defclass jballs-sdl-scene (rm::scene rm::aux-trackball)()
  (:default-initargs
   :name "default-scene"
   :default-lighting nil
   :compute-bounding-box t))

(defclass node (rm::node)()
  (:default-initargs
   :dims :rm-renderpass-3D
   :opacity :rm-renderpass-opaque
   :compute-bounding-box t))

(defclass floor-quad (node)()
  (:default-initargs
   :name "floor"
   :compute-bounding-box t
   :primitives (list (make-instance 'rm::plane-primitive
                                    :orientation :xz
                                    :xy/z #(#(-3.0 -2.0 -3.0)
                                            #( 3.0 -2.0  3.0))))))

(defmethod rm::on-idle ((window jballs-sdl-window))
  (rm::rotate *moving-group* *rotation-direction*
	      :reverse (list *stationary-group*)))

(defun jballs-sdl ()
  (make-instance 'jballs-sdl-window)
  (let* ((stationary-group (make-instance 'node :name "stationary-group"
                                          :children (list (rm::new-sphere :name "sloth"
                                                                          :p-xy/z (rm::vertex 0.0 0.0 0.0)
                                                                          :radius 1.0
                                                                          :tesselate 512)
                                                          (make-instance 'floor-quad))))
         (moving-group (make-instance 'node :name "moving-group"
                                      :center (rm::vertex 0.0 0.0 0.0)
                                      :lights (make-instance 'rm::point-light
                                                             :light-source :rm-light-3
                                                             :specular-color (rm::color 1.0 0.0 0.0 1.0)
                                                             :diffuse-color (rm::color 1.0 0.0 0.0 1.0)
                                                             :xy/z (rm::vertex 1.5 -1.0 0.0))
                                      :children (list stationary-group
                                                      (rm::new-sphere :name "zippy"
                                                                      :specular-exponent 10.0
                                                                      :specular-color (rm::color 0.9 0.25 0.25 1.0)
                                                                      :rgb/a (rm::color 0.9 0.25 0.25 1.0)
                                                                      :p-xy/z (rm::vertex 1.5 -1.0 0.0)
                                                                      :radius 0.25
                                                                      :tesselate 128))))
         (rotation-direction (rm::v3d 0.0 -2.0 0.0)))
      
    (setf *moving-group* moving-group
          *stationary-group* stationary-group
          *rotation-direction* rotation-direction)

    (make-instance 'jballs-sdl-scene
                   :window (rm::default-window)
                   :children moving-group
                   :lights (make-instance 'rm::directional-light
                                          :light-source :rm-light-0
                                          :specular-color (rm::color 0.7 0.7 0.7 1.0)))
    (setf (sdl:frame-rate) 60)
    (rm::process-events)
    (rm::clean-up)))
