;;;;; A version of the SPOTLIGHT demo from OpenRM rmdemos.
;;;;; Author: Luke J Crook, luke@balooga.com
;;;;; 02/25/2008
;;;;;

(in-package #:rm-examples)

(defvar *width* 320)
(defvar *height* 240)
(defvar *background-color* (rm::color 0.2 0.2 0.3 1.0))

(defvar *spot-color* (rm::color 0.9 0.5 0.5 1.0))
(defvar *spot-exponent* 4.0)
(defparameter *spot-xy/z* (rm:vertex 5.0 5.0 5.0))
(defparameter *spot-direction* (rm:vertex 0.0 -1.0 0.0))

(defparameter *arc* nil)
(defparameter *dummy* nil)
(defparameter *spotlight-root* nil)
(defparameter *spotlight-icon* nil)
(defparameter *spotlight* nil)
(defparameter *walls* nil)

(defclass spotlight-sdl-window (rm::sdl-window rm::aux-window)()
  (:default-initargs
   :width *width*
   :height *height*
   :name "window"
   :title-caption "Spotlight" :icon-caption "Spotlight"
   :events '(rm::on-idle rm::on-quit rm::on-mouse-down rm::on-mouse-move rm::on-active)))

(defclass camera (rm::camera-3d) ()
  (:default-initargs
   :eye (rm::vertex 5.0 5.0 22.0)
   :at (rm::vertex 5.0 5.0 0.0)
   :up-vector (rm::vertex 0.0 1.0 0.0)
   :hither 5.0
   :yon 40.0
   :fov 45.0
   :aspect-ratio (vector *width* *height*)
   :projection :rm-projection-perspective))
  
(defclass spotlight-sdl-scene (rm::scene rm::aux-scene) ()
  (:default-initargs
   :name "scene"
   :dims :rm-renderpass-3d
   :opacity :rm-renderpass-opaque
   :background-color *background-color*
   :camera (make-instance 'camera)
   :lights (list (make-instance 'rm::arena-light :xy/z (rm::color 5.0 7.5 5.0)))
   :compute-view-from-geometry nil
   :default-lighting nil))

(defclass spotlight (rm::spotlight) ()
  (:default-initargs
   :light-source :rm-light-1
   :diffuse-color *spot-color*
   :specular-color *spot-color*
   :cutoff rm::*default-spot-cutoff*
   :exponent *spot-exponent*
   :xy/z *spot-xy/z*
   :direction *spot-direction*))

(defmethod rm::on-mouse-down ((window spotlight-sdl-window) button x y)
  (when (rm::button= button :button-left)
    (rm::reset-arc *arc* *dummy* *width* *height* x y)))

(defmethod rm::on-mouse-move ((window spotlight-sdl-window) button x y x-rel y-rel)                       
  (when (rm::button= button :button-left)
    (rm::update-arc *arc* *dummy* *width* *height* x y)
    ;; Rotate spotlight
    (setf (rm::light *spotlight-root*)
          (make-instance 'spot-light :direction (rm::point-direction *dummy* *spot-direction* #(0.0 0.0 0.0))))
    ;; Rotate spotlight-icon
    (rm::rotate *spotlight-icon* *dummy*)))

;;;;; -----------------
;;;;; -----------------
;;;;; -----------------

(defun spotlight-sdl ()
  (make-instance 'spotlight-sdl-window)
  (setf *spot-direction* (rm:vertex 0.0 -1.0 0.0))
  (let ((quads '(((#(0.0   0.0 0.0) #(10.0  0.0 10.0)) :xz  1)
                 ((#(0.0  10.0 0.0) #(10.0 10.0 10.0)) :xz -1)
                 ((#(0.0   0.0 0.0) #( 0.0 10.0 10.0)) :yz  1)
                 ((#(10.0  0.0 0.0) #(10.0 10.0 10.0)) :yz -1)
                 ((#(0.0   0.0 0.0) #(10.0 10.0  0.0)) :xy  1))))

    (setf *arc* (make-instance 'rm::arc)
          *dummy* (make-instance 'rm::node)
          *spotlight* (make-instance 'spot-light))
    
    (setf *spotlight-icon* (rm::new-cone :name "spotlight-icon"
                                         :xy/z (rm::xy/z *spotlight*)
                                         :diffuse-color (rm::diffuse-color *spotlight*)
                                         :specular-color (rm::specular-color *spotlight*)
                                         :ambient-color (rm::ambient-color *spotlight*)
                                         :rgb/a *spot-color*
                                         :radius (coerce (rm::to-radian rm::*default-spot-cutoff*)
                                                         'single-float)
                                         :p-xy/z (vector *spot-direction*
                                                         (rm:vertex 0.0 0.0 0.0))
                                         :tesselate 32))

    (setf *walls* (make-instance 'rm::node :name "quad"
                                 :primitives (loop for (xy/z orientation sign) in quads
                                                   collecting (make-instance 'rm::plane-primitive
                                                                             :xy/z (rm::v3d* nil :initial-contents xy/z)
                                                                             :orientation orientation
                                                                             :sign sign
                                                                             :subdivisions 100))))
    (setf *spotlight-root*
          (make-instance 'rm::node :name "spotlight"
                         :lights (list *spotlight*)
                         :children (list *spotlight-icon* *walls*)))

    (make-instance 'spotlight-sdl-scene
                   :window (rm::default-window)
                   :children (list *spotlight-root*))
    
    (setf (sdl:frame-rate) 60)
    (rm::process-events)
    (rm::clean-up)))

