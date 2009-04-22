;;;;; A simple example to verify the correctness of the Win32 and OpenRM packages.
;;;;; Author: Luke J Crook, luke@balooga.com
;;;;; 

(in-package #:rm-examples)

(defun sphere-example-1 ()
  (make-instance 'rm::sdl-window
                 :title-caption "Sphere Example 1" :icon-caption "Sphere Example 1"
                 :width 320 :height 240)
  (make-instance 'rm::scene
                 :dims :rm-renderpass-3d
                 :opacity :rm-renderpass-all
                 :compute-bounding-box t
                 :compute-view-from-geometry t
                 :union-all t
                 :window (rm::default-window)
                 :children (rm::new-sphere :radius 1.0
                                           :tesselate 512
                                           :rgb/a (rm:color 1.0 1.0 0.0 1.0)
                                           :xy/z (rm::vertex 0.0 0.0 0.0)))
  (rm::process-events)
  (rm::clean-up))

(defun sphere-example-2 ()
  (make-instance 'rm::sdl-window
                 :title-caption "Sphere Example 2" :icon-caption "Sphere Example 2"
                 :width 320 :height 240
                 :scene (make-instance 'rm::scene
                                       :dims :rm-renderpass-3d
                                       :opacity :rm-renderpass-all
                                       :children (rm::new-sphere :radius 1.0
                                                                  :tesselate 512
                                                                  :rgb/a (rm::c4d 1.0 1.0 0.0 1.0))))
  (rm::with-scene ((rm::default-scene) scene)
    ;; Point the camera at the object. Camera location and look angle are
    ;; calculated from the bounding boxes specified previously.
    (rm::with-scene-camera (scene cam)
      (rm::compute-view-from-geometry cam scene)))
  (rm::process-events)
  (rm::clean-up))

(defun sphere-example-3 ()
  (make-instance 'rm::sdl-window
                 :title-caption "Sphere Example 3" :icon-caption "Sphere Example 3"
                 :width 320 :height 240)
  (make-instance 'rm::scene
                 :dims :rm-renderpass-3d
                 :opacity :rm-renderpass-all
                 :window (rm::default-window)
                 :children (make-instance 'rm::node
                                          :compute-bounding-box t
                                          :primitives (make-instance 'rm::sphere-primitive
                                                                     :radius 1.0
                                                                     :tesselate 512
                                                                     :rgb/a (rm::c4d 1.0 1.0 0.0 1.0)
                                                                     :xy/z (rm::v3d 0.0 0.0 0.0))))
  (rm::process-events)
  (rm::clean-up))

;;;
;;;

(defclass window (rm::sdl-window) ()
  (:default-initargs
   :title-caption "Sphere Example 4" :icon-caption "Sphere Example 4"
   :width 320 :height 240))

(defclass sphere-prim (rm::sphere-primitive) ()
  (:default-initargs
   :tesselate 512
   :radius 1.0
   :rgb/a (rm::c4d 1.0 1.0 0.0 1.0)
   :xy/z (rm::v3d 0.0 0.0 0.0)))

(defun sphere-example-4 ()
  (make-instance 'window)
  (make-instance 'rm::scene
                 :window (rm::default-window)
                 :dims :rm-renderpass-3d
                 :opacity :rm-renderpass-all
                 :children (make-instance 'rm::node
                                           :compute-bounding-box t
                                           :children (make-instance 'sphere-prim)))
  (rm::process-events)
  (rm::clean-up))

;;;
;;;

(defun sphere-example-5 ()
  (make-instance 'window :title-caption "Sphere Example 5" :icon-caption "Sphere Example 5")
  (let ((scene (make-instance 'rm::scene
                              :dims :rm-renderpass-3d
                              :opacity :rm-renderpass-all))
        (sphere-node (make-instance 'rm::node))
        (prim (make-instance 'rm::sphere-primitive)))

    ;; Add the scene to the window.
    (rm::add-scene scene (rm::default-window))

    ;; Add the sphere node to the scene.
    (rm::add-node sphere-node scene)
    
    ;; Create the sphere primitive
    (setf (rm::radius prim) 1.0
          (rm::tesselate prim) 512
          (rm::rgb/a prim) (rm:color 1.0 1.0 0.0 1.0)
          (rm::xy/z prim) (rm:vertex 0.0 0.0 0.0))
    
    ;; Add the sphere primitive to the parent node.
    (rm::add-primitive prim sphere-node)
    (rm::compute-bounding-box sphere-node)
    
    ;; Make sure to compute bounding boxs
    ;; as this will be used to determine where to place the camera.
    (rm::with-scene ((rm::default-scene (rm::default-window)) scene)
      (rm::union-all-boxes scene)
      (rm::compute-bounding-box scene)
      ;; Point the camera at the object. Camera location and look angle are
      ;; calculated from the bounding boxes specified previously.
      (rm::with-scene-camera (scene cam)
        (rm::compute-view-from-geometry cam scene)))

    (format t "Window pipe       color: ~A~%" (rm::background-color (rm::default-window)))
    (format t "Window background color: ~A~%" (rm::background-color (rm::default-window)))
    (format t "Window root-node-p     : ~A~%" (rm::root-node-p (rm::default-window)))
    (format t "Window xy/z            : ~A~%" (rm::xy/z (rm::default-window)))
    (format t "Window camera-p        : ~A~%" (rm::camera-p (rm::default-window)))
    (format t "Window background color: ~A~%" (rm::background-color (rm::default-window)))
    (format t "Window unlit      color: ~A~%" (rm::unlit-color (rm::default-window)))
    (format t "Window ambient    color: ~A~%" (rm::ambient-color (rm::default-window)))
    (format t "Window diffuse    color: ~A~%" (rm::diffuse-color (rm::default-window)))
    (format t "Window specular   color: ~A~%" (rm::specular-color (rm::default-window)))
    (format t "~%")
    (format t "Scene  background color: ~A~%" (rm::background-color (rm::default-scene)))
    (format t "Scene  root-node-p     : ~A~%" (rm::root-node-p (rm::default-scene)))
    (format t "Scene  camera-p        : ~A~%" (rm::camera-p (rm::default-scene)))
    (format t "Scene  xy/z            : ~A~%" (rm::xy/z (rm::default-scene)))
    (format t "Scene  bounding-box    : ~A~%" (rm::bounding-box (rm::default-scene)))
    (format t "Scene  background color: ~A~%" (rm::background-color (rm::default-scene)))
    (format t "Scene  unlit      color: ~A~%" (rm::unlit-color (rm::default-scene)))
    (format t "Scene  ambient    color: ~A~%" (rm::ambient-color (rm::default-scene)))
    (format t "Scene  diffuse    color: ~A~%" (rm::diffuse-color (rm::default-scene)))
    (format t "Scene  specular   color: ~A~%" (rm::specular-color (rm::default-scene)))
    (format t "Scene  nodes-p         : ~A~%" (rm::nodes-p (rm::default-scene)))
    (format t "Scene  primitives-p    : ~A~%" (rm::primitives-p (rm::default-scene)))
    (format t "~%")
    (format t "Node   xy/z            : ~A~%" (rm::xy/z sphere-node))
    (format t "Node   bounding-box    : ~A~%" (rm::bounding-box sphere-node))
    (format t "Node   background color: ~A~%" (rm::background-color sphere-node))
    (format t "Node   unlit      color: ~A~%" (rm::unlit-color sphere-node))
    (format t "Node   ambient    color: ~A~%" (rm::ambient-color sphere-node))
    (format t "Node   diffuse    color: ~A~%" (rm::diffuse-color sphere-node))
    (format t "Node   specular   color: ~A~%" (rm::specular-color sphere-node))
    (format t "Node   nodes-p         : ~A~%" (rm::nodes-p sphere-node))
    (format t "Node   primitives-p    : ~A~%" (rm::primitives-p sphere-node))
    (format t "~%")
    (format t "primitive bounding-box : ~A~%" (rm::bounding-box prim))
    
    ;; Start event loop.
    (rm::process-events)
    ;; Delete the Scene Graph. Attempt to close stray windows
    (rm::clean-up)))

(defclass floor-quad (node)()
  (:default-initargs
   :name "floor"
   :compute-bounding-box t
   :primitives (list (make-instance 'rm::plane-primitive
                                    :orientation :xz
                                    :xy/z '(#(-3.0 -2.0 -3.0)
                                            #(3.0 -2.0 3.0))))))

(defun quad-example-1 ()
  (make-instance 'rm::sdl-window
                 :title-caption "Quad Example 1" :icon-caption "Quad Example 1"
                 :width 320 :height 240)
  (make-instance 'rm::scene
                 :dims :rm-renderpass-3d
                 :opacity :rm-renderpass-all
                 :compute-bounding-box t
                 :compute-view-from-geometry t
                 :union-all t
                 :window (rm::default-window)
                 :children (make-instance 'floor-quad))
  (rm::process-events)
  (rm::clean-up))
