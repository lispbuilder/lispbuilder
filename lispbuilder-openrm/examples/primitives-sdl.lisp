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
                 :window (rm::default-window)
                 :children (list
                            (rm::new-sphere :radius 1.0
                                            :tesselate 512
                                            :rgb/a (rm::c4d 1.0 1.0 0.0 1.0)
                                            :xy/z (rm::v3d 0.0 0.0 0.0))))
  (rm::process-events)
  (rm::clean-up))

(defun sphere-example-2 ()
  (make-instance 'rm::sdl-window
                 :title-caption "Sphere Example 2" :icon-caption "Sphere Example 2"
                 :width 320 :height 240)
  (make-instance 'rm::scene
                 :dims :rm-renderpass-3d
                 :opacity :rm-renderpass-all
                 :window (rm::default-window)
                 :children (list
                            (make-instance 'rm::node
                                           :compute-bounding-box t
                                           :primitives (list
                                                        (make-instance 'rm::sphere-primitive
                                                                       :radius 1.0
                                                                       :tesselate 512
                                                                       :rgb/a (rm::c4d 1.0 1.0 0.0 1.0)
                                                                       :xy/z (rm::v3d 0.0 0.0 0.0)))
                                           )))
  (rm::process-events)
  (rm::clean-up))

;;;
;;;

(defclass window (rm::sdl-window) ()
  (:default-initargs
   :title-caption "Sphere Example 3" :icon-caption "Sphere Example 3"
   :width 320 :height 240))

(defclass sphere-prim (rm::sphere-primitive) ()
  (:default-initargs
   :tesselate 512
   :radius 1.0
   :rgb/a (rm::c4d 1.0 1.0 0.0 1.0)
   :xy/z (rm::v3d 0.0 0.0 0.0)))

(defun sphere-example-3 ()
  (make-instance 'window)
  (make-instance 'rm::scene
                 :window (rm::default-window)
                 :dims :rm-renderpass-3d
                 :opacity :rm-renderpass-all
                 :children (list
                            (make-instance 'rm::node
                                           :compute-bounding-box t
                                           :children (list
                                                      (make-instance 'sphere-prim)))))
  (rm::process-events)
  (rm::clean-up))

;;;
;;;

(defun sphere-example-4 ()
  (make-instance 'window :title-caption "Sphere Example 4" :icon-caption "Sphere Example 4")
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
          (rm::rgb/a prim) (rm::c4d 1.0 1.0 0.0 1.0)
          (rm::xy/z prim) (rm::v3d 0.0 0.0 0.0))
    
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
    
    ;; Start event loop.
    (rm::process-events)

    ;; Delete the Scene Graph. Attempt to close stray windows
    (rm::clean-up)))
