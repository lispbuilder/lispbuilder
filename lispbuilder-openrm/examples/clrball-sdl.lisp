
(in-package #:rm-examples)

(defclass clrball-window (rm::sdl-window) ()
  (:default-initargs
   :width 400 :height 300
   :name "window"
   :title-caption "Clrball" :icon-caption "Clrball"
   :events '(rm::on-quit rm::on-idle rm::on-paint)))

(defclass clrball-scene (rm::scene rm::aux-trackball)()
  (:default-initargs
   :dims :rm-renderpass-3d
   :opacity :rm-renderpass-all
   :name "scene"))

(defun clrball-1-sdl ()
  ;; Create a window.
  ;; Initializes SDL, OpenRM and creates an OpenGL context.
  (make-instance 'clrball-window)
  ;; Create a new scene.
  ;; A scene contains a camera.
  (make-instance 'clrball-scene
                 :window (rm::default-window)
                 ;; The transparent and opaque spheres are attached to the
                 ;; scene below the scene node.
                 :children (list
                            (rm::new-sphere :opacity :transparent
                                            :name "transparent"
                                            :radius 3.0
                                            :diffuse-color (rm:color 0.1 0.1 1.0 0.75)
                                            :ambient-color (rm::color 1.0 1.0 1.0 0.5)
                                            :specular-color (rm::color 0.1 0.1 1.0 1.0)
                                            :specular-exponent 20.0)
                            (rm::new-sphere :opacity :opaque
                                            :name "opaque" 
                                            :background-color (rm::color 0.0 0.0 0.0 0.0)
                                            :rgb/a (rm::color 1.0 0.0 0.0 1.0))))
  ;; Set the frame rate to 60fps.
  (setf (sdl:frame-rate) 60)
  ;; Start the event loop.
  (rm::process-events)
  ;; Close the window, clear the scene graph and close SDL.
  (rm::clean-up))

(defclass opaque-node (rm::node) ()
  (:default-initargs
   :dims :rm-renderpass-3d
   :opacity :rm-renderpass-opaque
   :compute-bounding-box t
   :background-color (rm::color 0.0 0.0 0.0 0.0)))

(defclass transparent-node (rm::node) ()
  (:default-initargs
   :dims :rm-renderpass-3d
   :opacity :rm-renderpass-transparent
   :compute-bounding-box t
   :diffuse-color (rm::color 0.1 0.1 1.0 0.75)
   :ambient-color (rm::color 1.0 1.0 1.0 0.5)
   :specular-color (rm::color 0.1 0.1 1.0 1.0)
   :specular-exponent 20.0))

(defclass opaque-sphere-prim (rm::sphere-primitive) ()
  (:default-initargs
   :radius 1.0
   :tesselate 512
   :xy/z (rm::vertex 0.0 0.0 0.0)
   :rgb/a (rm::color 1.0 0.0 0.0 1.0)))

(defclass transparent-sphere-prim (rm::sphere-primitive) ()
  (:default-initargs
   :radius 3.0
   :tesselate 512
   :xy/z (rm::vertex 0.0 0.0 0.0)))

(defun clrball-2-sdl ()
  ;; Create a window.
  ;; This also initializes SDL, OpenRM and creates the OpenGL context.  
  (make-instance 'clrball-window :scene (make-instance 'clrball-scene))

  ;; Create a new scene.
  ;; A scene contains a camera.
  ;; The transparent and opaque spheres are attached to the scene below the scene node.
  (rm::add-node (rm::make-instance 'opaque-node
                                   :name "opaque"
                                   :primitives (make-instance 'opaque-sphere-prim))
                (rm::default-scene))
  (rm::add-node (rm::make-instance 'transparent-node
                                   :name "transparent"
                                   :primitives (make-instance 'transparent-sphere-prim))
                (rm::default-scene))

  (rm::with-scene ((rm::default-scene) scene)
    (rm::union-all-boxes scene)
    (rm::compute-bounding-box scene)
    (rm::with-scene-camera (scene cam)
      (rm::compute-view-from-geometry cam scene)))

  ;; Set the frame rate to 60fps.
  (setf (sdl:frame-rate) 60)

  ;; Start the event loop.
  (rm::process-events)

  ;; Clean up.
  ;; This call closes the window, clears the scene graph and closes SDL.
  (rm::clean-up))
