
(in-package #:rm-examples)

(defclass cones-window (rm::sdl-window rm::aux-window) ()
  (:default-initargs
   :width 400 :height 300
   :title-caption "Cones" :icon-caption "Cones"
   :name "window"
   :pipe (make-instance 'rm::sdl-pipe :channel-format :RM-REDBLUE-STEREO-CHANNEL)
   :events '(rm::on-quit
             rm::on-idle)))

(defclass cones-scene (rm::scene rm::aux-trackball rm::aux-scene)()
  (:default-initargs
   :name "scene"
   :dims :rm-renderpass-3d
   :opacity :rm-renderpass-opaque
   :background-color (rm::c4d 0.4 0.4 0.4 1.0)
   :unlit-color (rm::c4d 1.0 1.0 1.0 1.0)
   :camera (make-instance 'rm::camera-3d
                          :stereo t
                          :eye-separation 2.5
                          :focal-distance 0.707
                          :defaults t)))

(defun cones-1-sdl ()
  (rm::make-instance 'cones-window)
  (rm::make-instance 'cones-scene
                     :window (rm::default-window)
                     :children (list (rm::new-cone :name "cone-node"
                                                   :p-xy/z (rm::v3d* '((5.0 0.0 0.0)
                                                                       (8.0 0.0 0.0)
                                                                       (0.0 5.0 0.0)
                                                                       (0.0 8.0 0.0)
                                                                       (0.0 0.0 5.0)
                                                                       (0.0 0.0 8.0)))
                                                   :radius '(1.0 1.0 1.0)
                                                   :rgb/a (rm::c4d* '((1.0 0.0 0.0 1.0)
                                                                      (0.0 1.0 0.0 1.0)
                                                                      (0.0 0.0 1.0 0.0))))
                                     (rm::new-cylinder :name "cylinder-node"
                                                       :p-xy/z (rm::v3d* '((0.0 0.0 0.0) (4.0 0.0 0.0)
                                                                           (0.0 0.0 0.0) (0.0 4.0 0.0)
                                                                           (0.0 0.0 0.0) (0.0 0.0 4.0)))
                                                       :radius '(0.3 0.3 0.3)
                                                       :rgb/a (rm::c4d* '((1.0 1.0 1.0 1.0)
                                                                          (1.0 1.0 1.0 1.0)
                                                                          (1.0 1.0 1.0 1.0))))
                                     (rm::new-sphere :name "sphere-node"
                                                     :p-xy/z (rm::v3d 0.0 0.0 0.0)
                                                     :radius 0.3
                                                     :rgb/a (rm::c4d 1.0 1.0 1.0 1.0))))
    
    (setf (sdl:frame-rate) 60)
    (rm::process-events)
    (rm::clean-up))

(defun cones-2-sdl ()
  (rm::make-instance 'cones-window)
  (let ((cone-node (rm::make-instance 'rm::node :name "cone-node"
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
                                          :primitives (list (make-instance 'rm::cylinder-primitive
                                                                           :xy/z (rm::v3d* '((0.0 0.0 0.0) (4.0 0.0 0.0)
                                                                                             (0.0 0.0 0.0) (0.0 4.0 0.0)
                                                                                             (0.0 0.0 0.0) (0.0 0.0 4.0)))
                                                                           :radius '(0.3 0.3 0.3)
                                                                           :rgb/a (rm::c4d* '((1.0 1.0 1.0 1.0)
                                                                                              (1.0 1.0 1.0 1.0)
                                                                                              (1.0 1.0 1.0 1.0)))))))
        (sphere-node (rm::make-instance 'rm::node :name "sphere-node"
                                        :primitives (list (make-instance 'rm::sphere-primitive
                                                                         :xy/z (rm::v3d 0.0 0.0 0.0)
                                                                         :radius 0.3
                                                                         :rgb/a (rm::c4d 1.0 1.0 1.0 1.0))))))
    (rm::make-instance 'cones-scene
                       :window (rm::default-window)
                       :children (list cone-node cylinder-node sphere-node))
      
    (setf (sdl:frame-rate) 60)
    (rm::process-events)
    (rm::clean-up)))
