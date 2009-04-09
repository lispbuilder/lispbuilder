
(in-package #:rm-examples)

(defparameter *image-width* 1024)
(defparameter *image-width* 960)

(defun build-image (a)
  (let* ((image-width 8) (image-height 8)
         (image (make-instance 'rm::image  
                               :type 2
                               :dims (vector image-width image-height)
                               :depth 1
                               :format :rm-image-rgba
                               :data-type :rm-float))
         (s (make-instance 'rm::c4d :r a :g 0.0 :b (- 1.0 a) :a 1.0)))
    (rm::copy-color (rm::image-data image) s :end (* image-width image-height))
    image))

(defun build-tiles ()
  (let* ((node (make-instance 'rm::node
                              :dims :rm-renderpass-all
                              :opacity :rm-renderpass-all))
         (sprite (make-instance 'rm::sprite-primitive
                                :images (list (build-image 1.0))
                                :xy/z (rm::v2d 0.0 0.0))))
    ;(setf (rm::unlit-color node) (rm::c4d 0.0 1.0 0.0 1.0))
    (rm:add-primitive sprite node)))

(defun sprite-test-sdl ()
  (rm::make-instance 'rm::window :width 320 :height 240
                     :scene (make-instance 'rm::scene
                                           :dims :rm-renderpass-2d
                                           :opacity :rm-renderpass-opaque
                                           ;:background-color (rm::c4d 0.3 0.3 0.3 1.0)
                                           :camera (make-instance 'rm::camera-2d :defaults t)
                                           :primitives (list (build-tiles))))
  (rm::process-events)
  (rm::clean-up))
