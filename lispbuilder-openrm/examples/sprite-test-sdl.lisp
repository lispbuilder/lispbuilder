
(in-package #:rm-examples)

(defun every-n-frames (max)
  (let ((count 0))
    #'(lambda ()
	(if (eql 0 (mod (incf count 1) max))
	    (setf count 0)
	    nil))))

(defun build-image (a)
  (let* ((image-width 8) (image-height 8)
         (image (make-instance 'rm::image
                               :type 2
                               :dims (vector image-width image-height)
                               :depth 1
                               :format :rm-image-rgba
                               :data-type :rm-float
                               :image-data (rm::color* (* image-width image-height)
                                                       :initial-element (rm::color a 0.0 (- 1.0 a) 1.0)))))
    image))

(defun build-tiles (num-width num-height)
  (let* ((x (+ -1.0 0.05))
         (dx (/ 1.9 num-width))
         (y (+ -1.0 0.05))
         (dy (/ 1.9 num-height))
         (a 1.0)
         (da (/ -1.0 (* num-width num-height)))
         (sprites nil))
    (dotimes (j num-height)
      (dotimes (i num-width)
        (incf a da)
        (push (rm::new-sprite :dims :rm-renderpass-2d
                              :opacity :opaque
                              :images (build-image a)
                              :xy/z (rm::vertex (+ x (* i dx))
                                                (+ y (* j dy)))
                              :display-list t)
              sprites)))
    sprites))

(defun sprite-test-sdl ()
  (rm::make-instance 'rm::sdl-window
                     :width 1024
                     :height 960
                     :title-caption "Sprite Test"
                     :icon-caption "Sprite Test"
                     :pipe (make-instance 'rm::sdl-pipe
                                          :display-list t
                                          :opaque-3d t
                                          :transparent-3d t
                                          :opaque-2d t))
  (make-instance 'rm::scene
                 :dims :rm-renderpass-2d
                 :opacity :rm-renderpass-opaque
                 :window (rm::default-window)
                 :background-color (rm::color 0.3 0.3 0.3 1.0)
                 :camera (make-instance 'rm::camera-2d)
                 :compute-view-from-geometry nil
                 :children (make-instance 'rm::node
                                          :name "blocks"
                                          :dims :rm-renderpass-all
                                          :opacity :rm-renderpass-all
                                          :unlit-color (rm::color 0.0 1.0 0.0 1.0)
                                          :children (build-tiles 60 60)))
  (setf (sdl:frame-rate) nil)

  (let ((frame-test (every-n-frames 50)))
    (sdl:with-events ()
      (:key-down-event () (sdl:push-quit-event))
      (:quit-event () t)
      (:video-expose-event () (rm::render))
      (:idle ()
       (when (funcall frame-test)
         (rm::format t "Frame Rate: ~A~%" (rm::cast float (sdl:average-fps))))
       (rm::render))))
  (rm::clean-up))
