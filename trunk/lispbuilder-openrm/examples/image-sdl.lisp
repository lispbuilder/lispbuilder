
(in-package #:rm-examples)

(defun calculate-image-center (window image)
  (let ((image-x-center (* (/ 1 (rm:width window)) (rm:width image)))
        (image-y-center (* (/ 1 (rm:height window)) (rm:height image)))
        (window-x-center 0.0)
        (window-y-center 0.0))
    (rm:vertex (- window-x-center image-x-center)
               (- window-y-center image-y-center))))

(defun image-sdl ()
  ;; Create a new Window.
  (make-instance 'rm::sdl-window
                 :width 320 :height 240
                 :title-caption "Simple Sprite Example"
                 :icon-caption "Simple Sprite Example")
  ;; Create a Scene (camera+viewport) and add it to the Window
  (make-instance 'rm:scene
                 :window (rm::default-window)
                 :camera (make-instance 'rm::camera-2d)
                 :viewport #(0.0 0.0 1.0 1.0)
                 :compute-view-from-geometry nil)
  
  (let* ((image (rm::load-image (merge-pathnames "sdl.bmp" sdl:*default-image-path*)))
         (sprite (rm::new-sprite :p-xy/z (calculate-image-center (rm::default-window) image)
                                 :images image)))
    ;; Add the sprite to the Scene.
    (rm::add-node sprite (rm::default-scene)))
  
  (setf (sdl:frame-rate) 5)
  (rm::process-events)
  (rm::clean-up))
