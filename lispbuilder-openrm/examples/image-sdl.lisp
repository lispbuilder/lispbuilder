
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
  
  (let* ((image (rm::load-image (merge-pathnames "sdl.bmp" sdl:*default-image-path*)
                                :copy t))
         (sprite (rm::new-sprite :p-xy/z (calculate-image-center (rm::default-window) image)
                                 :images image)))
    ;; Add the sprite to the Scene.
    (rm::add-node sprite (rm::default-scene)))

  (setf (sdl:frame-rate) 5)
  (rm::process-events)
  (rm::clean-up))


(defun draw-fps (string x y font surface render-p)
  (when render-p
    (sdl:render-string-shaded string sdl:*white* sdl:*black*
                              :font font :cache t :free t))
  (sdl:draw-font-at-* x y :font font :surface surface))

(defun sdl-circles ()
  (make-instance 'rm::sdl-window
                 :width 320 :height 240
                 :title-caption "Using OpenRM as a backend for SDL."
                 :icon-caption "Using OpenRM as a backend for SDL.")
  (make-instance 'rm:scene
                 :window (rm::default-window)
                 :camera (make-instance 'rm::camera-2d)
                 :viewport #(0.0 0.0 1.0 1.0)
                 :compute-view-from-geometry nil)

  (let* ((image (rm::load-image sdl:*default-display*))
         (sprite (rm::new-sprite :xy/z (calculate-image-center (rm::default-window) image)
                                 :images image)))

    (rm::add-node sprite (rm::default-scene))

    (let ((100-frames-p (every-n-frames 100)))
      
      (sdl:initialise-default-font sdl:*font-5x7*)
      (draw-fps "Calculating FPS....." 10 200
                sdl:*default-font* sdl:*default-surface* t)
      
      (setf (sdl:frame-rate) nil)
      
      (sdl:with-events ()
        (:key-down-event ()
         (sdl:push-quit-event))
        (:quit-event () t)
        (:idle ()
         (sdl:draw-circle-* (random 320) (random 240) (random 320)
                            :color (sdl:color :r (random 255)
                                              :g (random 255)
                                              :b (random 255)))
         (sdl:draw-filled-circle-* (random 320) (random 240) (random 50)
                                   :color (sdl:color :r (random 255)
                                                     :g (random 255)
                                                     :b (random 255)))
         (draw-fps (format nil "FPS : ~2$" (sdl:average-fps))
                   10 200 sdl:*default-font* sdl:*default-surface*
                   (funcall 100-frames-p))
       
         (rm::render)))))
  (rm::clean-up))



(defun sdl-circles-2 ()
  (make-instance 'rm::sdl-window
                 :width 320 :height 240
                 :title-caption "Using OpenRM as a backend for SDL."
                 :icon-caption "Using OpenRM as a backend for SDL.")
  (make-instance 'rm:scene
                 :window (rm::default-window)
                 :camera (make-instance 'rm::camera-2d)
                 :viewport #(0.0 0.0 1.0 1.0)
                 :compute-view-from-geometry nil)

  (let* ((image (rm::load-image sdl:*default-display*))
         (sprite (rm::new-sprite :xy/z (calculate-image-center (rm::default-window) image)
                                 :images image)))

    (rm::add-node sprite (rm::default-scene))

    (setf (sdl:frame-rate) nil)
      
    (sdl:with-events ()
      (:key-down-event ()
       (sdl:push-quit-event))
      (:quit-event () t)
      (:idle ()
       (sdl:draw-filled-circle-* (random 320) (random 240) (random 50)
                                 :color (sdl:color :r (random 255)
                                                   :g (random 255)
                                                   :b (random 255)))
       (draw-fps (format nil "FPS : ~2$" (sdl:average-fps))
                 10 200 sdl:*default-font* sdl:*default-surface*
                 (funcall 100-frames-p))
       
       (rm::render))))
  (rm::clean-up))


