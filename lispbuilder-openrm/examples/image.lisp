
(in-package #:rm-examples)

(defun image ()
  (make-instance 'rm::sdl-window
                 :width 400 :height 300
                 :title-caption "Simple Sprite Example"
                 :icon-caption "Simple Sprite Example")
  (let* ((sdl.bmp (sdl:create-path "sdl.bmp" sdl:*default-image-path*))
         (sprite (rm::new-sprite :p-xy/z (rm::v2d 0.0 0.0)
                                 :images (rm::load-image sdl.bmp))))
    (make-instance 'rm:scene
                   :window (rm::default-window)
                   :camera (make-instance 'rm::camera-2d :defaults t)
                   :compute-view-from-geometry nil
;                   :default-lighting t
                   :children sprite)
    (setf (sdl:frame-rate) 60)
    (rm::process-events)
    (rm::clean-up)))

(defun image-sdl ()
  (sdl:with-init()
    (sdl:window 320 240)
    (let* ((image (sdl:load-image (sdl:create-path "sdl.bmp" sdl:*default-image-path*)))
           (image-2 (sdl:create-surface (sdl:width image) (sdl:height image))))
      (sdl:draw-surface image :surface image-2)
    
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event (:KEY key)
         (when (sdl:key= key :sdl-key-escape)
           (sdl:push-quit-event)))
        (:idle ()
         (sdl:draw-surface image)
         (sdl:draw-surface-at image-2 #(0 100))
         (sdl:update-display))))))
