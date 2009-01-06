
(in-package #:sdl-examples) 

(defun circle-1 ()
  (sdl:with-init ()
    (let ((100-frames-p (every-n-frames 100)))
      (sdl:window 320 240
                  :title-caption "Circle-1: Circles & Filled Circles"
                  :icon-caption "Circle-1: Circles & Filled Circles")
      (setf (sdl:frame-rate) 100)
      
      (sdl:initialise-default-font sdl:*font-5x7*)
      (draw-fps "Calculating FPS....." 10 200
                sdl:*default-font* sdl:*default-surface* t)
           
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:idle ()
         (sdl:draw-circle-* (random 320) (random 240) (random 320)
                            :color (sdl:color :r (random 255)
                                              :g (random 255)
                                              :b (random 255)))
         (sdl:draw-filled-circle-* (random 320) (random 240) (random 50)
                                   :color (sdl:color :r (random 255)
                                                     :g (random 255)
                                                     :b (random 255)))

         ;; Optimization; Draw the font each frame,
         ;; but only render the font once every 100 frames.
         (draw-fps (format nil "FPS : ~2$" (sdl:average-fps))
                   10 200 sdl:*default-font* sdl:*default-surface*
                   (funcall 100-frames-p))
         (sdl:update-display))))))

(defun circle-2 ()
  (sdl:with-init ()
    (let ((100-frames-p (every-n-frames 100)))      
      (sdl:window 320 240
		  :title-caption "Circle-2: Filled Circles with circumference"
		  :icon-caption "Circle-2: Filled Circles with circumference")
      (setf (sdl:frame-rate) 0)
      
      (sdl:initialise-default-font sdl:*font-5x7*)
      (draw-fps "Calculating FPS....."
                10 200 sdl:*default-font* sdl:*default-surface* t)
    
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:idle ()
         (sdl:draw-filled-circle-* (random 320) (random 240) (random 50)
                                   :color (sdl:color :r (random 255)
                                                     :g (random 255)
                                                     :b (random 255)
                                                     :a (random 255))
                                   :stroke-color sdl:*white*)

         ;; Optimization; Draw the font each frame,
         ;; but only render the font once every 100 frames.
         (draw-fps (format nil "FPS : ~2$" (sdl:average-fps))
                   10 200 sdl:*default-font* sdl:*default-surface*
                   (funcall 100-frames-p))	     
         (sdl:update-display))))))

(defun circle-3 ()
  (sdl:with-init ()
    (let ((100-frames-p (every-n-frames 100)))
      
      (sdl:window 640 480
                  :title-caption "Circle-3: Alpha Filled Circles"
                  :icon-caption "Circle-3: Alpha Filled Circles")
      (setf (sdl:frame-rate) 0)

      (sdl:initialise-default-font sdl:*font-5x7*)
      (draw-fps "Calculating FPS....."
                10 200 sdl:*default-font* sdl:*default-surface* t)
    
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:idle ()
         (sdl:draw-filled-circle-* (random 640) (random 480) (random 100)
                                   :color (sdl:color :r (random 255)
                                                     :g (random 255)
                                                     :b (random 255)
                                                     :a (random 255))
                                   :alpha 255)
         ;; Optimization; Draw the font each frame,
         ;; but only render the font once every 100 frames.
         (draw-fps (format nil "FPS : ~2$" (sdl:average-fps))
                   10 200 sdl:*default-font* sdl:*default-surface*
                   (funcall 100-frames-p))
         (sdl:update-display))))))

(defun circle-4 ()
  (sdl:with-init ()
    (let ((100-frames-p (every-n-frames 100)))

      (setf (sdl:frame-rate) 0)
      (sdl:window 320 240
                  :title-caption "Circle-4: Alpha Circles with circumference"
                  :icon-caption "Circle-4: Alpha Circles with circumference")

      (sdl:initialise-default-font sdl:*font-5x7*)
      (draw-fps "Calculating FPS....."
                10 200 sdl:*default-font* sdl:*default-surface* t)
    
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:idle ()
         (sdl:draw-filled-circle-* (random 320) (random 240) (random 50)
                                   :color (sdl:color :r (random 255)
                                                     :g (random 255)
                                                     :b (random 255)
                                                     :a (random 255))
                                   :alpha 255
                                   :stroke-color sdl:*white*)
         ;; Optimization; Draw the font each frame,
         ;; but only render the font once every 100 frames.
         (draw-fps (format nil "FPS : ~2$" (sdl:average-fps))
                   10 200 sdl:*default-font* sdl:*default-surface*
                   (funcall 100-frames-p))

         (sdl:update-display))))))

(defun circle-5 ()
  (sdl:with-init ()
    (let ((100-frames-p (every-n-frames 100)))

      (sdl:window 640 480
                  :title-caption "Circle-5: Filled Circles"
                  :icon-caption "Circle-5: Filled Circles")
      (setf (sdl:frame-rate) 0)

      (sdl:initialise-default-font sdl:*font-5x7*)
      (draw-fps "Calculating FPS....."
                10 200 sdl:*default-font* sdl:*default-surface* t)

      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:idle ()
         (sdl:draw-filled-circle-* (random 640) (random 480) (random 100)
                                   :color (sdl:color :r (random 255)
                                                     :g (random 255)
                                                     :b (random 255)))
         ;; Optimization; Draw the font each frame,
         ;; but only render the font once every 100 frames.
         (draw-fps (format nil "FPS : ~2$" (sdl:average-fps))
                   10 200 sdl:*default-font* sdl:*default-surface*
                   (funcall 100-frames-p))

         (sdl:update-display))))))
