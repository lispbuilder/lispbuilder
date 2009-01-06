;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; see COPYING for license

(in-package #:sdl-examples) 

(defun random-rects ()
  (let ((width 320) (height 240))
    (sdl:with-init ()
      (let ((100-frames-p (every-n-frames 1000))
	    (rect (sdl:rectangle)))
	    
	(sdl:window width height
		    :title-caption "random-rects"
		    :icon-caption "random-rects")
        (setf (sdl:frame-rate) 0)

	(sdl:initialise-default-font sdl:*font-5x7*)
	(draw-fps "Calculating FPS....." 10 200
		  sdl:*default-font* sdl:*default-surface* t)

	(sdl:with-events ()
	  (:quit-event () t)
	  (:key-down-event (:key key)
           (if (sdl:key= key :SDL-KEY-ESCAPE)
             (sdl:push-quit-event)))
	  (:idle ()
           (sdl:draw-rectangle (sdl:random-rectangle width height rect)
                               :color (sdl:color :r (random 255)
                                                 :g (random 255)
                                                 :b (random 255))
                               :clipping nil)

           ;; Optimization; Draw the font each frame,
           ;; but only render the font once every 100 frames.
           (draw-fps (format nil "FPS : ~2$" (sdl:average-fps))
                     10 200 sdl:*default-font* sdl:*default-surface*
                     (funcall 100-frames-p))
           (sdl:update-display)))))))

(defun random-rect ()
  (let ((width 320) (height 240))
    (sdl:with-init ()     
      (sdl:window width height
                  :title-caption "random-rect"
                  :icon-caption "random-rect")
      (setf (sdl:frame-rate) 5)
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event (:key key)
         (if (sdl:key= key :SDL-KEY-ESCAPE)
           (sdl:push-quit-event)))
        (:idle ()
         (sdl:draw-hline 10 100 10 :color (sdl:color :r 255 :g 255 :b 255))
         (sdl:draw-hline 10 100 100 :color (sdl:color :r 255 :g 255 :b 255))
         (sdl:draw-vline 10 10 100 :color (sdl:color :r 255 :g 255 :b 255))
         (sdl:draw-vline 100 10 100 :color (sdl:color :r 255 :g 255 :b 255))
         (sdl:with-rectangle (template (sdl:rectangle))
           (sdl:draw-hline 210 300 10
                           :color (sdl:color :r 255 :g 255 :b 255)
                           :template template)
           (sdl:draw-hline 210 300 100
                           :color (sdl:color :r 255 :g 255 :b 255)
                           :template template)
           (sdl:draw-vline 210 10 100
                           :color (sdl:color :r 255 :g 255 :b 255)
                           :template template)
           (sdl:draw-vline 300 10 100
                           :color (sdl:color :r 255 :g 255 :b 255)
                           :template template))
         (sdl:draw-rectangle-* 10 110 90 90
                               :color (sdl:color :r 255 :g 255 :b 255))
         (sdl:update-display))))))

(defun random-box-1 ()
  (let ((width 320) (height 240))
    (sdl:with-init ()
      (let ((rect (sdl:rectangle)))	
        (sdl:window width height
		    :title-caption "Random solid box"
		    :icon-caption "Random solid box")
        (setf (sdl:frame-rate) 0)
	(sdl:with-events (:poll)
	  (:quit-event () t)
	  (:key-down-event (:key key) (if (sdl:key= key :SDL-KEY-ESCAPE)
                                        (sdl:push-quit-event)))
	  (:idle ()
           (sdl:fill-surface-* (random 255) (random 255) (random 255)
                               :template (sdl:random-rectangle width height
                                                               rect)
                               :update t)))))))

(defun random-box-2 ()
  (let ((width 320) (height 240))
    (sdl:with-init ()
      ;; Using the :WAIT event model so me may as well unlock the frame rate.      
      (sdl:window width height
                  :title-caption "Random solid box with outline"
                  :icon-caption "Random solid box with outline")
      (setf (sdl:frame-rate) 0)

      (sdl:initialise-default-font sdl:*font-10x20*)
      (sdl:draw-string-shaded-* "Move mouse over window." 0 0
				sdl:*white* sdl:*black*)
      (sdl:update-display)
      (sdl:with-events (:wait)
	(:quit-event () t)
	(:key-down-event (:key key)
         (if (sdl:key= key :SDL-KEY-ESCAPE) (sdl:push-quit-event)))
	(:video-expose-event () (sdl:update-display))
	(:key-down-event (:key key)
         (when (sdl:key= key :SDL-KEY-ESCAPE)
           (sdl:push-quit-event)))
	(:mouse-motion-event ()
         (sdl:draw-box (sdl:random-rectangle width height)
                       :color (sdl:color :r (random 255)
                                         :g (random 255)
                                         :b (random 255))
                       :stroke-color sdl:*white*)
         (sdl:draw-string-shaded-* "Move mouse over window."
                                   0 0 sdl:*white* sdl:*black*)
         (sdl:update-display))))))

(defun random-box-3 ()
  (let ((width 320) (height 240))
    (sdl:with-init ()
      (let ((500-frames-p (every-n-frames 500))
	    (rect (sdl:rectangle)))
	        
	(sdl:window width height
		    :title-caption "Random alpha Box"
		    :icon-caption "Random alpha Box")
        (setf (sdl:frame-rate) 0)

      	(sdl:initialise-default-font sdl:*font-5x7*)
	(draw-fps "Calculating FPS....." 10 200
		  sdl:*default-font* sdl:*default-surface* t)

	(sdl:with-events ()
	  (:quit-event () t)
          (:key-down-event (:key key)
           (when (sdl:key= key :SDL-KEY-ESCAPE)
             (sdl:push-quit-event)))
	  (:idle ()
           (sdl:draw-box (sdl:random-rectangle width height rect)
                         :color (sdl:color :r (random 255)
                                           :g (random 255)
                                           :b (random 255)
                                           :a (random 255))
                         :alpha 255)

           ;; Optimization; Draw the font each frame,
           ;; but only render the font once every 100 frames.
           (draw-fps (format nil "FPS : ~2$" (sdl:average-fps))
                     10 200 sdl:*default-font* sdl:*default-surface*
                     (funcall 500-frames-p))

           (sdl:update-display)))))))


(defun random-box-4 ()
  (let ((width 320) (height 240))
    (sdl:with-init ()
      (let ((500-frames-p (every-n-frames 500)))
	              
	(sdl:window width height
                    :title-caption "Random alpha box with outline"
                    :icon-caption "Random alpha Box with outline")
        (setf (sdl:frame-rate) 0)

      	(sdl:initialise-default-font sdl:*font-5x7*)
	(draw-fps "Calculating FPS....."
                  10 200 sdl:*default-font* sdl:*default-surface* t)

	(sdl:with-events ()
	  (:quit-event () t)
	  (:key-down-event (:key key)
           (when (sdl:key= key :SDL-KEY-ESCAPE) (sdl:push-quit-event)))
	  (:video-expose-event () (sdl:update-display))
	  (:idle ()
           (sdl:draw-box (sdl:random-rectangle width height)
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
                     (funcall 500-frames-p))

           (sdl:update-display)))))))
