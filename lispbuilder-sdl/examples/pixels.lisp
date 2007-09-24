;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones
;;;; see COPYING for license

(in-package #:sdl-examples) 
  
(defun pixels-1 ()
  (sdl:with-init ()
    (sdl:window 640 480)
    (setf (sdl:frame-rate) 0)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
	     (sdl:draw-point-* (random 640) (random 480)
			       :color (sdl:color :r (random 255)
						 :g (random 255)
						 :b (random 255))
			       :surface sdl:*default-display*)
	     (sdl:update-display)))))

(defun pixels-2 ()
  (let ((width 640) (height 480))
    (sdl:with-init ()
      
      (sdl:window width height)
      (setf (sdl:frame-rate) 0)

      (sdl:with-color (color (sdl:color))
	
	(sdl:with-events ()
	  (:quit-event () t)
	  (:idle ()
		 (sdl-base::with-pixel (disp (sdl:fp sdl:*default-display*))
		   (sdl-base::write-pixel disp (random width) (random height)
					  (sdl:map-color (sdl:set-color-* color :r (random 255)
										:g (random 255)
										:b (random 255))
							 sdl:*default-display*)))
		 (sdl:update-display)))))))

(defun pixels-3 ()
  (let ((width 640) (height 480))
    (sdl:with-init ()

      (sdl:window width height)
      (setf (sdl:frame-rate) 0)

      (sdl:with-rectangle (template (sdl:rectangle :w 1 :h 1))
	(sdl:with-color (color (sdl:color))
	
	  (sdl:with-events ()
	    (:quit-event () t)
	    (:idle ()
		   (sdl:fill-surface (sdl:set-color-* color :r (random 255) :g (random 255) :b (random 255))
				     :template (sdl:set-rectangle-* template :x (random width) :y (random height))
				     :update-p t
				     :surface sdl:*default-display*))))))))

(defun pixels-4 ()
  (let ((width 640) (height 480))
    (sdl:with-init ()

      (sdl:window width height)
      (setf (sdl:frame-rate) 0)

      (let ((color (sdl:color))
	    (template (sdl:rectangle :w 1 :h 1)))
	
	(sdl:with-events ()
	  (:quit-event () t)
	  (:idle ()
		 (sdl:fill-surface (sdl:set-color-* color :r (random 255) :g (random 255) :b (random 255))
				   :template (sdl:set-rectangle-* template :x (random width) :y (random height))
				   :update-p t
				   :surface sdl:*default-display*)))))))

