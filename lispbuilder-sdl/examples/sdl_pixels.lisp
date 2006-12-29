;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones
;;;; see COPYING for license

(in-package #:sdl-examples) 
  
(defun pixels-1 ()
  (sdl::with-init ()
    (sdl::window 640 480)
    (setf (sdl-base::frame-rate) 0)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
	     (sdl::draw-point (sdl::point :x (random 640) :y (random 480))
			      :color (sdl::color :r (random 255)
						 :g (random 255)
						 :b (random 255))
			      :surface sdl::*default-display*)
	     (sdl::update-display)))))

(defun pixels-2 ()
  (let ((width 640) (height 480))
    (sdl-base::with-init ()
      (sdl::window width height)
      (setf (sdl-base::frame-rate) 0)
      (sdl::with-events ()
	(:quit-event () t)
	(:idle ()
	       (sdl::with-locked-surface (disp sdl::*default-display*)
		 (sdl::write-pixel (random width) (random height)
				   (sdl::color :r (random 255)
					       :g (random 255)
					       :b (random 255))))
	       (sdl::update-display))))))

(defun pixels-3 ()
  (let ((width 640) (height 480))
    (sdl-base::with-init ()
      (sdl::window width height)
      (setf (sdl-base::frame-rate) 0)
      (sdl::with-events ()
	(:quit-event () t)
	(:idle ()
	       (sdl::with-rectangle (template (sdl::rectangle))
		 (setf template.x (random width)
		       template.y (random height)
		       template.width 1
		       template.height 1)
		   (sdl::fill-surface (sdl::color :r (random 255)
						  :g (random 255)
						  :b (random 255))
				      :template template
				      :update-p t
				      :dst sdl::*default-display*)))))))

(defun pixels-4 ()
  (let ((width 640) (height 480))
    (sdl-base::with-init ()
      (sdl::window width height)
      (setf (sdl-base::frame-rate) 0)
      (sdl::with-events ()
	(:quit-event () t)
	(:idle ()
	       (sdl::fill-surface (sdl::color :r (random 255)
					      :g (random 255)
					      :b (random 255))
				  :template (sdl::rectangle :x (random width) :y (random height)
							    :w 1 :h 1)
				  :update-p t
				  :dst sdl::*default-display*))))))

