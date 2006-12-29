;;;; (C) 2006 Jonathan Heusser, minor changes by Frank Buss

(in-package #:lispbuilder-sdl-examples)
		     
(defun show-score (score)
  (sdl-cffi::sdl-wm-set-caption (format nil "Squashed - Score: ~a" score) "Squashed"))

(defun squashed ()
  "Squashed: main entry function"
  (sdl:with-init ()
    (sdl::window 640 480)
    (let ((bug-point (sdl::point :x (random 640)
				 :y (random 480)))
	  (racket-point (sdl::point :x 100 :y 100))
	  (squashed-point nil)
	  (levelticks 1000)
	  (last-squash-tick 0)
	  (lasttick 0)
	  (score 0)
	  (bug (sdl::load-image "squashed/bug.bmp" *bmp-path* :key-color (sdl::color :r 255 :g 255 :b 255)))
	  (racket (sdl::load-image "squashed/racket.bmp" *bmp-path* :key-color (sdl::color :r 255 :g 255 :b 255)))
	  (blood (sdl::load-image "squashed/blood.bmp" *bmp-path* :key-color (sdl::color :r 255 :g 255 :b 255)))
	  (squash (sdl::load-image "squashed/squash.bmp" *bmp-path* :key-color (sdl::color :r 255 :g 255 :b 255))))
      
      (show-score score)
      (sdl-base::display-cursor nil)
      (sdl:with-events ()
	(:quit-event () t)
	(:mouse-motion-event (:x x :y y)
			     (setf (sdl::x racket-point) x
				   (sdl::y racket-point) y))
	(:mouse-button-down (:x x :y y)
			    ;; check if squashed
			    (when (sdl::points-in-range racket-point bug-point 17)
			      (setf squashed-point (sdl::point :x x :y y)
				    last-squash-tick (sdl-cffi::SDL-Get-Ticks))
			      (show-score (incf score))
					; increase the bug jumping speed
			      (when (> levelticks 200)
				(decf levelticks 100))))
	(:idle ()
	       ;; fill the background white
	       (sdl::clear-display (sdl::color :r 255 :g 255 :b 255))
	       ;; draw images
	       (when squashed-point
		   (sdl::draw-image blood
				    :surface sdl::*default-display*
				    :position squashed-point))
	       (when (> (sdl-cffi::SDL-Get-Ticks) (+ lasttick levelticks))
		 (setf lasttick (sdl-cffi::SDL-Get-Ticks)
		       bug-point (sdl::point :x (random 640)
					     :y (random 480)))
		 (if (< (- (sdl-cffi::SDL-Get-Ticks) last-squash-tick) 500)
		     (sdl::draw-image squash
				      :surface sdl::*default-display*
				      :position (sdl::point :x 0 :y 0))
		     (sdl::draw-image bug
				     :surface sdl::*default-display*
				     :position bug-point))
		 (sdl::draw-image racket
				  :surface sdl::*default-display*
				  :position racket-point)
		 (sdl::update-display)))))))
