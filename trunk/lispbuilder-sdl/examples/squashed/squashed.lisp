;;;; (C) 2006 Jonathan Heusser, minor changes by Frank Buss

(in-package #:lispbuilder-sdl-examples)
		     
(defun show-score (score)
  (sdl-cffi::sdl-wm-set-caption (format nil "Squashed - Score: ~a" score) "Squashed"))

(defun squashed ()
  "Squashed: main entry function"
  (sdl:with-init ()
    (sdl:window 640 480)
    (setf (sdl-base::frame-rate) 30)
    (let ((bug (sdl:convert-surface :surface (sdl:load-image "squashed/bug.bmp" *bmp-path* :key-color (sdl:color :r 255 :g 255 :b 255))
				    :free-p t))
	  (racket (sdl:convert-surface :surface (sdl:load-image "squashed/racket.bmp" *bmp-path* :key-color (sdl:color :r 255 :g 255 :b 255))
				       :free-p t))
	  (blood (sdl:convert-surface :surface (sdl:load-image "squashed/blood.bmp" *bmp-path* :key-color (sdl:color :r 255 :g 255 :b 255))
				      :free-p t))
	  (squash (sdl:convert-surface :surface (sdl:load-image "squashed/squash.bmp" *bmp-path* :key-color (sdl:color :r 255 :g 255 :b 255))
				       :free-p t))
	  (levelticks 1000)
	  (last-squash-tick 0)
	  (lasttick 0)
	  (score 0)
	  (bug? t) (racket? t) (blood? nil) (squash? nil) (squashed? nil))

      (sdl:set-position-* bug :x (random 640) :y (random 480))
      (sdl:set-position-* racket :x 100 :y 100)
      (sdl:set-position-* squash :x 0 :y 0)
      
      (show-score score)
      (sdl-base::display-cursor nil)
      (sdl:with-events ()
		       (:quit-event () t)
		       (:mouse-motion-event (:x x :y y)
					    (sdl:set-position-* racket :x x :y y))
		       (:mouse-button-down-event (:x x :y y)
						 ;; check if squashed
						 (when (sdl:within-range (sdl:get-point racket)
									 (sdl:get-point bug)
									 17)
						   (setf squashed? t
							 bug? nil
							 blood? t
							 squash? t)
						   (sdl:set-position-* blood :x x :y y)
						   (setf last-squash-tick (sdl-cffi::SDL-Get-Ticks))
						   (show-score (incf score))
					; increase the bug jumping speed
						   (when (> levelticks 200)
						     (decf levelticks 100))))
		       (:idle ()
			      (sdl:with-surface (disp sdl:*default-display*)
				;; fill the background white
				(sdl:clear-display (sdl:color :r 255 :g 255 :b 255))
				;; draw images
				(if squashed?
				    (if (> (- (sdl-cffi::SDL-Get-Ticks) last-squash-tick) 700)
					(setf squashed? nil
					      squash? nil
					      blood? nil
					      bug? t))
				    (when (> (sdl-cffi::SDL-Get-Ticks) (+ lasttick levelticks))
				      (setf lasttick (sdl-cffi::SDL-Get-Ticks))
				      (sdl:set-position-* bug :x (random 640) :y (random 480))))
				(when squash? (sdl:draw-surface squash))
				(when bug? (sdl:draw-surface bug))
				(when blood? (sdl:draw-surface blood))
				(when racket? (sdl:draw-surface racket))
				(sdl:update-display)))))))
