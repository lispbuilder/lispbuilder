;;;; (C) 2006 Jonathan Heusser, minor changes by Frank Buss

(in-package #:lispbuilder-sdl-examples)

(defvar *image-path* (or *load-truename* *default-pathname-defaults*))

(defun load-image (filename)
  "load an image and sets white to transparent"
  (let ((loaded-image (sdl::load-bmp (namestring (merge-pathnames filename *image-path*)))))
    (when loaded-image
      (sdl::convert-surface-to-display-format loaded-image :key-color (sdl::color #xff #xff #xff)))))

(defun draw-image (screen image point)
  (let ((w (sdl::surf-w image))
        (h (sdl::surf-h image)))
    (sdl::blit-surface image
		       screen
		       :src-rect (sdl:rectangle 0 0 w h)
		       :dst-rect (sdl::point (sdl::point-x point) (sdl::point-y point)))))
			     
(defun in-range (p1 p2 distance)
  "return true, if the distance between p1 and p2 is not more than 'distance'"
  (<= (+ (expt (- (sdl::point-x p1) (sdl::point-x p2)) 2)
         (expt (- (sdl::point-y p1) (sdl::point-y p2)) 2))
      (expt distance 2)))

(defun make-random-point ()
  (sdl::point (random 630) (random 470)))

(defun show-score (score)
  (sdl::WM_SetCaption (format nil "Squashed - Score: ~a" score) "Squashed"))

(defun squashed ()
  "Squashed: main entry function"
  (sdl::with-init ()
    (sdl:with-display (640 480 :surface-name screen)
      (let ((bug (load-image "bug.bmp"))
	    (racket (load-image "racket.bmp"))
	    (blood (load-image "blood.bmp"))
	    (squash (load-image "squash.bmp"))
	    (bug-point (make-random-point))
	    (racket-point (sdl::point 100 100))
	    (squashed-point nil)
	    (levelticks 1000)
	    (last-squash-tick 0)
	    (lasttick 0)
	    (score 0))
	(show-score score)
	(sdl::SDL_ShowCursor 0)
	(sdl::with-events
	  (:quit t)
	  (:mousemotion (state x y xrel yrel)
			(setf (sdl::point-x racket-point) x
			      (sdl::point-y racket-point) y))
	  (:mousebuttondown (button state x y)
			    ;; check if squashed
			    (when (in-range racket-point bug-point 17)
			      (setf squashed-point (sdl::point x y)
				    last-squash-tick (sdl::SDL_GetTicks))
			      (show-score (incf score))
			      :	      ; increase the bug jumping speed
			      (when (> levelticks 200)
				(decf levelticks 100))))
	  (:idle
	   ;; fill the background white
	   (sdl::fill-surface screen #(255 255 255))
	   ;; draw images
	   (when squashed-point
	     (draw-image screen blood squashed-point))
	   (when (> (sdl::SDL_GetTicks) (+ lasttick levelticks))
	     (setf lasttick (sdl::SDL_GetTicks)
		   bug-point (make-random-point)))
	   (if (< (- (sdl::SDL_GetTicks) last-squash-tick) 500)
	       (draw-image screen squash (sdl::point 0 0))
	       (draw-image screen bug bug-point))
	   (draw-image screen racket racket-point)
	   ;; blit to screen
	   (sdl::update-screen screen)))))))
