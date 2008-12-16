
(in-package #:sdl-examples)

(defun vecto-test ()
  (sdl:with-init ()
    (sdl:window 200 200 :title-caption "VECTO, Test" :icon-caption "VECTO, Test")
    (setf (sdl:frame-rate) 5)
    (sdl:fill-surface (sdl:color :r 255 :g 255 :b 255))

    (vecto:with-canvas (:width 100 :height 100)
      (vecto:set-line-width 5.0)
      ;; red stroke
      (vecto:set-rgb-stroke 1 0 0)
      (vecto:move-to 10 10)
      (vecto:line-to 90 90)
      (vecto:stroke)
      ;; green stroke
      (vecto:set-rgb-stroke 0 1 0)
      (vecto:move-to 10 90)
      (vecto:line-to 90 10)
      (vecto:stroke)
      ;; blue+alpha transform stroke
      (vecto:set-rgba-stroke 0 0 1 0.5)
      (flet ((elbow (radians)
	       (vecto:with-graphics-state
		 (vecto:translate 50 50)
		 (vecto:rotate radians)
		 (vecto:scale 0.25 0.25)
		 (vecto:move-to 0 0)
		 (vecto:curve-to 0 100
				 0 100
				 100 100)
		 (vecto:set-line-width 10.0)
		 (vecto:stroke))))
	(let* ((rotations 25)
	       (step (/ (* pi 2) rotations)))
	  (dotimes (i rotations)
	    (elbow (* i step)))))

      (sdl:vecto->surface vecto::*graphics-state* sdl:*default-display* 0 0)

      (sdl:with-events ()
	(:video-expose-event () (sdl:update-display))
	(:idle () (sdl:update-display))
	(:quit-event () t)))))

(defun radiant-lambda ()
  (sdl:with-init ()
    (sdl:window 200 200 :title-caption "VECTO, Radiant-lambda" :icon-caption "VECTO, Radiant-lambda")
    (setf (sdl:frame-rate) 5)
    (sdl:fill-surface (sdl:color :r 255 :g 255 :b 255))

    (vecto:with-canvas (:width 90 :height 90)
      (let ((font (vecto:get-font sdl:*default-ttf-font*))
	    (step (/ pi 7)))
	(vecto:set-font font 40)
	(vecto:translate 45 45)
      (vecto:draw-centered-string 0 -10 ;;#(#x3BB)
				  #(#\n))
      (vecto:set-rgb-stroke 1 0 0)
      (vecto:centered-circle-path 0 0 35)
      (vecto:stroke)
      (vecto:set-rgba-stroke 0 0 1.0 0.5)
      (vecto:set-line-width 4)
      (dotimes (i 14)
        (vecto:with-graphics-state
          (vecto:rotate (* i step))
          (vecto:move-to 30 0)
          (vecto:line-to 40 0)
          (vecto:stroke))))

      (sdl:vecto->surface vecto::*graphics-state* sdl:*default-display* 0 0)

      (sdl:with-events ()
	(:video-expose-event () (sdl:update-display))
	(:idle () (sdl:update-display))
	(:quit-event () t)))))

(defun feedlike-icon ()
  (sdl:with-init ()
    (sdl:window 200 200 :title-caption "VECTO, Feedlike-icon" :icon-caption "VECTO, Feedlike-icon")
    (setf (sdl:frame-rate) 5)
    (sdl:fill-surface (sdl:color :r 255 :g 255 :b 255))

    (vecto:with-canvas (:width 100 :height 100)
      (vecto:set-rgb-fill 1.0 0.65 0.3)
      (vecto:rounded-rectangle 0 0 100 100 10 10)
      (vecto:fill-path)
      (vecto:set-rgb-fill 1.0 1.0 1.0)
      (vecto:centered-circle-path 20 20 10)
      (vecto:fill-path)
      (flet ((quarter-circle (x y radius)
	       (let ((kappa (* vecto:+kappa+ radius)))
		 (vecto:move-to (+ x radius) y)
		 (vecto:curve-to (+ x radius) (+ y kappa)
				 (+ x kappa) (+ y radius)
				 x (+ y radius)))))
	(vecto:set-rgb-stroke 1.0 1.0 1.0)
	(vecto:set-line-width 15)
	(quarter-circle 20 20 30)
	(vecto:stroke)
	(quarter-circle 20 20 60)
	(vecto:stroke))

      (sdl:vecto->surface vecto::*graphics-state* sdl:*default-display* 0 0)

      (sdl:with-events ()
	(:video-expose-event () (sdl:update-display))
	(:idle () (sdl:update-display))
	(:quit-event () t)))))

(defun star-clipping ()
  (sdl:with-init ()
    (sdl:window 200 200 :title-caption "VECTO, Star-clipping" :icon-caption "VECTO, Star-clipping")
    (setf (sdl:frame-rate) 5)
    (sdl:fill-surface (sdl:color :r 255 :g 255 :b 255))

    (vecto:with-canvas (:width 200 :height 200)
      (let ((size 100)
	    (angle 0)
	    (step (* 2 (/ (* pi 2) 5))))
	(vecto:translate size size)
	(vecto:move-to 0 size)
	(dotimes (i 5)
	  (setf angle (+ angle step))
	  (vecto:line-to (* (sin angle) size)
			 (* (cos angle) size)))
	(vecto:even-odd-clip-path)
	(vecto:end-path-no-op)
	(flet ((circle (distance)
		 (vecto:set-rgba-fill distance 0 0
				      (- 1.0 distance))
		 (vecto:centered-circle-path 0 0 (* size distance))
		 (vecto:fill-path)))
	  (loop for i downfrom 1.0 by 0.05
	     repeat 20 do
	     (circle i))))

      (sdl:vecto->surface vecto::*graphics-state* sdl:*default-display* 0 0)

      (sdl:with-events ()
	(:video-expose-event () (sdl:update-display))
	(:idle () (sdl:update-display))
	(:quit-event () t)))))




