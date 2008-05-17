

(in-package #:sdl-examples)

;;; From http://paste.lisp.org/display/60655
;;; Based on http://billmill.org/static/viewji/viewji.html which is based in
;;; turn on http://nodebox.net/code/index.php/Superfolia_|_root_source_code

;;;; roots.lisp

(use-package 'cl 'vecto)

(defun radians (degrees)
  ;; Using PI causes type promotion to DOUBLE-FLOAT, which SVG doesn't love.
  (/ (* degrees pi) 180.0))

(defun random-range (start end)
  (+ start (random (- end start))))

(defun rgb (r g b)
  (list r g b))

(defun set-fill-color (color alpha)
  (vecto:set-rgba-fill (first color) (second color) (third color) alpha))

(defun set-stroke-color (color alpha)
  (vecto:set-rgba-stroke (first color) (second color) (third color) alpha))

(defun root (canvas x y &optional (angle 0) (depth 5) (alpha 1.0) (decay 0.005))
  (let ((w (* depth 6.0)))
    (dotimes (i (* depth (random-range 10 20)))
      (let* ((v (/ depth 5.0))
             (color (rgb  (- 0.8 (* v 0.25))
                          0.8
                          (- 0.8 v))))
        (setf alpha (max 0.0 (- alpha (* i decay))))
	;; CL will start to use exponential notation when alpha gets
	;; very small, and SVG hates this.
        (when (> alpha 0.00001)
          (setf angle (+ angle (random-range -60 60)))
          (let ((dx (+ x (* (cos (radians angle)) w)))
                (dy (+ y (* (sin (radians angle)) w))))
            (vecto:set-rgba-stroke (first color)
				   (second color)
				   (third color)
				   alpha)
            (set-fill-color color (* alpha 0.6))
            (set-stroke-color color alpha)
            (vecto:set-line-width (* depth 0.5))
            (vecto:set-line-cap :round)
            (vecto:with-graphics-state
              (vecto:set-rgba-fill 0 0 0 (* alpha 0.6))
              (vecto:centered-circle-path (+ x depth 1)
					  (1- (+ y depth))
					  (/ w 3))
              (vecto:fill-path))
            ;; line segment to next position:
            (vecto:move-to x y)
            (vecto:line-to dx dy)
            (vecto:stroke)
            (vecto:centered-circle-path x y (/ w 4))
            (vecto:fill-and-stroke)
            (when (and (> depth 0) (> (random 1.0) 0.85))
              (root canvas x y (+ angle (random-range -60 60)) (1- depth) alpha))
            (setf x dx
                  y dy)))))
    (when (and (> depth 0) (> (random 1.0) 0.7))
      (root canvas x y angle (1- depth) alpha))))

(defun roots (&key (depth 7))
  (sdl:with-init ()
    (sdl:window 400 400 :title-caption "Roots: Please be patient - this takes a while." :icon-caption "Roots: Please be patient - this takes a while.")
    (vecto:with-canvas (:width 400 :height 400)
      (vecto:set-rgb-fill 0 0 0)
      (vecto:clear-canvas)
      (root nil 350 350 (random 360) depth)
      (sdl:vecto->surface vecto::*graphics-state* sdl:*default-display* 0 0)
      (sdl:update-display)
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event (sdl:update-display))
	(:key-down-event (:key key)
			 (when (sdl:key= key :sdl-key-escape)
			   (sdl:push-quit-event)))))))