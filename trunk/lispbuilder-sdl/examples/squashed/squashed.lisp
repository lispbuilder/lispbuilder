;;;; (C) 2006 Jonathan Heusser, minor changes by Frank Buss

(defpackage :squashed
  (:use #:cl #:lispbuilder-sdl)
  (:export #:main))

(in-package :squashed)

;; change it
(defparameter *imagepath* "/lispbuilder-sdl/examples/squashed")

(defun load-image (filename)
  "load an image and sets white to transparent"
  (let ((loaded-image (sdl::load-bmp (concatenate 'string
                                                  *imagepath*
                                                  filename))))
    (when loaded-image
      (let ((optimized-image (sdl::SDL_DisplayFormat loaded-image)))
        (sdl::SDL_FreeSurface loaded-image)
	(when optimized-image
	  (sdl::set-colorkey optimized-image #xff #xff #xff)
	  optimized-image)))))

(defun draw-image (screen image point)
  (let ((w (sdl::surf-w image))
        (h (sdl::surf-h image)))
    (sdl::apply-surface-free image
                             screen
                             :source-rect (sdl::rectangle 0 0 w h)
                             :destination-x (point-x point)
                             :destination-y (point-y point))))

(defstruct point x y)

(defun in-range (p1 p2 distance)
  "return true, if the distance between p1 and p2 is not more than 'distance'"
  (<= (+ (expt (- (point-x p1) (point-x p2)) 2)
         (expt (- (point-y p1) (point-y p2)) 2))
      (expt distance 2)))

(defun make-random-point ()
  (make-point :x (random 630) :y (random 470)))

(defun show-score (score)
  (sdl::SDL_WM_SetCaption (format nil "Squashed - Score: ~a" score) "Squashed"))

(defun main ()
  "main entry function"
  (sdl::with-init ()
    (let ((screen (sdl::set-window 640 480))
          (bug (load-image "/bug.bmp"))
          (racket (load-image "/racket.bmp"))
          (blood (load-image "/blood.bmp"))
          (squash (load-image "/squash.bmp"))
          (bug-point (make-random-point))
          (racket-point (make-point :x 100 :y 100))
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
         (setf (point-x racket-point) x
               (point-y racket-point) y))
        (:mousebuttondown (button state x y)
         ;; check if squashed
         (when (in-range racket-point bug-point 17)
           (setf squashed-point (make-point :x x :y y)
                 last-squash-tick (sdl::SDL_GetTicks))
           (show-score (incf score))
           :; increase the bug jumping speed
           (when (> levelticks 200)
             (decf levelticks 100))))
        (:idle
         ;; fill the background white
         (sdl::fill-surface screen #xFF #xFF #xFF)
         ;; draw images
         (when squashed-point
           (draw-image screen blood squashed-point))
         (when (> (sdl::SDL_GetTicks) (+ lasttick levelticks))
           (setf lasttick (sdl::SDL_GetTicks)
                 bug-point (make-random-point)))
         (if (< (- (sdl::SDL_GetTicks) last-squash-tick) 500)
             (draw-image screen squash (make-point :x 0 :y 0))
           (draw-image screen bug bug-point))
         (draw-image screen racket racket-point)
         ;; blit to screen
         (sdl::update-surface screen))))))
