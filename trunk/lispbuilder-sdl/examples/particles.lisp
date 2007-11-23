
;;;; ---------------------------------------------------------------------
;;;;  Particle Demo: a simple SDL demo using common lisp.
;;;;  Copyright (C) 2007  Nicolas Martyanoff <khaelin@gmail.com>
;;;; 
;;;;  This program is free software: you can redistribute it and/or modify
;;;;  it under the terms of the GNU General Public License as published by
;;;;  the Free Software Foundation, either version 3 of the License, or
;;;;  (at your option) any later version.
;;;; 
;;;;  This program is distributed in the hope that it will be useful,
;;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;  GNU General Public License for more details.
;;;; 
;;;;  You should have received a copy of the GNU General Public License
;;;;  along with this program. If not, see <http://www.gnu.org/licenses/>.
;;;; ---------------------------------------------------------------------

(in-package #:sdl-examples)

;;; ----------------------------------------------------------------------
;;;  Parameters of the demo.
;;; ----------------------------------------------------------------------
(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *particle-speed-min* 120.0)
(defparameter *particle-speed-max* 200.0)
(defparameter *particle-count* 200)

;;; ----------------------------------------------------------------------
;;;  Global variables.
;;; ----------------------------------------------------------------------
(defvar *particle-img* nil)
(defvar *particles* nil)

(defvar *ftime* 0.1)

(defvar *font-large* nil)
(defvar *font-small* nil)

(defvar *increment* 10)

;;; ----------------------------------------------------------------------
;;;  Functions for rendering the current FPS to the screen.
;;; ----------------------------------------------------------------------
(defun draw-cached-string (string x y font surface render-p)
  ;; Create a new FPS string when render-p is T
  (when render-p
    (sdl:render-string-solid string :color sdl:*white* :font font :cache t :free t))
  ;; Draw the string each frame
  (sdl:draw-font-at-* x y :font font :surface surface))

;;; ----------------------------------------------------------------------
;;;  Generate a random number in the range [a, b].
;;; ----------------------------------------------------------------------
(defun rand-range (a b)
  (+ (* (random 1.0) (- b a)) a))

;;; ----------------------------------------------------------------------
;;;  A 2D vector.
;;; ----------------------------------------------------------------------
(defclass vec2 ()
  ((x :accessor vec2-x :initform 0.0 :initarg :x)
   (y :accessor vec2-y :initform 0.0 :initarg :y)))

;;; ----------------------------------------------------------------------
;;;  Create a 2D vector.
;;; ----------------------------------------------------------------------
(defun make-vec2 (x y)
  (make-instance 'vec2 :x x :y y))

;;; ----------------------------------------------------------------------
;;;  Normalize a 2D vector.
;;; ----------------------------------------------------------------------
(defun vec2-normalize (v)
  (let ((n (sqrt (+ (* (vec2-x v) (vec2-x v))
                    (* (vec2-y v) (vec2-y v))))))
    (when (not (zerop n))
      (setf (vec2-x v) (/ (vec2-x v) n))
   (setf (vec2-y v) (/ (vec2-y v) n)))))

;;; ----------------------------------------------------------------------
;;;  A particule.
;;; ----------------------------------------------------------------------
(defclass particle ()
  ((pos :accessor particle-pos :initarg :pos)
   (velocity :accessor particle-velocity :initarg :velocity)))

;;; ----------------------------------------------------------------------
;;;  Create a particle.
;;; ----------------------------------------------------------------------
(defun make-particle (pos vel)
  (make-instance 'particle :pos pos :velocity vel))

;;; ----------------------------------------------------------------------
;;;  Initialize a particle, position it to the center of the screen, 
;;;  and give it a random velocity.
;;; ----------------------------------------------------------------------
(defun init-particle (p)
  (let ((hx (/ *screen-width* 2)) (hy (/ *screen-height* 2)))
    (let ((vel (make-vec2 (rand-range -1.0 1.0) (rand-range -1.0 1.0))))
      (vec2-normalize vel)
      (let ((sp (rand-range *particle-speed-min* *particle-speed-max*)))
        (setf (vec2-x vel) (* (vec2-x vel) sp))
        (setf (vec2-y vel) (* (vec2-y vel) sp))
        (setf (particle-pos p) (make-vec2 hx hy))
        (setf (particle-velocity p) vel))))
  p)

;;; ----------------------------------------------------------------------
;;;  Update the position of a particle.
;;; ----------------------------------------------------------------------
(defun update-particle (p ftime)
  (let ((vel (particle-velocity p)))
    (incf (vec2-x (particle-pos p)) (* (vec2-x vel) ftime))
    (incf (vec2-y (particle-pos p)) (* (vec2-y vel) ftime)))
  (let ((pos (particle-pos p)))
    (when (or (< (vec2-x pos) (- 0 (sdl:width *particle-img*)))
              (< (vec2-y pos) (- 0 (sdl:height *particle-img*)))
              (> (vec2-x pos) *screen-width*)
              (> (vec2-y pos) *screen-height*))
      ;; The particle is out of screen, re-init it
      (init-particle p)))
  p)

;;; ----------------------------------------------------------------------
;;;  Add and remove particles.
;;; ----------------------------------------------------------------------
(defun add-particles ()
  (dotimes (i *increment*)
    (push (init-particle (make-instance 'particle))
	  *particles*))
  (incf *particle-count* *increment*))

(defun remove-particles ()
  (when *particles*
    (dotimes (i *increment*)
      (pop *particles*))
    (if (< (decf *particle-count* *increment*) 0)
	(setf *particle-count* 0))))

;;; ----------------------------------------------------------------------
;;;  'Main' function.
;;; ----------------------------------------------------------------------
(defun particles ()
  (let ((previous-average 0))

    (sdl:with-init ()
      ;; Create a window
      (unless (sdl:window *screen-width* *screen-height*
			  :title-caption "Particles Demo"
			  :icon-caption "Particles Demo"
			  :flags '(sdl:sdl-doublebuf sdl:sdl-hw-surface))
	(error "~&Unable to create a SDL window~%"))

      ;; Fix the framerate
      (setf (sdl:frame-rate) 0)

      ;; Enable key repeat.
      (sdl-cffi::sdl-enable-key-repeat sdl-cffi::sdl-default-repeat-delay
				       sdl-cffi::sdl-default-repeat-interval)

      ;; Load images
      (setf *particle-img* (sdl:load-image (sdl:create-path "particle.bmp"
							    *bmp-path*)))
      (unless *particle-img*
	(error "~&Unable to load ~A.~%" (sdl:create-path "particle.bmp" *bmp-path*)))

      ;; Load the bitmap fonts
      (setf *font-large* (sdl:initialise-font sdl:*font-8x13*))
      (setf *font-small* (sdl:initialise-font sdl:*font-7x14*))

      ;; Render the initial text.
      ;; We cache the rendered string in the FONT object.
      ;; Drawing the font to the display then involves a fast blit.
      (draw-cached-string (format nil "Particles Demo: {ESC} = Exit, {P} Add 10 Particles, {L} Remove 10 Particles.")
			  5 5 *font-large* sdl:*default-display* t)
      (draw-cached-string (format nil "Particles: ~d, Framerate: Calculating...." *particle-count*)
			  5 35 *font-small* sdl:*default-display* t)

      ;; Create the particles
      (setf *particles* nil)
      (dotimes (i *particle-count*)
	(push (init-particle (make-instance 'particle))
	      *particles*))

      ;; Event loop
      (sdl:with-events ()
	;; A key was pressed
	(:key-down-event (:key k)
			 (cond
			   ;; Escape
			   ((sdl:key= k :sdl-key-escape)
			    (sdl:push-quit-event))
			   ;; Add particle
			   ((sdl:key= k :sdl-key-p)
			    (add-particles))
			   ;; Remove particle
			   ((sdl:key= k :sdl-key-l)
			    (remove-particles))))

	;; Application is quitting
	(:quit-event () t)

	;; Application has to be redrawn
	(:video-expose-event () (sdl:update-display))

	;; Idle work
	(:idle
	 ;; Clear screen
	 (sdl:clear-display sdl:*black*)

	 ;; Draw the particles
	 (dolist (p *particles*)
	   (update-particle p *ftime*)
	   (let ((pos (particle-pos p)))
	     (sdl:draw-surface-at-* *particle-img*
				    (round (vec2-x pos)) (round (vec2-y pos)))))

	 ;; Display text.
	 (draw-cached-string "" 5 5 *font-large* sdl:*default-display* nil)
	 (if (equal (sdl:average-fps) previous-average)
	     (draw-cached-string "" 5 35 *font-small* sdl:*default-display* nil)
	     (draw-cached-string (format nil "Particles: ~d, Framerate: ~f" *particle-count* (sdl:average-fps))
				 5 35 *font-small* sdl:*default-display* t))     
	 
	 (setf previous-average (sdl:average-fps))
	
	 ;; Flip back/front buffers
	 (sdl:update-display)

	 ;; Update the timescale
	 (setf *ftime* (sdl:time-scale)))))))

