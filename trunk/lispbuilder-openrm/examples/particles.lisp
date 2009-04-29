
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

(in-package #:rm-examples)

;;; ----------------------------------------------------------------------
;;;  Parameters of the demo.
;;; ----------------------------------------------------------------------
(defparameter *screen-width* 800)
(defparameter *screen-height* 600)
(defparameter *particle-speed-min* 0.1)
(defparameter *particle-speed-max* 0.9)
(defparameter *particle-count* 50)

(defun every-n-frames (max)
  (let ((count 0))
    #'(lambda ()
	(if (eql 0 (mod (incf count 1) max))
	    (setf count 0)
	    nil))))

(defparameter *frame-test* (every-n-frames 100))

;;; ----------------------------------------------------------------------
;;;  Global variables.
;;; ----------------------------------------------------------------------
(defvar *particle-img* nil)
(defparameter *particles* nil)

(defparameter *ftime* 0.5)

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
;;;  Create a 2D vector.
;;; ----------------------------------------------------------------------

(defun make-vec2 (x y)
  (vector x y))

(defun vec2-x (vect)
  (svref vect 0))
(defun (setf vec2-x) (value vect)
  (setf (svref vect 0) value))

(defun vec2-y (vect)
  (svref vect 1))
(defun (setf vec2-y) (value vect)
  (setf (svref vect 1) value))

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
(defstruct particle
  pos velocity obj)

;;; ----------------------------------------------------------------------
;;;  Initialize a particle, position it to the center of the screen, 
;;;  and give it a random velocity.
;;; ----------------------------------------------------------------------
(defun init-particle (p)
  (let ((vel (make-vec2 (rand-range -1.0 1.0) (rand-range -1.0 1.0))))
    (vec2-normalize vel)
    (let ((sp (rand-range *particle-speed-min* *particle-speed-max*)))
      (setf (vec2-x vel) (* (vec2-x vel) sp)
	    (vec2-y vel) (* (vec2-y vel) sp))
      (setf (rm::xy/z (particle-obj p)) #(0.0 0.0 -1.0)
            (particle-velocity p) vel)))
  p)

;;; ----------------------------------------------------------------------
;;;  Update the position of a particle.
;;; ----------------------------------------------------------------------
(defun update-particle (p ftime)
  (let ((vel (particle-velocity p)))
    (setf (rm::xy/z+ (particle-obj p)) (vector (* (vec2-x vel) ftime) (* (vec2-y vel) ftime) 0.0)))
  (let* ((pos (rm::xy/z (particle-obj p)))
         (x (rm::x pos))
         (y (rm::y pos)))
    (when (or (< x -1.0 ;; (- (- (/ *screen-width* 2)) (particle-width p))
		 )
              (< y -1.0 ;; (- (- (/ *screen-height* 2)) (particle-height p))
		 )
              (> x 1.0 ;; (/ *screen-width* 2)
		 )
              (> y 1.0 ;; (/ *screen-height* 2)
		 ))
      ;; The particle is out of screen, re-init it
      (init-particle p)))
  p)

;;; ----------------------------------------------------------------------
;;;  Add and remove particles.
;;; ----------------------------------------------------------------------
(defun create-particles (number)
  (let ((particle.bmp (sdl:create-path "particle.bmp" sdl:*default-image-path*)))
    (incf *particle-count* number)
    (loop repeat number
          collect (init-particle (make-particle
                                  :obj (rm::new-sprite :p-xy/z #(10.0 10.0 48.0)
                                                       :images (rm::load-image particle.bmp)
                                                       :dims :rm-renderpass-3d))))))
(defun remove-particles ()
  (when *particles*
    (dotimes (i *increment*)
      (pop *particles*))
    (if (< (decf *particle-count* *increment*) 0)
	(setf *particle-count* 0))))

(defclass pipe (rm::sdl-pipe)()
  (:default-initargs
   :target :RM-PIPE-NOPLATFORM
   :processing-mode :RM-PIPE-MULTISTAGE
   :opaque-3d t
   :transparent-3d t
   :opaque-2d t
   :display-list nil
   :swap-buffers nil))

(defclass particles-window (rm::sdl-window) ()
  (:default-initargs
   :width *screen-width*
   :height *screen-height*
   :name "particles-window"
   :pipe (make-instance 'pipe)
   :title-caption "Particles example"
   :icon-caption "Particles example"))

(defclass particles-scene (rm::scene)()
  (:default-initargs
   :name "particles-scene"
   :dims :rm-renderpass-3D
   :opacity :rm-renderpass-opaque
   :viewport t
   :background-color #(0.0 0.0 0.0 1.0)
   :camera (make-instance 'rm::camera-3d)
   :compute-view-from-geometry nil))

(defclass particles-node (rm::node) ()
  (:default-initargs
   :name "particles-node"))

;;; ----------------------------------------------------------------------
;;;  'Main' function.
;;; ----------------------------------------------------------------------
(defun particles ()
  (make-instance 'particles-window)
  (make-instance 'particles-scene :window (rm::default-window))
    
  ;; Create the particles
  (setf *particles* (create-particles *particle-count*))
  (dolist (particle *particles*)
    (rm::add-to-node (rm::default-scene (rm::default-window)) (particle-obj particle)))
  (setf (sdl:frame-rate) nil
        (rm::frame-rate (rm::default-window)) nil)
  (rm::process-events)
  (rm::clean-up))

(defmethod rm::on-idle ((window particles-window))
  ;; Update particles
  (dolist (p *particles*)
    (update-particle p *ftime*))
  ;; Update timescale
  (setf *ftime* (sdl:time-scale))
  (when (funcall *frame-test*)
    (rm::fformat "Frame Rate: ~A~%" (rm::cast float (sdl:average-fps)))))

(defun particles ()
  (make-instance 'particles-window)
  (make-instance 'particles-scene :window (rm::default-window))
    
  ;; Create the particles
  (setf *particles* (create-particles *particle-count*))
  (dolist (particle *particles*)
    (rm::add-to-node (rm::default-scene (rm::default-window)) (particle-obj particle)))
  (setf (sdl:frame-rate) nil)
  
  (sdl:with-events ()
    ;; A key was pressed
    (:key-down-event (:key k)
     (cond
      ;; Escape
      ((sdl:key= k :sdl-key-escape)
       (sdl:push-quit-event))))
    ;; Application is quitting
    (:quit-event () t)
    ;; Idle work
    (:idle ()
     ;; Update particles
     (dolist (p *particles*)
       (update-particle p *ftime*))
     ;; Update timescale
     (setf *ftime* (sdl:time-scale))
     (when (funcall *frame-test*)
       (rm::fformat "Frame Rate: ~A~%" (rm::cast float (sdl:average-fps))))
     ;; Flip back/front buffers
     (rm::render (rm::default-window))))
  (rm::clean-up))

