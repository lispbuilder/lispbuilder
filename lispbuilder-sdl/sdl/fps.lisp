
(in-package #:lispbuilder-sdl)


(defun (setf frame-rate) (rate &optional (fpsmanager sdl-base::*default-fpsmanager*))
  (setf (sdl-base::target-frame-rate fpsmanager) rate))

;;;; (defun (setf frame-rate) (rate &optional fpsmanager)
;;;;   (setf (sdl-base::target-frame-rate sdl-base::*default-fpsmanager*) rate))

(defun frame-rate (&optional (fpsmanager sdl-base::*default-fpsmanager*))
    "Manage the target frame rate for the game loop.
`RATE` > `0` will lock the game loop to the specified frame rate, and
calculate the average frame rate over a number of frames.
`RATE` = `0` will unlock the frame rate, and calculate the average
frame rate over a number of frames.
`RATE` < `0` will unlock the frame rate. The average frane rate is 
not calculated.

See [WITH-EVENTS](#with-events), and [AVERAGE-FPS](#average-fps)."
  (sdl-base::target-frame-rate fpsmanager))

(defun time-scale (&optional (fpsmanager sdl-base::*default-fpsmanager*))
  (sdl-base::calculate-time-scale fpsmanager (sdl-base::delta-ticks fpsmanager)))

(defun average-fps (&optional (fpsmanager sdl-base::*default-fpsmanager*))
  "Returns the average frame rate of the event loop calculated over a sliding window
of 'n' frames."
  (sdl-base::average-fps fpsmanager))

(defun frame-time (&optional (fpsmanager sdl-base::*default-fpsmanager*))
  "Returns how long current frame time is"
  (sdl-base::calculate-time-scale fpsmanager (sdl-base::delta-ticks fpsmanager)))

(defun dt (&optional (fpsmanager sdl-base::*default-fpsmanager*))
  (sdl-base::dt fpsmanager))
(defun (setf dt) (value &optional (fpsmanager sdl-base::*default-fpsmanager*))
  (setf (sdl-base::dt fpsmanager) value))

(defun max-dt (&optional (fpsmanager sdl-base::*default-fpsmanager*))
  (sdl-base::max-dt fpsmanager))
(defun (setf max-dt) (value &optional (fpsmanager sdl-base::*default-fpsmanager*))
  (setf (sdl-base::max-dt fpsmanager) value))

(defun ticks (&optional (fpsmanager sdl-base::*default-fpsmanager*))
  (sdl-base::fps-ticks fpsmanager))

;(defun physics-hook-p (&optional (fpsmanager sdl-base::*default-fpsmanager*))
;  (sdl-base::ps-fn fpsmanager))
;(defun set-physics-hook (fn &optional (fpsmanager sdl-base::*default-fpsmanager*))
;  (setf (sdl-base::ps-fn fpsmanager) fn))
;(defsetf physics-hook-p set-physics-hook)

;;;; --------------------------
;;;; Lock timestep to Specified Rate
;;;; From http://www.gaffer.org/game-physics/fix-your-timestep/
(defmacro with-timestep (&body body)
  `(progn
     (incf (sdl-base::accumulator *default-fpsmanager*) (if (> (sdl-base::delta-ticks *default-fpsmanager*)
                                                               (sdl-base::max-dt *default-fpsmanager*))
                                                (sdl-base::max-dt *default-fpsmanager*)
                                                (sdl-base::delta-ticks *default-fpsmanager*)))
     (loop until (< (sdl-base::accumulator *default-fpsmanager*) (sdl-base::dt *default-fpsmanager*)) do
           (progn
             ,@body
             (incf (sdl-base::fps-ticks *default-fpsmanager*) (sdl-base::dt *default-fpsmanager*))
             (decf (sdl-base::accumulator *default-fpsmanager*) (sdl-base::dt *default-fpsmanager*))))))
