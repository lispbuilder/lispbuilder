
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
  (sdl-base::time-scale-tscale (sdl-base::time-scale fpsmanager)))

(defun average-fps (&optional (fpsmanager sdl-base::*default-fpsmanager*))
  "Returns the average frame rate of the event loop calculated over a sliding window of 100 frames."
  (sdl-base::average-fps (sdl-base::fps-average fpsmanager)))

