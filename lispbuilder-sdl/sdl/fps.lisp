
(in-package #:lispbuilder-sdl)


(defun (setf frame-rate) (rate)
  (setf (sdl-base::target-frame-rate sdl-base::*default-fpsmanager*) rate))

(defun frame-rate ()
  (sdl-base::target-frame-rate sdl-base::*default-fpsmanager*))

(defun time-scale ()
  (sdl-base::time-scale-tscale sdl-base::*time-scale*))

(defun average-fps ()
  (sdl-base::average-fps sdl-base::*fps-average*))

