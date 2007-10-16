
(in-package #:lispbuilder-sdl)


(defun (setf frame-rate) (rate)
  (setf (sdl-base::target-frame-rate sdl-base::*default-fpsmanager*) rate))

(defun frame-rate ()
  (sdl-base::target-frame-rate sdl-base::*default-fpsmanager*))

(defun average-fps ()
  (sdl-base::calculate-average-fps sdl-base::*default-fpsmanager*))