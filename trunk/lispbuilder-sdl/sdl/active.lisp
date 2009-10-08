
(in-package #:lispbuilder-sdl)

(defun mouse-gain-focus-p (gain &optional (state (sdl-cffi::sdl-get-app-state)))
  (when (mouse-focus-p state)
    (= 1 gain)))

(defun input-gain-focus-p (gain &optional (state (sdl-cffi::sdl-get-app-state)))
  (when (input-focus-p state)
    (= 1 gain)))

(defun active-gain-p (gain &optional (state (sdl-cffi::sdl-get-app-state)))
  (when (active-p state)
    (= 1 gain)))

(defun mouse-focus-p (&optional (state (sdl-cffi::sdl-get-app-state)))
  (/= 0 (logand sdl-cffi::APP-MOUSE-FOCUS state)))

(defun input-focus-p (&optional (state (sdl-cffi::sdl-get-app-state)))
  (/= 0 (logand sdl-cffi::APP-INPUT-FOCUS state)))

(defun active-p (&optional (state (sdl-cffi::sdl-get-app-state)))
  (/= 0 (logand sdl-cffi::APP-ACTIVE state)))
