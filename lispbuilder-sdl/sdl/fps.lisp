
(in-package #:lispbuilder-sdl)


;;;; --------------------------
;;;; Lock timestep to Specified Rate
;;;; From http://www.gaffer.org/game-physics/fix-your-timestep/
(let ((time 0) (d-time 10)
      (accumulator 0)
      (current-time 0) (new-time 0) (delta-time 0) (max-delta-time 25)
      (previous-state 0.0) (current-state 0.0)
      (alpha 0.0))
  (declare (type fixnum new-time delta-time current-time time d-time accumulator max-delta-time)
	   (type single-float alpha)
	   (type single-float previous-state current-state))

  (defun init-timestep ()
    (setf current-time (sdl-cffi::sdl-get-ticks)
	  time 0))
  
  (defun process-timestep (renderer)
    (setf new-time (sdl-cffi::sdl-get-ticks))
    (setf delta-time (- new-time current-time))
    (setf current-time new-time)
    (when (> delta-time max-delta-time)
      (setf delta-time max-delta-time))
    (incf accumulator delta-time)
    
    (do ()
	((<= accumulator d-time))
;;      (setf previous-state current-state)
      (hook-process-physics renderer current-state time d-time)
;;    (incf time d-time)
      (decf accumulator d-time))

;;     (setf alpha (/ accumulator d-time))
;;     (+ (* current-state alpha)
;;        (* previous-state (- 1.0 alpha)))
    ))



;;;; --------------------------
;;;; Lock fps to Specified Rate
(let ((frame-count 0) (rate 30) (rate-ticks (/ 1000.0 30.0)) (last-ticks 0)
      (fps-upper-limit 200) (fps-lower-limit 1)
      (current-ticks 0) (target-ticks 0.0))
  (declare (type fixnum frame-count rate last-ticks)
	   (type float rate-ticks)
	   (type fixnum fps-upper-limit fps-lower-limit current-ticks)
	   (type float target-ticks))
  (defun init-framerate-manager()
    (setf frame-count 0
	  rate 30
	  rate-ticks (the float (/ 1000.0 30.0))
	  last-ticks 0))
  (defun set-framerate (fps)
    (declare (type fixnum fps))
    (if (> fps 0)
        (if (and (>= fps fps-lower-limit) (<= fps fps-upper-limit))
            (progn
              (setf frame-count 0
		    rate fps
		    rate-ticks (the float (/ 1000.0 fps)))
              t)
	    nil)
	(setf rate fps)))
  (defsetf framerate set-framerate)
  (defun framerate ()
    rate)

  (defun ticks ()
    current-ticks)

  (defun framerate-delay ()
    (when (> rate 0)
      (setf current-ticks (sdl-cffi::sdl-get-ticks))
      (incf frame-count)
      (setf target-ticks (the float (+ (* frame-count rate-ticks)
				       last-ticks )))
      (if (<= current-ticks target-ticks)
	  (sdl-cffi::sdl-delay (round (- target-ticks current-ticks)))
	  (progn
	    (setf frame-count 0)
	    (setf last-ticks current-ticks))))))
;;;;
;;;; ------------------------
