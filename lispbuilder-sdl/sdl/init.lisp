
(in-package #:lispbuilder-sdl)

;;;; Functions


;; (defmacro with-init (renderer &body body)
;;   (let ((rend (gensym "rend-")))
;;     `(let ((,rend ,(if renderer
;; 		       `(make-instance ,@renderer)
;; 		       `(make-instance 'sdl))))
;;        (when (hook-initialize-engine ,rend)
;; 	 (unwind-protect
;; 	      (progn
;; ;		(init-timestep)
;; 		,@body)
;; 	   (hook-finalize-engine ,rend))))))

(defmacro with-init (flags &body body)
  (declare (ignore flags))
  `(block nil
     (unwind-protect
	  (when (sdl-base::init-sdl :flags nil)
	    (init-sub-systems *initialize-on-startup*)
	    ,@body)
       (quit-sub-systems *quit-on-exit*)
       (when *sdl-quit-on-exit*
	 (sdl-cffi::SDL-Quit)))))

(defun initialize-on-startup (&rest flags)
  (setf *initialize-on-startup* (apply #'logior flags)))

(defun quit-on-exit (&rest flags)
  (setf *quit-on-exit* (apply #'logior flags)))

(defun list-sub-systems (flag)
  (let ((subsystems nil))
    (if (= flag sdl-cffi::sdl-init-everything)
	(push (list 'sdl-cffi::sdl-init-everything
		    sdl-cffi::sdl-init-everything)
	      subsystems)
	(progn
	  (when (/= 0 (logand flag sdl-cffi::sdl-init-video))
	    (push (list 'sdl-cffi::sdl-init-video
			sdl-cffi::sdl-init-video)
		  subsystems))
	  (when (/= 0 (logand flag sdl-cffi::sdl-init-cdrom))
	    (push (list 'sdl-cffi::sdl-init-cdrom
			sdl-cffi::sdl-init-cdrom)
		  subsystems))
	  (when (/= 0 (logand flag sdl-cffi::sdl-init-audio))
	    (push (list 'sdl-cffi::sdl-init-audio
			sdl-cffi::sdl-init-audio)
		  subsystems))
	  (when (/= 0 (logand flag sdl-cffi::sdl-init-timer))
	    (push (list 'sdl-cffi::sdl-init-timer
			sdl-cffi::sdl-init-timer)
		  subsystems))
	  (when (/= 0 (logand flag sdl-cffi::sdl-init-joystick))
	    (push (list 'sdl-cffi::sdl-init-joystick
			sdl-cffi::sdl-init-joystick)
		  subsystems))
	  (when (/= 0 (logand flag sdl-cffi::sdl-init-eventthread))
	    (push (list 'sdl-cffi::sdl-init-eventthread
			sdl-cffi::sdl-init-eventthread)
		  subsystems))
	  (when (/= 0 (logand flag sdl-cffi::sdl-init-noparachute))
	    (push (list 'sdl-cffi::sdl-init-noparachute
			sdl-cffi::sdl-init-noparachute)
		  subsystems))))
    subsystems))

(defun return-sub-systems-of-status (flags status)
  (let ((subsystems (sdl-cffi::sdl-was-init sdl-cffi::sdl-init-everything))
	(to-initialize 0)
	(status-fn (if status #'/= #'=)))
    (mapc #'(lambda (subsystem)
	      (let ((value (second subsystem)))
		(when (funcall status-fn 0 (logand subsystems value))
		  (setf to-initialize (logior to-initialize
					      value)))))
	  (list-sub-systems flags))
    to-initialize))

(defun init-sub-systems (&optional (flags *initialize-on-startup*))
  (sdl-cffi::sdl-init-sub-system (return-sub-systems-of-status flags nil)))

(defun quit-sub-systems (&optional (flags *quit-on-exit*))
  (sdl-cffi::sdl-quit-sub-system (return-sub-systems-of-status flags t)))

(defun sdl-quit-on-exit ()
  *sdl-quit-on-exit*)
(defun set-sdl-quit-on-exit (val)
  (setf *sdl-quit-on-exit* val))
(defsetf sdl-quit-on-exit set-sdl-quit-on-exit)

(defun initialized-sub-systems-p ()
  (list-sub-systems (sdl-cffi::sdl-was-init sdl-cffi::sdl-init-everything)))
