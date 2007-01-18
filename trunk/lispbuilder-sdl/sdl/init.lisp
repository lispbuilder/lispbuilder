
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
  "Initializes the SDL library and SDL subsystems prior to executing the forms in BODY.
Upon exit will uninitialize the SDL library and subsystems only when SDL-QUIT-ON-EXIT returns true T.
Initializes the SDL library using INIT-SDL and INIT-SUB-SYSTEMS.
Uninitializes the SDL library using QUIT-SUB-SYSTEMS and QUIT-SDL."
  (declare (ignore flags))
  `(block nil
     (unwind-protect
	  (when (init-sdl)
	    (init-sub-systems)
	    ,@body)
       (quit-sub-systems)
       (quit-sdl))))

(defun initialize-on-startup (&rest flags)
  "Sets one or more SDL subsystems that shall be initialized in 
subsequent calls to INIT-SUB-SYSTEMS. Where FLAGS may be one or more of:

SDL-CFFI::SDL-INIT-EVERYTHING, SDL-CFFI::SDL-INIT-VIDEO, 
SDL-CFFI::SDL-INIT-CDROM, SDL-CFFI::SDL-INIT-AUDIO, 
SDL-CFFI::SDL-INIT-TIMER, SDL-CFFI::SDL-INIT-JOYSTICK,
SDL-CFFI::SDL-INIT-EVENTTHREAD, SDL-CFFI::SDL-INIT-NOPARACHUTE."
  (setf *initialize-on-startup* (apply #'logior flags)))

(defun quit-on-exit (&rest flags)
    "Sets one or more SDL subsystems that shall be uninitialized in 
subsequent calls to QUIT-SUB-SYSTEMS. Where FLAGS may be one or more of:

SDL-CFFI::SDL-INIT-EVERYTHING, SDL-CFFI::SDL-INIT-VIDEO, 
SDL-CFFI::SDL-INIT-CDROM, SDL-CFFI::SDL-INIT-AUDIO, 
SDL-CFFI::SDL-INIT-TIMER, SDL-CFFI::SDL-INIT-JOYSTICK,
SDL-CFFI::SDL-INIT-EVENTTHREAD, SDL-CFFI::SDL-INIT-NOPARACHUTE."
  (setf *quit-on-exit* (apply #'logior flags)))

(defun list-sub-systems (flag)
  "Returns a list of SDL subsystems that are specified in FLAG.
Where FLAG is an INTEGER bitmask containing the logior of the following:

SDL-CFFI::SDL-INIT-EVERYTHING, SDL-CFFI::SDL-INIT-VIDEO, 
SDL-CFFI::SDL-INIT-CDROM, SDL-CFFI::SDL-INIT-AUDIO, 
SDL-CFFI::SDL-INIT-TIMER, SDL-CFFI::SDL-INIT-JOYSTICK,
SDL-CFFI::SDL-INIT-EVENTTHREAD, SDL-CFFI::SDL-INIT-NOPARACHUTE"
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
  "Returns the status of the the specified SDL subsystems as an INTEGER bitmask. 

Where FLAG is an INTEGER bitmask containing the logior of the following:
SDL-CFFI::SDL-INIT-EVERYTHING, SDL-CFFI::SDL-INIT-VIDEO, 
SDL-CFFI::SDL-INIT-CDROM, SDL-CFFI::SDL-INIT-AUDIO, 
SDL-CFFI::SDL-INIT-TIMER, SDL-CFFI::SDL-INIT-JOYSTICK,
SDL-CFFI::SDL-INIT-EVENTTHREAD, SDL-CFFI::SDL-INIT-NOPARACHUTE

When STATUS is T, the SDL subsystems in FLAG that are initialized are returned as an INTEGER bitmask.

When STATUS is NIL, the SDL subsystems in FLAG that are uninitiazed are returned as an INTEGER bitmask."
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
  "Initializes the SDL subsystems specified in FLAGS. 
Where FLAG is an INTEGER bitmask containing the logior of the following:
SDL-CFFI::SDL-INIT-EVERYTHING, SDL-CFFI::SDL-INIT-VIDEO, 
SDL-CFFI::SDL-INIT-CDROM, SDL-CFFI::SDL-INIT-AUDIO, 
SDL-CFFI::SDL-INIT-TIMER, SDL-CFFI::SDL-INIT-JOYSTICK,
SDL-CFFI::SDL-INIT-EVENTTHREAD, SDL-CFFI::SDL-INIT-NOPARACHUTE

INIT-SUB-SYSTEMS can be called only after SDL is succesfully initialized using INIT-SDL."
  (sdl-cffi::sdl-init-sub-system (return-sub-systems-of-status flags nil)))

(defun quit-sub-systems (&optional (flags *quit-on-exit*))
  "Uninitializes the SDL subsystems specified in FLAGS. 
Where FLAG is an INTEGER bitmask containing the logior of the following:
SDL-CFFI::SDL-INIT-EVERYTHING, SDL-CFFI::SDL-INIT-VIDEO, 
SDL-CFFI::SDL-INIT-CDROM, SDL-CFFI::SDL-INIT-AUDIO, 
SDL-CFFI::SDL-INIT-TIMER, SDL-CFFI::SDL-INIT-JOYSTICK,
SDL-CFFI::SDL-INIT-EVENTTHREAD, SDL-CFFI::SDL-INIT-NOPARACHUTE

QUIT-SUB-SYSTEMS can be called only after SDL is successfully intialized using INIT-SDL."
  (sdl-cffi::sdl-quit-sub-system (return-sub-systems-of-status flags t)))

(defun sdl-quit-on-exit ()
  "Returns T if QUIT-SDL is called when the macro WITH-INIT exits. 
Returns NIL otherwise."
  *sdl-quit-on-exit*)
(defun set-sdl-quit-on-exit (val)
  (setf *sdl-init-on-startup* val
	*sdl-quit-on-exit* val))
(defsetf sdl-quit-on-exit set-sdl-quit-on-exit)

(defun initialized-sub-systems-p ()
  "Returns a list of the initialized SDL subsystems."
  (list-sub-systems (sdl-cffi::sdl-was-init sdl-cffi::sdl-init-everything)))

(defun init-sdl (&optional (init (sdl-init-on-startup)))
  "Initializes SDL using INIT-SDL when the &optional parameter INIT is T, or
the value returned by \(SDL-INIT-ON-STARTUP\) when INIT is unspecified."
  (if (or init
	  (not *sdl-initialized*))
      (if (sdl-base::init-sdl :flags nil)
	  (setf *sdl-initialized* t)
	  nil)
      t))

(defun quit-sdl (&optional (quit (sdl-quit-on-exit)))
  "Uninitalizes SDL using QUIT-SDL when the &optional parameter QUIT is T, or 
the value returned by \(SDL-QUIT-ON-EXIT\) if QUIT is unspecified."
  (when quit
    (setf *sdl-initialized* nil)
    (sdl-cffi::SDL-Quit)))

(defun sdl-init-on-startup ()
  "Returns T if INIT-SDL will be called in the macro WITH-INIT. 
Returns NIL otherwise."
  *sdl-init-on-startup*)
(defun set-sdl-init-on-startup (val)
  (setf *sdl-init-on-startup* val))
(defsetf sdl-init-on-startup set-sdl-init-on-startup)
