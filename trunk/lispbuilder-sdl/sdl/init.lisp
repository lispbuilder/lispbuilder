(in-package #:lispbuilder-sdl)

;;;; Functions


;; (defmacro with-init (renderer &body body)
;;   (let ((rend (gensym "rend-Initializes")))
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
  "`WITH-INIT` is a convenience macro that will attempt to initialise the SDL library and SDL subsystems 
prior to executing the forms in `BODY`. Upon exit `WITH-INIT` will uninitialize the SDL library and SDL subsystems.

The lispbuilder-sdl initialization routines are somewhat complicated by the fact that 
a Lisp development environment will load a foreign library once but then 
initialise and uninitialise the library multiple times. A C/C++ development environment will open and then 
close a library after each execution, freeing all resources left hanging by incomplete 
or buggy uninitialise functions. C libraries may therefore frequently core dump in a Lisp environment when 
resources are not feed properly prior to the library being reinitialized.

LISPBUILDER-SDL provides functionality affording the programmer a finer granularity of control 
of the initialisation/uninitialisation of foreign libraries. The
fuctions that provide these capabilities are as follows:
* [INITIALIZE-ON-STARTUP](#initialize-on-startup)
* [QUIT-ON-EXIT](#quit-on-exit)
* [LIST-SUB-SYSTEMS](#list-sub-systems)
* [RETURN-SUB-SYSTEMS-OF-STATUS](#return-sub-systems-of-status)
* [INIT-SUB-SYSTEMS](#init-sub-systems)
* [QUIT-SUB-SYSTEMS](#quit-sub-systems)
* [INIT-SDL](#init-sdl)
* [QUIT-SDL](#quit-sdl)

##### Defaults

* By default `WITH-INIT` will only initialise the `SDL-INIT-VIDEO` SDL subsystem. 
Additional SDL subsystems can be initialized by calling [INITIALIZE-ON-STARTUP](#initialize-on-startup).
* By default `WITH-INIT` will only uninitialise the `SDL-INIT-VIDEO` SDL subsystem. 
Additional SDL subsystems can be uninitialized by calling [QUIT-ON-EXIT](#quit-on-exit).

###### Initialisation/Uninitialisation of the SDL library

The SDL library is initialised only:
* If the library is not yet already initialized, or  
* [SDL-INIT-ON-STARTUP](#sdl-init-on-startup) is `T`.

The SDL library is uninitialised only:
* When [SDL-QUIT-ON-EXIT](#sdl-quit-on-exit) is `T`.

###### Initialisation/Uninitialisation of external libraries

Hooks are provided to allow external libraries to be initialized or uninitialised automatically following 
the initialisation or uninitialisation of the SDL library.

To initialise an external library, push a function that initialises the external library onto 
`\*EXTERNAL-INIT-ON-STARTUP\*`. The function must take no arguments. For example:

    \(defun init-ttf \(\)
       \(if \(is-init\)
         t
         \(sdl-ttf-cffi::ttf-init\)\)\)
    \(pushnew 'init-ttf sdl:*external-init-on-startup*\) 

To uninitialise an external library, push a function that uninitialises the external library onto
 `\*EXTERNAL-QUIT-ON-EXIT\*`. The function must take no arguments. For example:

    \(defun quit-ttf \(\)
       \(if \(is-init\)
         \(sdl-ttf-cffi::ttf-quit\)\)\)
    \(pushnew 'quit-ttf sdl:*external-quit-on-exit*\)

##### Parameters

* `FLAGS` may be one or more of: `SDL-INIT-EVERYTHING`, `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-TIMER`, `SDL-INIT-JOYSTICK`, `SDL-INIT-EVENTTHREAD` and `SDL-INIT-NOPARACHUTE`. *Note*: When `FLAGS` is set by 
`WITH-INIT`, subsequent calls to [INITIALIZE-ON-STARTUP](#initialize-on-startup) and 
[QUIT-ON-EXIT](#quit-on-exit) are ignored. `WITH-INIT` will only initialize and uninitialize the subsytems 
specified in `FLAGS`.

##### Example

    \(with-init \(SDL-INIT-VIDEO SDL-INIT-CDROM SDL-INIT-AUDIO\)
      ....\)

    \(with-init \(\)
      \(INITIALIZE-ON-STARTUP SDL-INIT-VIDEO SDL-INIT-CDROM SDL-INIT-AUDIO\)
      \(QUIT-ON-EXIT SDL-INIT-VIDEO SDL-INIT-CDROM SDL-INIT-AUDIO\)
      ....\)"
  `(block nil
     (unwind-protect
	  (when (init-sdl)
	    ,(when flags
		   `(initialize-on-startup ,@flags))
	    (init-sub-systems)
	    ,@body)
       ,(when flags
	      `(quit-on-exit ,@flags))
       (quit-sub-systems)
       (quit-sdl))))

(defun initialize-on-startup (&rest flags)
  "Sets the SDL subsystems that must be initialized in 
subsequent calls to [INIT-SUB-SYSTEMS](#init-sub-systems).

##### Parameters

* `FLAGS` may be one or more of: `SDL-INIT-EVERYTHING`, `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-TIMER`, `SDL-INIT-JOYSTICK`, `SDL-INIT-EVENTTHREAD` and `SDL-INIT-NOPARACHUTE`.

##### Returns

* Returns an INTEGER bitmask of the SDL subsystems in `FLAGS`.

##### Example

    \(INITIALIZE-ON-STARTUP SDL:SDL-INIT-VIDEO SDL:SDL-INIT-CDROM\)"
  (setf *initialize-on-startup* (apply #'logior flags)))

(defun quit-on-exit (&rest flags)
    "Sets one or more SDL subsystems that must be uninitialized in 
subsequent calls to [QUIT-SUB-SYSTEMS](#quit-sub-systems). 

##### Parameters

* `FLAGS` may be one or more of: `SDL-INIT-EVERYTHING`, `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-TIMER`, `SDL-INIT-JOYSTICK`, `SDL-INIT-EVENTTHREAD`, `SDL-INIT-NOPARACHUTE`.

##### Returns

* Returns an INTEGER bitmask of the SDL subsystems in `FLAGS`.

##### Example

    \(QUIT-ON-EXIT SDL:SDL-INIT-VIDEO SDL:SDL-INIT-CDROM\)"
  (setf *quit-on-exit* (apply #'logior flags)))

(defun list-sub-systems (flag)
  "Returns a list of SDL subsystems that are specified in `FLAGS`.

`FLAGS` is an `INTEGER` bitmask containing the logior of zero or more of: `SDL-INIT-EVERYTHING`, `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-TIMER`, `SDL-INIT-JOYSTICK`, `SDL-INIT-EVENTTHREAD` and `SDL-INIT-NOPARACHUTE`."
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
  "Returns the status `STATUS` of the the specified SDL subsystems in `FLAGS` as an INTEGER bitmask. 

##### Parameters
* `FLAGS` may contain a logior of zero or more of: `SDL-INIT-EVERYTHING`, `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-TIMER`, `SDL-INIT-JOYSTICK`, `SDL-INIT-EVENTTHREAD`, `SDL-INIT-NOPARACHUTE`.
* `STATUS` when `T` determines the status of initialised subsystems.
`STATUS` when `NIL`, determines the status of uninitialised subsystems. 

##### Returns
* Returns an INTEGER bitmask of the SDL subsystems in `FLAGS` that are initialised when `STATUS` is `T` or 
uninitialised when `STATUS` is `NIL`."
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
  "Initializes the SDL subsystems specified in `FLAGS`. 
`FLAGS` is an `INTEGER` bitmask containing the logior of zero or more of: `SDL-INIT-EVERYTHING`, `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-TIMER`, `SDL-INIT-JOYSTICK`, `SDL-INIT-EVENTTHREAD` and `SDL-INIT-NOPARACHUTE`.

`INIT-SUB-SYSTEMS` can be called only after SDL is succesfully initialized by [INIT-SDL](#init-sdl)."
  (sdl-cffi::sdl-init-sub-system (return-sub-systems-of-status flags nil)))

(defun quit-sub-systems (&optional (flags *quit-on-exit*))
  "Uninitializes the SDL subsystems specified in the `INTEGER` bitmask `FLAGS`. 
`FLAGS` contains the logior of zero or more of: `SDL-INIT-EVERYTHING`, `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-TIMER`, `SDL-INIT-JOYSTICK`, `SDL-INIT-EVENTTHREAD`, `SDL-INIT-NOPARACHUTE`.

`QUIT-SUB-SYSTEMS` can be called only after SDL is successfully intialized using [INIT-SDL](#init-sdl)."
  (sdl-cffi::sdl-quit-sub-system (return-sub-systems-of-status flags t)))

(defun sdl-quit-on-exit ()
  "Returns `T` if the SDL library must be uninitialised in [QUIT-SDL](#quit-sdl), or [WITH-INIT](#with-init). 
Returns `NIL` otherwise."
  *sdl-quit-on-exit*)
(defun set-sdl-quit-on-exit (val)
  (if val
      (setf *sdl-init-on-startup* val
	    *sdl-quit-on-exit* val)
      (setf *sdl-quit-on-exit* val)))
(defsetf sdl-quit-on-exit set-sdl-quit-on-exit)

(defun initialized-sub-systems-p ()
  "Returns a list of the initialized SDL subsystems."
  (list-sub-systems (sdl-cffi::sdl-was-init sdl-cffi::sdl-init-everything)))

(defun init-sdl (&optional (init (sdl-init-on-startup)))
  "Initalizes the SDL library when the `OPTIONAL` parameter `INIT` is `T`, or 
the value returned by [SDL-INIT-ON-STARTUP](#sdl-init-on-startup) is `T`."
  (let ((initialized? (if (or init
			      (not *sdl-initialized*))
			  (if (sdl-base::init-sdl :flags nil)
			      (setf *sdl-initialized* t)
			      nil)
			  t)))
    (dolist (fn *external-init-on-startup*)
      (funcall fn))
    (setf sdl-base::*default-fpsmanager* (make-instance 'sdl-base::fps-fixed))
    initialized?))

(defun quit-sdl (&optional (quit (sdl-quit-on-exit)))
  "Uninitalizes the SDL library when the `OPTIONAL` parameter `QUIT` is `T`, or 
the value returned by [SDL-QUIT-ON-EXIT](#sdl-quit-on-exit) is `T`."
  (when quit
    (setf *sdl-initialized* nil)
    (sdl-cffi::SDL-Quit))
  (dolist (fn *external-quit-on-exit*)
    (funcall fn)))

(defun sdl-init-on-startup ()
    "Returns `T` if the SDL library must be initialised in [INIT-SDL](#init-sdl), or [WITH-INIT](#with-init). 
Returns `NIL` otherwise."
  *sdl-init-on-startup*)
(defun set-sdl-init-on-startup (val)
  (setf *sdl-init-on-startup* val))
(defsetf sdl-init-on-startup set-sdl-init-on-startup)
