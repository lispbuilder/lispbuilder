(in-package #:lispbuilder-sdl)

;;;; Functions

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
* [INIT-SUBSYSTEMS](#init-subsystems)
* [QUIT-SUBSYSTEMS](#quit-subsystems)
* [INIT-SDL](#init-sdl)
* [QUIT-SDL](#quit-sdl)
* [LIST-SUBSYSTEMS](#list-subsystems)
* [RETURN-SUBSYSTEMS-OF-STATUS](#return-subsystems-of-status)

##### Defaults

`WITH-INIT` will initialize the SDL subsystem and additional subsystems in `:FLAGS` if specified. If `:FLAGS` is not specified, then 
initializes the subsystems specified by [INITIALIZE-SUBSYSTEMS-ON-STARTUP](#initialize-subsystems-on-startup).
`WITH-INIT` will uninitialize the SDL subsystem and additional subsystems in `:FLAGS` if specified.

###### Initialisation/Uninitialisation of the SDL library

The SDL library is initialized only:
* If the library is not yet already initialized.

The SDL library is uninitialized only:
* When [*QUIT-ON-EXIT*](#*quit-on-exit*) is `T`.

###### Initialisation/Uninitialisation of external libraries

Hooks are provided to allow external libraries to be initialized or uninitialised automatically following 
the initialisation or uninitialisation of the SDL library.

To initialise an external library, push a function that initialises the external library onto 
`\*EXTERNAL-INIT-SUBSYSTEMS-ON-STARTUP\*`. The function must take no arguments. For example:

    \(defun init-ttf \(\)
       \(if \(is-init\)
         t
         \(sdl-ttf-cffi::ttf-init\)\)\)
    \(pushnew 'init-ttf sdl:*external-init-subsystems-on-startup*\) 

To uninitialise an external library, push a function that uninitialises the external library onto
 `\*EXTERNAL-QUIT-SUBSYSTEMS-ON-EXIT\*`. The function must take no arguments. For example:

    \(defun quit-ttf \(\)
       \(if \(is-init\)
         \(sdl-ttf-cffi::ttf-quit\)\)\)
    \(pushnew 'quit-ttf sdl:*external-quit-subsystems-on-exit*\)

##### Parameters

* `FLAGS` may be one or more of: `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-JOYSTICK`, and `SDL-INIT-NOPARACHUTE`. *Note*: When `FLAGS` is set by 
`WITH-INIT`, subsequent calls to [INITIALIZE-SUBSYSTEMS-ON-STARTUP](#initialize-subsystems-on-startup) and 
[QUIT-SUBSYSTEMS-ON-EXIT](#quit-subsystems-on-exit) are ignored. `WITH-INIT` will only initialize and uninitialize the subsytems 
specified in `FLAGS`.

##### Example

    \(with-init \(SDL-INIT-VIDEO SDL-INIT-CDROM SDL-INIT-AUDIO\)
      ....\)

    \(with-init \(\)
      \(INITIALIZE-SUBSYSTEMS-ON-STARTUP SDL-INIT-VIDEO SDL-INIT-CDROM SDL-INIT-AUDIO\)
      \(QUIT-SUBSYSTEMS-ON-EXIT SDL-INIT-VIDEO SDL-INIT-CDROM SDL-INIT-AUDIO\)
      ....\)"
  `(block nil
     (unwind-protect
         (when (init-sdl :flags ',flags)
           ,@body)
       (close-audio)
       (quit-sdl :flags ',flags))))

(defun list-subsystems (flag)
  "Returns a list of SDL subsystems that are specified in `FLAGS`.

`FLAGS` is an `INTEGER` bitmask containing the logior of zero or more of: `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`,
`SDL-INIT-JOYSTICK` and `SDL-INIT-NOPARACHUTE`."
  (let ((subsystems nil))
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
            subsystems))
    subsystems))

(defun return-subsystems-of-status (flags status)
  "Returns the `STATUS` of the the specified SDL subsystems in `FLAGS`. 

##### Parameters
* `FLAGS` must contain one or more of: `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`,
`SDL-INIT-JOYSTICK`.
* `STATUS` when `T` returns the specified initialized subsystems.
`STATUS` when `NIL`, returns the specified uninitialised subsystems."
  (let ((initialized (list-subsystems (sdl-cffi::sdl-was-init sdl-cffi::sdl-init-everything)))
        (queried (list-subsystems (sdl-base::set-flags flags)))
        (to-initialize 0))
    (mapc #'(lambda (subsystem)
              (let ((value (second subsystem)))
                (setf to-initialize (logior to-initialize
                                            value))))
          (if status
            (intersection queried initialized :key #'car)
            (set-difference queried initialized :key #'car)))
    to-initialize))

(defun init-subsystems (subsystems &optional force)
  "Initializes only the SDL subsystems specified in `FLAGS` that are not
already initialized. 
`subsystems` is a list containing the one or more of:
`SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-JOYSTICK`, and
`SDL-INIT-NOPARACHUTE`.

`INIT-SUBSYSTEMS` can be called only after SDL is succesfully initialized by [INIT-SDL](#init-sdl)."
  (if (= -1
         (if force
           (sdl-cffi::sdl-init-subsystem (sdl-base::set-flags subsystems))
           (sdl-cffi::sdl-init-subsystem (return-subsystems-of-status (sdl-base::set-flags subsystems) nil))))
    nil
    t))

(defun quit-subsystems (subsystems &optional force)
  "Uninitializes only the SDL subsystems specified in the `FLAGS` that are
already initialized.
`subsystems` is a list containing the one or more of:
`SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-JOYSTICK` and `SDL-INIT-NOPARACHUTE`.

`QUIT-SUBSYSTEMS` can be called only after SDL is successfully intialized using [INIT-SDL](#init-sdl)."
  (if force
    (sdl-cffi::sdl-init-subsystem (sdl-base::set-flags subsystems))
    (sdl-cffi::sdl-quit-subsystem (return-subsystems-of-status (sdl-base::set-flags subsystems) t))))

(defun initialized-subsystems-p (&optional subsystem)
  "Returns a list of the initialized SDL subsystems."
  (if subsystem
    (if (and (/= 0 (logand (sdl-cffi::sdl-was-init sdl-cffi::sdl-init-everything)
                           (sdl-base::set-flags subsystem)))
             (>= (sdl-cffi::sdl-was-init sdl-cffi::sdl-init-everything) (sdl-base::set-flags subsystem)))
      (list-subsystems (sdl:return-subsystems-of-status (sdl-base::set-flags subsystem) t)))
    (list-subsystems (sdl:return-subsystems-of-status (sdl-base::set-flags sdl:sdl-init-everything) t))))

(defun init-sdl (&key flags force video cdrom audio joystick no-parachute)
  "Initalizes the SDL library."
  (unless flags
    (setf flags (remove nil (list (when video sdl-init-video)
                                  (when cdrom sdl-init-cdrom)
                                  (when audio sdl-init-audio)
                                  (when joystick sdl-init-joystick)
                                  (when no-parachute sdl-init-noparachute)))))
  (let ((init? (when (sdl-base::init-sdl)
                 (init-subsystems (sdl-base::set-flags flags) force))))
    (dolist (fn *external-init-subsystems-on-startup*)
      (funcall fn))
    init?))

(defun quit-sdl (&key flags force
                      video cdrom audio joystick no-parachute)
  (dolist (fn *external-quit-subsystems-on-exit*)
    (funcall fn))
  (unless flags
    (setf flags (sdl-base::set-flags (remove nil
                                             (list (when video sdl-init-video)
                                                   (when cdrom sdl-init-cdrom)
                                                   (when audio sdl-init-audio)
                                                   (when joystick sdl-init-joystick)
                                                   (when no-parachute sdl-init-noparachute))))))
  (if flags
    (setf flags (sdl-base::set-flags flags))
    (setf flags (sdl-cffi::sdl-was-init sdl-cffi::sdl-init-everything)))
  (if (/= flags 0)
    (quit-subsystems flags force)
    (sdl-cffi::sdl-quit)))

(defun register-physics (fn)
  (setf (ps-fn *default-fpsmanager*) fn))

(defun sdl-init-p ()
  (initialized-subsystems-p))
(defun video-init-p ()
  (when (/= 0 (logand (sdl-cffi::sdl-was-init sdl-init-everything)
                      sdl-init-video))
    t))
(defun audio-init-p ()
  (when (/= 0 (logand (sdl-cffi::sdl-was-init sdl-init-everything)
                      sdl-init-audio))
    t))
(defun joystick-init-p ()
  (when (/= 0 (logand (sdl-cffi::sdl-was-init sdl-init-everything)
                      sdl-init-joystick))
    t))
(defun cdrom-init-p ()
  (when (/= 0 (logand (sdl-cffi::sdl-was-init sdl-init-everything)
                      sdl-init-cdrom))
    t))

(defun init-video (&optional force)
  (init-subsystems sdl-init-video force))
(defun init-audio (&optional force)
  (init-subsystems sdl-init-audio force))
(defun init-joystick (&optional force)
  (init-subsystems sdl-init-joystick force))
(defun init-cdrom (&optional force)
  (init-subsystems sdl-init-cdrom force))

(defun quit-video (&optional force)
  (quit-subsystems sdl-init-video force))
(defun quit-audio (&optional force)
  (quit-subsystems sdl-init-audio force))
(defun quit-joystick (&optional force)
  (quit-subsystems sdl-init-joystick force))
(defun quit-cdrom (&optional force)
  (quit-subsystems sdl-init-cdrom force))
