
(in-package #:rm)

(define-condition pipe-creation-error (error)
  ((self :initarg :self :reader self)))

(define-condition set-opengl-context-error (error)
  ((self :initarg :self :reader self)))

(define-condition pipe-set-window-error (error)
  ((self :initarg :self :reader self)))

(define-condition pipe-make-current-error (error)
  ((self :initarg :self :reader self)))

(define-condition swap-buffer-error (error)
  ((self :initarg :self :reader self)))

(define-condition rendering-pass-error (error)
  ((pipe :initarg :pipe :reader get-pipe)
   (opaque-3d :initarg :opaque-3d :reader opaque-3d-p)
   (transparent-3d :initarg :transparent-3d-p :reader transparent-3d-p)
   (opaque-2d :initarg :opaque-2d :reader opaque-2d-p)))

(define-condition processing-mode-error (error)
  ((pipe :initarg :pipe :reader get-pipe)
   (mode :initarg :mode :reader get-processing-mode)))

(defclass rm-pipe (foreign-object)()
  (:default-initargs
   :free (simple-free #'rm-cffi::rm-pipe-delete 'rm-pipe)))

(defclass pipe (rm-pipe)
  ((target-platform
    :initarg :target
    :reader target-platform)
   (text-support
    :initarg :text-support-p
    :reader text-support-p))
  (:default-initargs
   ;; Do not get any bright ideas and assign anything to :fp,
   ;; as this will short-circuit normal pipe behaviour.
   :target :PIPE-NOPLATFORM
   :processing-mode :PIPE-MULTISTAGE
   :opaque-3d t
   :transparent-3d t
   :opaque-2D t
   :swap-buffers nil
   :display-list t
   :init-matrix-stack t
   :channel-format :MONO-CHANNEL
   :background-color (color 0.2 0.2 0.3 1.0)
   :notify-level :NOTIFY-FULL
   :text-support-p t)
  (:documentation
   "A `PIPE` contains all the information that pertains to
the display environment, including window handles and the OpenGL context.

\(1\) Use `:SWAP-BUFFERS` to set the swapbuffer function.
    Specifying a `:TARGET` of `:PIPE-NOPLATFORM` will result in no swapbuffers function being
    assigned to the `PIPE` and the application is then responsible for performing the
    swapbuffers call.
(2) Use `:TARGET` to set the target platform.
    `:TARGET` must be one of `:PIPE-GLX`, `:PIPE-WGL`, `:PIPE-CR` or `:PIPE-NOPLATFORM`.
    `:PIPE-GLX` specifies use on any X-based system that has the GLX extension
     \(supports OpenGL through the server\).
     Opens the X display referenced by the $DISPLAY environment variable and
     assigns it to the RMpipe using rmxPipeSetDisplay.
    `:PIPE-WGL` specifies use on a Win32 platform.
    `:PIPE-CR` specifies use only on a Chromium-enabled system.
    `:PIPE-NOPLATFORM` is intended to be used by applications that want to perform
    all OpenGL context management, including swapping the buffers.

;;Display to the RMpipe using rmxPipeSetDisplay if either of the following is true:
;;(1) your scene graph contains RM_TEXT primitives, or (2) RM will be performing
;;the SwapBuffers operation. If neither is true, then you do not need to assign a Display
;;to the RMpipe when using the RM_PIPE_NOPLATFORM enumerator.

\(3\) The default `:CHANNEL-FORMAT` if `:MONO-CHANNEL`, which corresponds to
    an OpenGL visual that has RGBA color buffers, a depth buffer, and is doublebuffered.
    The channel format can be set after a `PIPE` is created, but
    before a winfows is created or drawing occurs, using `CHANNEL-FORMAT`.
\(4\) The default `:PROCESSING-MODE` is `:PIPE-MULTISTAGE`. This mode corresponds
    to serial rendering using a two-stage pipeline. You can set the processing
    mode after a `PIPE` is created but before `PIPE-MAKE-CURRENT` using `PROCESSING-MODE`.
    Other processing modes (not yet tested) include;
    `:PIPE-SERIAL`, `:PIPE-MULTISTAGE-PARALLEL`, `:PIPE-MULTISTAGE-VIEW-PARALLEL`,
    `:PIPE-SERIAL-NOBLOCK`, `:PIPE-MULTISTAGE-NOBLOCK and `:PIPE-MULTISTAGE-PARALLEL-NOBLOCK`.
\(5\) The post render barrier function is set to NULL (see rmPipeSetPostRender-
    BarrierFunc - TBD).
\(6\) The post render function is set to NULL (see rmPipeSetPostRenderFunc - TBD).
\(7\) Each of the following three passes of the multipass rendering engine is enabled
    by default: `OPAQUE-3D`, `TRANSPARENT-3D`, `OPAQUE-2D`. Applications are unable to
    change the order of these rendering passes, but may enable or disable a given
    rendering pass with `ENABLE-RENDER-PASS`.
\(8\) The `PIPE` will use OpenGL display lists during rendering. You can globally
    enable or disable use of display lists on an using RMpipe with the routine
    `ENABLE-DISPLAY-LIST`.
\(9\) The `PIPE` takes control of the OpenGL matrix stack during rendering.
    This behavior assumes that RM is not sharing the OpenGL context with any
    other processes. You can change this behavior with `INIT-MATRIX-STACK`.
\(10\) Context management. If you specify one of `:PIPE-GLX`, `:PIPE-WGL`, or
     `:PIPE-CR`, a platform-specific routine will be assigned to the `PIPE`
     that will be used to create a platform-appropriate OpenGL context when your
     application makes a call to `PIPE-MAKE-CURRENT`. If you specify
     `:PIPE-NOPLATFORM, your application must create and make current
     an appropriate OpenGL context, but need not assign it to the `PIPE`.
\(11\) The frame rate for the `PIPE` is set to -1, which means that no attempt will
     be made to marshal frame rendering times."))

(defmethod initialize-instance :before ((self pipe) &key
                                        fp
                                        &allow-other-keys)
  ;; 1. Call rmInit to initialize RM.
  (unless fp
    (init-rm)))

(defmethod initialize-instance :after ((self pipe) &key
                                       target processing-mode
                                       opaque-3d transparent-3d opaque-2D
                                       channel-format
                                       ;;dimensions
                                       display-list swap-buffers
                                       init-matrix-stack
                                       background-color
                                       notify-level)
  (unless (this-fp self)
    ;; Turn on all warnings from the OpenRM library itself.
    ;; Invaluable for debugging purposes.
    (set-notify-level notify-level)

    ;; 2.1 Create an RMpipe object with rmPipeNew. After the pipe has been created, but
    ;; before you create a window.
    (let ((pipe-fp (rm-cffi::rm-Pipe-New target)))
      (if (cffi:null-pointer-p pipe-fp)
        (error 'pipe-creation-error :self self)
        (setf (slot-value self 'simfin::foreign-pointer-to-object) pipe-fp)))
  
    (enable-render-pass self :opaque-3d opaque-3d :transparent-3d transparent-3d :opaque-2d opaque-2d)
    (setf (processing-mode self) processing-mode)

    (setf (swap-buffers self) swap-buffers)
    (setf (init-matrix-stack self) init-matrix-stack)
    (setf (channel-format self) channel-format)

    (when display-list
      (enable-display-list self))
  
    ;;(when dimensions
    ;;  (setf (dimensions self) dimensions))
  
    (when background-color
      (setf (background-color self) background-color))))

(defun set-notify-level (level)
  "Sets the warning level.
   `:NOTIFY-SILENCE` will turn all warnings off.
   `:NOTIFY-FULL` will turn all warnings on."
  (check-type level keyword)
  (setf *notify-level* level)
  (rm-cffi::rm-notify-level *notify-level*))

(defmethod (setf swap-buffers)(value (self pipe))
  (if (not value)
    (rm-cffi::rm-Pipe-Set-Swap-Buffers-Func (fp self) (cffi:null-pointer))
    (if (cffi:pointerp value)
      (rm-cffi::rm-Pipe-Set-Swap-Buffers-Func (fp self) value)
      (error 'swap-buffer-error :self self)))
  self)

(defmethod swap-buffers ((self pipe))
  "Causes a buffer-swap on the display and window specified by `PIPE`.
This routine is most typically called from within RM after
rendering has completed rather than directly from the application. There is
nothing that precludes it's use directly by applications, if so desired."
  (rm-cffi::rm-pipe-swap-buffers (fp self)))

(defmethod width ((self rm-pipe))
  (cffi:with-foreign-object (width :int)
    (rm-cffi::rm-Pipe-Get-Window-Size (fp self) width (cffi:null-pointer))
    (cffi:mem-aref width :int)))

(defmethod height ((self rm-pipe))
  (cffi:with-foreign-object (height :int)
    (rm-cffi::rm-Pipe-Get-Window-Size (fp self) (cffi:null-pointer) height)
    (cffi:mem-aref height :int)))

(defmethod dimensions ((self rm-pipe))
  (vector (width self) (height self)))
(defmethod (setf dimensions) ((size vector) (self rm-pipe))
  (unless (rm-cffi::rm-pipe-set-window-size (fp self) (elt size 0) (elt size 1))
    (error 'pipe-set-window-error :self self)))

(defmethod channel-format ((self rm-pipe))
  (rm-cffi::rm-pipe-get-channel-format (fp self)))
(defmethod (setf channel-format) (value (self rm-pipe))
  (rm-cffi::rm-pipe-set-channel-format (fp self) value))

(defmethod display-list-p ((self rm-pipe))
  (rm-cffi::rm-pipe-get-display-list-enable (fp self)))
(defmethod (setf display-list-p) (value (self rm-pipe))
  (rm-cffi::rm-pipe-set-display-list-enable (fp self) value))
(defmethod enable-display-list ((self rm-pipe))
  (setf (display-list-p self) t))
(defmethod disable-display-list ((self rm-pipe))
  (setf (display-list-p self) nil))

(defmethod frame-rate ((self rm-pipe))
  "Returns the `PIPE` render rate in frames per second.
Returns `-1` for an unconstrained rendering rate, and `NIL` on error."
  (let ((rate (rm-cffi::rm-pipe-get-frame-rate (fp self))))
    (when (/= rate 0)
      rate)))
(defmethod (setf frame-rate) (value (self rm-pipe))
  (rm-cffi::rm-pipe-set-frame-rate (fp self) value))

(defmethod close-pipe ((self pipe))
  (rm-cffi::rm-pipe-close (fp self)))

(defmethod delete-pipe ((self pipe))
  (free self))

(defmethod enable-render-pass ((self pipe) &key (opaque-3d t) (transparent-3d t) (opaque-2d t))
  (unless (rm-cffi::rm-Pipe-Set-Render-Pass-Enable (fp self) opaque-3d transparent-3d opaque-2D)
    (error 'rendering-pass-error :pipe self :opaque-3d opaque-3d :transparent-3d transparent-3d :opaque-2d opaque-2d)))

(defmethod opaque-3d-p ((self pipe))
  (cffi:with-foreign-object (render-pass :pointer)
    (rm-cffi::rm-pipe-get-render-pass-enable (fp self)
                                             render-pass
                                             (cffi:null-pointer)
                                            (cffi:null-pointer))
    (if (eq :rm-true
            (foreign-enum-keyword 'rm-cffi::rm-enum-wrapper
                                  (cffi::mem-aref render-pass :int)))
      t
      nil)))

(defmethod (setf opaque-3d-p) (value (self pipe))
  (let ((opaque-3d value)
        (transparent-3d (transparent-3d-p self))
        (opaque-2d (opaque-2d-p self)))
    (unless (rm-cffi::rm-Pipe-Set-Render-Pass-Enable (fp self) opaque-3d transparent-3d opaque-2D)
      (error 'rendering-pass-error :pipe self :opaque-3d opaque-3d :transparent-3d transparent-3d :opaque-2d opaque-2d))))

(defmethod transparent-3d-p ((self pipe))
  (cffi:with-foreign-object (render-pass :pointer)
    (rm-cffi::rm-pipe-get-render-pass-enable (fp self)
                                             (cffi:null-pointer)
                                             render-pass
                                             (cffi:null-pointer))
    (if (eq :rm-true
            (foreign-enum-keyword 'rm-cffi::rm-enum-wrapper
                                  (cffi::mem-aref render-pass :int)))
      t
      nil)))

(defmethod (setf transparent-3d-p) (value (self pipe))
  (let ((opaque-3d (opaque-3d-p self))
        (transparent-3d value)
        (opaque-2d (opaque-2d-p self)))
    (unless (rm-cffi::rm-Pipe-Set-Render-Pass-Enable (fp self) opaque-3d transparent-3d opaque-2D)
      (error 'rendering-pass-error :pipe self :opaque-3d opaque-3d :transparent-3d transparent-3d :opaque-2d opaque-2d))))

(defmethod opaque-2d-p ((self pipe))
  (cffi:with-foreign-object (render-pass :pointer)
    (rm-cffi::rm-pipe-get-render-pass-enable (fp self)
                                             (cffi:null-pointer)
                                             (cffi:null-pointer)
                                             render-pass)
    (if (eq :rm-true
            (foreign-enum-keyword 'rm-cffi::rm-enum-wrapper
                                  (cffi::mem-aref render-pass :int)))
      t
      nil)))

(defmethod (setf opaque-2d-p) (value (self pipe))
  (let ((opaque-3d (opaque-3d-p self))
        (transparent-3d (transparent-3d-p self))
        (opaque-2d value))
    (unless (rm-cffi::rm-Pipe-Set-Render-Pass-Enable (fp self) opaque-3d transparent-3d opaque-2D)
      (error 'rendering-pass-error :pipe self :opaque-3d opaque-3d :transparent-3d transparent-3d :opaque-2d opaque-2d))))

(defmethod (setf processing-mode) (value (self pipe))
  (unless (rm-cffi::rm-Pipe-Set-Processing-Mode (fp self) value)
    (error 'processing-mode-error :pipe self :mode value)))
(defmethod processing-mode ((self pipe))
  (rm-cffi::rm-pipe-get-processing-mode (fp self)))

(defmethod (setf init-matrix-stack) (value (self pipe))
  (unless (rm-cffi::rm-pipe-set-init-matrix-stack-mode (fp self) value)
    (error 'init-matrix-stack-error :pipe self :mode value)))
(defmethod init-matrix-stack ((self pipe))
  (rm-cffi::rm-pipe-get-init-matrix-stack-mode (fp self)))

(defmethod (setf background-color) ((color vector) (self pipe))
  (with-copy-color-4d-to-foreign (color fp)
    (when (rm-cffi::rm-Pipe-Set-Scene-Background-Color (fp self) fp)
      color)))
(defmethod (setf background-color) ((color c4d) (self pipe))
  (when (rm-cffi::rm-Pipe-Set-Scene-Background-Color (fp self) (fp color))
    color))

(defmethod background-color ((self pipe))
  (rm-base:with-c4d (col)
    (when (rm-cffi::rm-pipe-get-scene-background-color (fp self) col)
      (color rm-base:r rm-base:g rm-base:b rm-base:a))))
(defmethod background-color* ((self pipe))
  (let ((col (c4d nil nil nil nil)))
    (when (rm-cffi::rm-pipe-get-scene-background-color (fp self) (fp col))
      col)))

(defmethod pipe-make-current ((self pipe))
  (unless (rm-cffi::rm-Pipe-Make-Current (fp self))
    (error 'pipe-make-current-error :self self)))

(defmethod print-object ((obj pipe) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "\(
:GC-P              ~s
:WIDTH, HEIGHT     ~s
:OPAQUE-3D-P       ~s
:TRANSPARENT-3D-P  ~s
:OPAQUE-2D-P       ~s
:CHANNEL-FORMAT    ~s
:INIT-MATRIX-STACK ~s
:PROCESSING-MODE   ~s
:BACKGROUND-COLOR  ~s
:DISPLAY-LIST-P    ~s
:FRAME-RATE        ~s
\)"
            (gc-p obj) (dimensions obj)
            (opaque-3d-p obj) (transparent-3d-p obj) (opaque-2d-p obj)
            (channel-format obj) (init-matrix-stack obj) (processing-mode obj)
            (background-color obj) (display-list-p obj) (frame-rate obj)
          )))
