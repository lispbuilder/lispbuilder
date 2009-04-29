
(in-package #:rm)

(defclass rm-pipe (foreign-object) ()
  (:default-initargs
   :free (simple-free #'rm-cffi::rm-pipe-delete 'rm-pipe)))

(defclass pipe (rm-pipe) ()
  (:default-initargs
   :target :RM-PIPE-NOPLATFORM
   :processing-mode :RM-PIPE-MULTISTAGE
   :opaque-3d t
   :transparent-3d t
   :opaque-2D t
   :swap-buffers nil
   :display-list t
   :init-matrix-stack t
   :channel-format :rm-mono-channel
   :background-color (color 0.2 0.2 0.3 1.0)
   :notify-level t))

(defmethod initialize-instance :after ((self pipe) &key
				       target processing-mode
                                       opaque-3d transparent-3d opaque-2D
                                       channel-format
                                       dimensions
                                       display-list swap-buffers
                                       init-matrix-stack
                                       background-color
                                       notify-level)

  (unless *initialised*
    (setf *initialised* t)
    (rm-cffi::rm-Init))

  (cond
   ((not notify-level)
    (rm-cffi::rm-notify-level :rm-notify-silence))
   (t
    (rm-cffi::rm-notify-level :rm-notify-full)))
  
  (setf (slot-value self 'foreign-pointer-to-object) (rm-cffi::rm-Pipe-New target))
  
  (when (cffi:null-pointer-p (fp self))
    (error "Cannot create OpenRM Pipe: ~A" (fp self)))
  
  (unless (set-render-pass self opaque-3d transparent-3d opaque-2d)
    (error "Cannot specify the rendering pass."))
  (unless (setf (processing-mode self) processing-mode)
    (error "Cannot specify the processing mode."))

  (setf (swap-buffers self) swap-buffers)
  (setf (init-matrix-stack self) init-matrix-stack)
  (setf (channel-format self) channel-format)
  (setf (display-list-p self) display-list)
  
  (when dimensions
    (setf (dimensions self) dimensions))
  (when background-color
    (setf (background-color self) background-color)))

(defmethod (setf swap-buffers)(value (self pipe))
  (if (not value)
    (rm-cffi::rm-Pipe-Set-Swap-Buffers-Func (fp self) (cffi:null-pointer))
    (if (cffi:pointerp value)
      (rm-cffi::rm-Pipe-Set-Swap-Buffers-Func (fp self) value)))
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
  (rm-cffi::rm-pipe-set-window-size (fp self) (elt size 0) (elt size 1)))

(defmethod channel-format ((self rm-pipe))
  (rm-cffi::rm-pipe-get-channel-format (fp self)))
(defmethod (setf channel-format) (value (self rm-pipe))
  (rm-cffi::rm-pipe-set-channel-format (fp self) value))

(defmethod display-list-p ((self rm-pipe))
  (rm-cffi::rm-pipe-get-display-list-enable (fp self)))
(defmethod (setf display-list-p) (value (self rm-pipe))
  (rm-cffi::rm-pipe-set-display-list-enable (fp self) value))

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

(defmethod set-render-pass ((self pipe) opaque-3d transparent-3d opaque-2d)
  (setf opaque-3d (if opaque-3d :rm-true :rm-false))
  (setf transparent-3d (if transparent-3d :rm-true :rm-false))
  (setf opaque-2d (if opaque-2d :rm-true :rm-false))
  (rm-cffi::rm-Pipe-Set-Render-Pass-Enable (fp self) opaque-3d transparent-3d opaque-2D))

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

(defmethod (setf processing-mode) (value (self pipe))
  (rm-cffi::rm-Pipe-Set-Processing-Mode (fp self) value))
(defmethod processing-mode ((self pipe))
  (rm-cffi::rm-pipe-get-processing-mode (fp self)))

(defmethod (setf init-matrix-stack) (value (self pipe))
  (if value
    (rm-cffi::rm-pipe-set-init-matrix-stack-mode (fp self) :rm-true)
    (rm-cffi::rm-pipe-set-init-matrix-stack-mode (fp self) :rm-false)))
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
  (rm-cffi::rm-Pipe-Make-Current (fp self)))

(defmethod print-object ((obj pipe) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "\(
:GC-P              ~s
:COPY-P            ~s
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
          (gc-p obj) (copy-p obj) (dimensions obj)
          (opaque-3d-p obj) (transparent-3d-p obj) (opaque-2d-p obj)
          (channel-format obj) (init-matrix-stack obj) (processing-mode obj)
          (background-color obj) (display-list-p obj) (frame-rate obj)
          )))
