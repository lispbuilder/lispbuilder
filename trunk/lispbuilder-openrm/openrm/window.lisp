
(in-package #:rm)

(define-condition pipe-not-specified-error (error)
  ((self :initarg :self :reader self)))

(defclass generic-window (node)
  ((pipe
    :accessor pipe
    :initform nil
    :initarg :pipe)
   (scenes
    :accessor scenes
    :initform nil)
   (default-scene
    :initform nil)
   (width
    :initform 320
    :initarg :width)
   (height
    :initform 240
    :initarg :height))
  (:default-initargs
   :node (get-root-node))
  (:documentation
   "The basic sequence of steps needed to fully initialize OpenRM for rendering are:

(1) Create a `PIPE` object specific to the platform, and assign this to the `WINDOW`
    using `:PIPE`.
(2) Set any optional parameters on the `PIPE - target, processing mode, channel format,
    display, etc..
(3) Create a `WINDOW` suitable for display. 
(4) Assign the `WINDOW` to the `PIPE`.
(5) Make the pipe current."))

(defclass window (generic-window event-handler)()
  (:default-initargs
   :events '(on-idle on-create on-destroy on-paint on-resize on-expose
	     on-quit on-wm-event on-user on-mouse-move on-mouse-up on-mouse-down
	     on-key-down on-key-up on-active on-joy-axis-motion on-joy-button-down
	     on-joy-button-up on-joy-hat-motion on-joy-ball-motion)
   ;;:scene (make-instance 'scene)
   ))

(defmethod initialize-instance :around ((self generic-window)
					&key
                                        parent
                                        scene
                                        pipe)

  (if pipe
    (setf (pipe self) pipe)
    (error 'pipe-not-specified-error :self self))
  
  (call-next-method)
  
  (add-window self)
  ;; 4. Assign window dimensions and the window handle to the RMpipe.
  ;;(unless (rm-cffi::rm-pipe-set-window (fp (pipe self)) (hwnd self)
  ;;                                     (slot-value self 'width) (slot-value self 'height))
  ;;  (error 'pipe-set-window-error :self (pipe self)))

  ;;(unless (rm-cffi::rm-pipe-set-window-size (fp (pipe self)) 
  ;;                                     (slot-value self 'width) (slot-value self 'height))
  ;;  (error 'pipe-set-window-error :self (pipe self)))

  ;;(rm-cffi::rm-pipe-create-context (fp (pipe self)))
  
  ;; 3.2 Assign an OpenGL context to the RMpipe using rmPipeSetContext.
  ;; SDL creates its own context.
  ;; The following seems to work, but it might not.
  ;;(unless (rm-cffi::rm-pipe-set-context (fp (pipe self))
  ;;                                      ;;(cffi:null-pointer)
  ;;                                      (hwnd self)
  ;;                                      ;;(opengl-context self)
  ;;                              )
  ;;  (error 'set-opengl-context-error :self (pipe self)))

  ;; 5. Make the RMpipe “current” using rmPipeMakeCurrent.
  (pipe-make-current (pipe self))
  
  (when scene
    (dolist (s (if (listp scene) scene (list scene)))
      (add-scene s self)))

  (when (root-node-p self)
    (log5:log-for (info) "WINDOW node set to ROOT-NODE"))

  (when parent
    (unless (root-node-p self)
      (log5:log-for (info) "WINDOW node added to ROOT-NODE")
      (add-to-node parent self)))
  (setf *window* self))

(defmethod add-to-node :around ((parent rm-node) (child window))
  (if (or (equal child parent)
	  (node-eq child parent)
	  (root-node-p child))
    (log5:log-for (info) "ADD-TO-NODE:NODE->CHILD: parent: ~A, child: ~A. SAME NODE, or ROOT-NODE."
                  (name parent) (name child))
    (call-next-method)))

(defmethod add-scene ((scene scene) (window window) &optional (add-node t))
  (labels ((scene-exists? (window scene)
             (assoc (id scene) (scenes window))))
    (unless (scene-exists? window scene)
      (setf (slot-value scene 'parent-window) window
            (scenes window) (acons (id scene) scene (scenes window))
            (slot-value window 'default-scene) scene)
      (add-event-listener window scene)
      (when add-node
        (add-to-node window scene)))))

(defun find-window (index)
  (cdr (assoc index *windows* :test #'cffi:pointer-eq)))

(defgeneric add-window (window))
(defmethod add-window ((self window))
  (setf *windows* (acons (hwnd self) self *windows*))
  self)

(defmethod (setf hwnd) (value (self window))
  (setf (slot-value self 'hwnd) value))

(defmethod close-window :after (window)
  (setf *windows* (remove (rassoc window *windows* :test #'cffi:pointer-eq)
			  *windows*
			  :test #'cffi:pointer-eq)))

(defmethod find-scene ((window window) id)
  (cdr (assoc id (scenes window) :test 'equal)))

;; (defmethod delete-scene ((window window) (scene scene))
;;   (setf (scenes window) (delete scene (scenes window)
;; 				:test #'(lambda (x y) (equal (car x) (car y)))))
;;   (remove-event-listener window scene)
;;   (setf (parent scene) nil)
;;   (unless (node-eq window scene)
;;     (remove-child-node window scene)))

(defun current-window ()
  *current-window*)
(defun (setf current-window) (window-val)
  (setf *current-window* window-val))

(defun default-window ()
  *window*)

(defun default-scene (&optional (window (default-window)))
  (slot-value window 'default-scene))

(defmethod width ((self window))
  (width (pipe self)))
(defmethod height ((self window))
  (height (pipe self)))

(defmethod frame-rate ((self window))
  (frame-rate (pipe self)))

(defmethod (setf frame-rate) (value (self window))
  (if (integerp value)
    (setf (frame-rate (pipe self)) value)
    (setf (frame-rate (pipe self)) -1)))

(defmethod (setf dimensions) ((size vector) (self window))
  (setf (dimensions (pipe self)) size))
(defmethod dimensions ((self window))
  (when (pipe self)
    (dimensions (pipe self))))

(defmethod close-windows ()
  (loop for (hwnd . window) in *windows* do
       (close-window window)))

(defun render ()
  (%render (default-window)))

(defmethod %render ((self window))
  (rm-cffi::rm-frame (fp (pipe self)) (fp self)))

;;;
;;; Events

(defmethod on-paint :after ((self window))
  (%render self))

(defmethod on-idle :after ((self window))
  (%render self))

(defmethod on-resize ((self window) width height)
  "Automatically sets the width and height of the PIPE to the width and height
of the WINDOW."
  (setf (dimensions self) (vector width height))
  (call-next-method))

(defmethod print-object ((obj window) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "\(
:GC-P              ~s
:WIDTH, HEIGHT     ~s
:SCENES            ~s
:DEFAULT-SCENE     ~s
:PIPE              ~s
\)"
            (gc-p obj) (dimensions obj)
            (scenes obj) (default-scene obj) (if (pipe obj) t nil)
            )))
