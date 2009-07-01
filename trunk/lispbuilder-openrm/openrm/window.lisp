
(in-package #:rm)

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
   :node (rm::rm-root-node)
   :create-context nil))

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
                                        scene create-context
                                        pipe)
  (call-next-method)
  
  (add-window self)

  (setf (pipe self) pipe)
  (unless (pipe self)
    (error "ERROR: initialize-instance :around GENERIC-WINDOW: :PIPE must be specified."))
  (setf (dimensions (pipe self))
        (vector (slot-value self 'width) (slot-value self 'height)))
    ;;(setf (pipe self)
    ;;      (make-instance 'sdl-pipe
    ;;                     :dimensions (vector (slot-value self 'width)
    ;;                                         (slot-value self 'height))))
  (when create-context
    (rm-cffi::rmw-pipe-create-context (fp (pipe self))))

  ;;(rm-cffi::rm-pipe-set-window (fp (pipe window)) (hwnd window)
  ;;                             (slot-value window 'width) (slot-value window 'height))
  ;;(rm-cffi::rm-pipe-set-window-size (fp (pipe window))
  ;;                             (slot-value window 'width) (slot-value window 'height))

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

(defun delete-window (index)
  (setf *windows* (delete (assoc index *windows* :test #'cffi:pointer-eq)
			  *windows*
			  :test #'(lambda (x y) (cffi:pointer-eq (car x) (car y))))))

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
  (dimensions (pipe self)))

(defmethod close-window ((self window)) nil)

(defun render ()
  (%render (default-window)))

(defmethod %render ((self window))
  ;;(format t "RENDER: ~A~%" (incf *count*))
  ;;(force-output t)
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
:COPY-P            ~s
:WIDTH, HEIGHT     ~s
:SCENES            ~s
:DEFAULT-SCENE     ~s
:PIPE              ~s
\)"
            (gc-p obj) (copy-p obj) (dimensions obj)
            (scenes obj) (default-scene obj) (if (pipe obj) t nil)
            )))
