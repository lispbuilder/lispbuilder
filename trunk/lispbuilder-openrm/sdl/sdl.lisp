
(in-package #:lispbuilder-openrm)

(defvar *quit* nil)

;;;# Features

;; From CFFI
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (mapc (lambda (feature) (pushnew feature *features*))
;;         '(#+lispbuilder-sdl lispbuilder-openrm:lispbuilder-sdl)))

(defclass sdl-pipe (pipe)()
  (:default-initargs
   :target :RM-PIPE-NOPLATFORM
   :processing-mode :RM-PIPE-MULTISTAGE
   :opaque-3d t
   :transparent-3d t
   :opaque-2d t
   :swap-buffers nil
   :notify-level t))

(defclass sdl-window (window)
  ((surface
    :reader surface
    :initform nil
    :initarg :surface)
   (hwnd
    :initform nil
    :initarg :hwnd)
   (default-display
    :reader default-display))
  (:default-initargs
   :pipe (make-instance 'sdl-pipe)
   :double-buffer t
   :init-video t
   :frame t))

(defclass sdl-pixels* (color-array)()
  (:default-initargs
   :create nil
   :size nil
   :gc nil
   :free-on-delete nil
   :copy-p nil
   :free (simple-free #'cffi:foreign-free 'color-single)))

(defmethod initialize-instance :after ((window sdl-window)
                                       &key surface double-buffer
                                       (bpp 0)
                                       title-caption icon-caption width height
                                       hw-surface accel any-format fullscreen
                                       resizable frame
                                       init-timer init-audio init-video
                                       init-cdrom init-joystick init-noparachute
                                       init-eventthread init-everything
                                       &allow-other-keys)
  (let ((backend-flags nil))
    (when init-everything
      (pushnew SDL:SDL-INIT-EVERYTHING backend-flags))
    (unless init-everything
      (when init-video
        (pushnew SDL:SDL-INIT-VIDEO backend-flags))
      (when init-timer
        (pushnew SDL:SDL-INIT-TIMER backend-flags))
      (when init-audio
        (pushnew SDL:SDL-INIT-AUDIO backend-flags))
      (when init-cdrom
        (pushnew SDL:SDL-INIT-CDROM backend-flags))
      (when init-joystick
        (pushnew SDL:SDL-INIT-JOYSTICK backend-flags)))
    (when init-noparachute
      (pushnew SDL:SDL-INIT-NOPARACHUTE backend-flags))
    (when init-eventthread
      (pushnew SDL:SDL-INIT-EVENTTHREAD backend-flags))
    (sdl:init-sdl :flags backend-flags))

  (unless surface
    (if double-buffer
      (sdl:set-gl-attribute :SDL-GL-DOUBLEBUFFER 1)
      (sdl:set-gl-attribute :SDL-GL-DOUBLEBUFFER 0))
    (let ((window-flags nil))
      ;; Make sure to have SDL create the OpenGL context.
      (pushnew SDL:SDL-OPENGL window-flags)
      ;; And set the rest of the SDL window flags as appropriate.
      (if hw-surface
        (pushnew SDL:SDL-HW-SURFACE window-flags)
        (pushnew SDL:SDL-SW-SURFACE window-flags))
      (when accel
        (pushnew SDL:SDL-HW-ACCEL window-flags))
      (when any-format
        (pushnew sdl:SDL-ANY-FORMAT window-flags))
      (when fullscreen
        (pushnew sdl:SDL-FULLSCREEN window-flags))
      (unless fullscreen
        (when resizable
          (pushnew sdl:SDL-RESIZABLE window-flags))
        (unless frame
          (pushnew sdl:SDL-NO-FRAME window-flags)))
    
      (setf surface (sdl:window width height
                                :bpp bpp
                                :flags window-flags
                                :title-caption title-caption
                                :icon-caption icon-caption))))
  (unless surface
    (error "ERROR: SDL Surface or OpenGL context cannot be created."))

  (setf (slot-value window 'surface) surface
        *quit* nil
        (slot-value window 'default-display) (sdl:create-surface width height :rle-accel nil)
        sdl:*default-display* (slot-value window 'default-display)))

(defmethod close-window ((self sdl-window))
  (log5:log-for (free) "CLOSE-WINDOW: ~A" self)
  (free (pipe self))
  (setf (pipe self) nil)
  (delete-window (hwnd self)))

(defmethod close-windows ()
  (loop for (hwnd . window) in *windows* do
       (close-window window)))

(defmethod hwnd ((self sdl-window))
  (sdl:get-native-window))

(defmethod clean-up :after (&optional (quit-rm nil))
  (sdl-cffi::SDL-Quit))

(defmethod %render :after ((self sdl-window))
  (sdl-cffi::sdl-gl-swap-buffers))

(defun load-3bpp-image (surface copy free-on-delete)
  (declare (ignore copy free-on-delete))
  (sdl:with-surface (surf (sdl:create-surface (sdl:width surface)
                                              (sdl:height surface))
                          t)
    (sdl-base::with-pixel (px (sdl:fp surf))
      ;; Convert the 3bpp surface to a 4bpp surface
      (sdl:draw-surface surface :surface surf)
      (image-mirror (make-instance 'image
                                   :type 2
                                   :dims (vector (sdl:width surf) (sdl:height surf))
                                   :depth 0
                                   :format :rm-image-rgba
                                   :data-type :rm-unsigned-byte
                                   :copy-p t
                                   :free-on-delete nil
                                   :image-data (make-instance 'sdl-pixels*
                                                              :fp (sdl-base::pixel-data px)
                                                              :copy-p t
                                                              :free-on-delete nil))
                    :rm-image-mirror-height))))

(defun load-4bpp-image (surface copy free-on-delete)
  (sdl-base::with-pixel (px (sdl:fp surface))
    (image-mirror (make-instance 'image
                                 :type 2
                                 :dims (vector (sdl:width surface) (sdl:height surface))
                                 :depth 0
                                 :format :rm-image-rgba
                                 :data-type :rm-unsigned-byte
                                 :copy-p copy
                                 :free-on-delete free-on-delete
                                 :image-data (make-instance 'sdl-pixels*
                                                            :fp (sdl-base::pixel-data px)
                                                            :copy-p copy
                                                            :free-on-delete free-on-delete))
                  :rm-image-mirror-height)))

(defmethod load-image (filename &key (copy t) (free-on-delete nil))
  (declare (ignore copy free-on-delete))
  (sdl:with-surface (temp (sdl:load-image filename) t)
    (load-image temp :copy t :free-on-delete nil)))

(defmethod load-image ((surface SDL:SDL-SURFACE) &key (copy nil) (free-on-delete nil))
  (sdl-base::with-pixel (px (sdl:fp surface))
    (cond
     ((eq 3 (sdl-base::pixel-bpp px))
      (load-3bpp-image surface t nil))
     ((eq 4 (sdl-base::pixel-bpp px))
      (load-4bpp-image surface copy free-on-delete))
     (t (error "LOAD-IMAGE: The image must be 3bpp or 4bpp.")))))

(cffi:defcallback sdl-surface-proc :pointer
    ((data-fp :pointer))
  "Called when a surface is no longer used by the scene graph."
  (declare (ignore data-fp))
  (log5:log-for (info) "DEFCALLBACK:SDL-SURFACE")
  ;(cffi:foreign-free data-fp)
  (cffi:null-pointer))

;;;;;
;;;;; Event Handling

;;; Event Handling from here   -----------------------


(defun event? (sdl-event event-type &optional event-type-end)
  (if event-type-end
    (if (and (>= (cffi:foreign-slot-value sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type)
                 (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type event-type))
             (< (cffi:foreign-slot-value sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type)
                (- (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type event-type-end) 1)))
      t
      nil)
    (if (eql (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type event-type)
             (cffi:foreign-slot-value sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type))
      t
      nil)))

(defun expand-activeevent (sdl-event window)
  (handle-on-active ;; window
                    window
                    ;; gain
                    (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Active-Event 'sdl-cffi::gain)
                    ;; state
                    (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Active-Event 'sdl-cffi::state)))

(defun expand-keydown (sdl-event window)
  (handle-on-key-down ;; window
                      window
                      ;; state
                      (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Keyboard-Event 'sdl-cffi::state)
                      ;; scancode
                      (cffi:foreign-slot-value (cffi:foreign-slot-pointer sdl-event
                                                                          'sdl-cffi::sdl-keyboard-event
                                                                          'sdl-cffi::keysym)
                                               'sdl-cffi::sdl-key-sym 'sdl-cffi::scancode)
                      ;; key
                      (cffi:foreign-slot-value (cffi:foreign-slot-pointer sdl-event
                                                                          'sdl-cffi::sdl-keyboard-event
                                                                          'sdl-cffi::keysym)
                                               'sdl-cffi::sdl-key-sym 'sdl-cffi::sym)
                      ;; mod
                      (cffi:foreign-slot-value (cffi:foreign-slot-pointer sdl-event
                                                                          'sdl-cffi::sdl-keyboard-event
                                                                          'sdl-cffi::keysym)
                                               'sdl-cffi::sdl-key-sym 'sdl-cffi::mod)
                      ;; unicode
                      (cffi:foreign-slot-value (cffi:foreign-slot-pointer sdl-event
                                                                          'sdl-cffi::sdl-keyboard-event
                                                                          'sdl-cffi::keysym)
                                               'sdl-cffi::sdl-key-sym 'sdl-cffi::unicode)))

(defun expand-keyup (sdl-event window)
  (handle-on-key-up
   ;; window
   window
   ;; state
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Keyboard-Event 'sdl-cffi::state)
   ;; scancode
   (cffi:foreign-slot-value (cffi:foreign-slot-pointer sdl-event
						       'sdl-cffi::sdl-keyboard-event
						       'sdl-cffi::keysym)
			    'sdl-cffi::sdl-key-sym 'sdl-cffi::scancode)
   ;; key
   (cffi:foreign-slot-value (cffi:foreign-slot-pointer sdl-event
						       'sdl-cffi::sdl-keyboard-event
						       'sdl-cffi::keysym)
			    'sdl-cffi::sdl-key-sym 'sdl-cffi::sym)
   ;; mod
   (cffi:foreign-slot-value (cffi:foreign-slot-pointer sdl-event
						       'sdl-cffi::sdl-keyboard-event
						       'sdl-cffi::keysym)
			    'sdl-cffi::sdl-key-sym 'sdl-cffi::mod)
   ;; unicode
   (cffi:foreign-slot-value (cffi:foreign-slot-pointer sdl-event
						       'sdl-cffi::sdl-keyboard-event
						       'sdl-cffi::keysym)
			    'sdl-cffi::sdl-key-sym 'sdl-cffi::unicode)))

(defun expand-mousemotion (sdl-event window)
  (handle-on-mouse-move
   ;; window
   window
   ;; :state
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::SDL-Mouse-Motion-Event 'sdl-cffi::state)
   ;; x
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::SDL-Mouse-Motion-Event 'sdl-cffi::x)
   ;; y
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::SDL-Mouse-Motion-Event 'sdl-cffi::y)
   ;; x-rel
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::SDL-Mouse-Motion-Event 'sdl-cffi::xrel)
   ;; y-rel
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::SDL-Mouse-Motion-Event 'sdl-cffi::yrel)))

(defun expand-mousebuttondown (sdl-event window)
  (handle-on-mouse-down
   ;; window
   window
   ;; button
   (translate-mouse-button (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::button))
   ;; x
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::x)
   ;; y
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::y)))

(defun expand-mousebuttonup (sdl-event window)
  (handle-on-mouse-up
   ;; window
   window
   ;; button
   (translate-mouse-button (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::button))
   ;; x
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::x)
   ;; y
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::y)))

(defun expand-joyaxismotion (sdl-event window)
  (handle-on-joy-axis-motion
   ;; window
   window
   ;; which
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Axis-Event 'sdl-cffi::which)
   ;; axis
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Axis-Event 'sdl-cffi::axis)
   ;; value
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Axis-Event 'sdl-cffi::value)))

(defun expand-joybuttondown (sdl-event window)
  (handle-on-joy-button-down
   ;; window
   window
   ;; which
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Button-Event 'sdl-cffi::which)
   ;; button
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Button-Event 'sdl-cffi::button)
   ;; state
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Button-Event 'sdl-cffi::state)))

(defun expand-joybuttonup (sdl-event window)
  (handle-on-joy-button-up
   ;; window
   window
   ;; which
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Button-Event 'sdl-cffi::which)
   ;; button
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Button-Event 'sdl-cffi::button)
   ;; state
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Button-Event 'sdl-cffi::state)))

(defun expand-joyhatmotion (sdl-event window)
  (handle-on-joy-hat-motion
   ;; window
   window
   ;; which
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Hat-Event 'sdl-cffi::which)
   ;; axis
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Hat-Event 'sdl-cffi::hat)
   ;; value
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Hat-Event 'sdl-cffi::value)))

(defun expand-joyballmotion (sdl-event window)
  (handle-on-joy-ball-motion
   ;; window
   window
   ;; which
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Ball-Event 'sdl-cffi::which)
   ;; ball
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Ball-Event 'sdl-cffi::ball)
   ;; x-rel
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Ball-Event 'sdl-cffi::xrel)
   ;; y-rel
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Ball-Event 'sdl-cffi::yrel)))

(defun expand-videoresize (sdl-event window)
  (handle-on-resize
   ;; window
   window
   ;; width
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Resize-Event 'sdl-cffi::w)
   ;; height
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Resize-Event 'sdl-cffi::h)))

(defun expand-videoexpose (sdl-event window)
  (declare (ignore sdl-event))
  (handle-on-expose
   ;; window
   window))

(defun expand-syswmevent (sdl-event window)
  (declare (ignore sdl-event))
  (handle-on-wm-event
   ;; window
   window))

(defun expand-quit (sdl-event window)
  (declare (ignore sdl-event))
  (handle-on-quit
   ;; window
   window))

(defun expand-userevent (sdl-event window)
  (handle-on-user
   ;; window
   window
   ;; type
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-User-Event 'sdl-cffi::type)
   ;; code
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-User-Event 'sdl-cffi::code)
   ;; data1
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-User-Event 'sdl-cffi::data1)
   ;; data2
   (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-User-Event 'sdl-cffi::data2)))

(defun expand-idle (sdl-event window)
  (declare (ignore sdl-event))
  (handle-on-idle
   ;; window
   window))

(defun event-types (sdl-event window)
  (cond
    ;; active-event
    ((event? sdl-event :SDL-ACTIVE-EVENT)
     (expand-activeevent sdl-event window))  
    ;; key-down-event
    ((event? sdl-event :SDL-KEY-DOWN-EVENT)
     (expand-keydown sdl-event window))
    ;; key-up-event
    ((event? sdl-event :SDL-KEY-UP-EVENT)
     (expand-keyup sdl-event window))
    ;; mouse-motion-event
    ((event? sdl-event :SDL-MOUSE-MOTION-EVENT)
     (expand-mousemotion sdl-event window))
    ;; mouse-button-down-event
    ((event? sdl-event :SDL-MOUSE-BUTTON-DOWN-EVENT)
     (expand-mousebuttondown sdl-event window))
    ;; mouse-button-up-event
    ((event? sdl-event :SDL-MOUSE-BUTTON-UP-EVENT)
     (expand-mousebuttonup sdl-event window))
    ;; joy-axis-motion-event
    ((event? sdl-event :SDL-JOY-AXIS-MOTION-EVENT)
     (expand-joyaxismotion sdl-event window))
    ;; joy-button-down-event
    ((event? sdl-event :SDL-JOY-BUTTON-DOWN-EVENT)
     (expand-joybuttondown sdl-event window))
    ;; joy-button-up-event
    ((event? sdl-event :SDL-JOY-BUTTON-UP-EVENT)
     (expand-joybuttonup sdl-event window))
    ;; joy-hat-motion-event
    ((event? sdl-event :SDL-JOY-HAT-MOTION-EVENT)
     (expand-joyhatmotion sdl-event window))
    ;; joy-ball-motion-event
    ((event? sdl-event :SDL-JOY-BALL-MOTION-EVENT)
     (expand-joyballmotion sdl-event window))
    ;; video-resize-event
    ((event? sdl-event :SDL-VIDEO-RESIZE-EVENT)
     (expand-videoresize sdl-event window))
    ;; video-expose-event
    ((event? sdl-event :SDL-VIDEO-EXPOSE-EVENT)
     (expand-videoexpose sdl-event window))
    ;; sys-wm-event
    ((event? sdl-event :SDL-SYS-WM-EVENT)
     (expand-syswmevent sdl-event window))
    ;; quit-event
    ((event? sdl-event :SDL-QUIT-EVENT)
     (expand-quit sdl-event window))
    ;; user-event
    ((event? sdl-event :SDL-USER-EVENT :SDL-NUM-EVENTS)
     (expand-userevent sdl-event window))))

(defmethod process-events-wait ((window sdl-window))
  (let ((sdl-event (sdl:new-event))
        (idle-func #'(lambda () (on-idle window))))
    (loop until *quit* do
	 (loop until (or *quit* (eq (sdl-cffi::SDL-Wait-Event sdl-event) 0)) do
	      (event-types sdl-event window))
	 (unless *quit*
           (sdl-base::process-timestep sdl-base::*default-fpsmanager*
                                       idle-func)))))

(defmethod process-events-poll ((window sdl-window))
  (let ((sdl-event (sdl:new-event))
        (idle-func #'(lambda () (on-idle window))))
    (loop until *quit* do
	 (loop until (or *quit* (eq (sdl-cffi::SDL-Poll-Event sdl-event) 0)) do
	      (event-types sdl-event window))
	 (unless *quit*
	   (sdl-base::process-timestep sdl-base::*default-fpsmanager*
                                       idle-func)))))

;;;;;
;;;;; Events

;;(defmethod on-paint ((window sdl-window))
;;  (render window))

(defmethod on-quit ((window sdl-window))
  (setf *quit* t))

;;(defmethod on-idle ((window sdl-window))
;;  (render window))

;;;;; Event Conversion
;;;;;

(defmethod translate-mouse-button (button)
  (cond
    ((eq button sdl-cffi::SDL-BUTTON-LEFT)
     :BUTTON-LEFT)
    ((eq button sdl-cffi::SDL-BUTTON-MIDDLE)
     :BUTTON-MIDDLE)
    ((eq button sdl-cffi::SDL-BUTTON-RIGHT)
     :BUTTON-RIGHT)
    ((eq button sdl-cffi::SDL-BUTTON-WHEEL-UP)
     :WHEEL-UP)
    ((eq button sdl-cffi::SDL-BUTTON-WHEEL-DOWN)
     :WHEEL-DOWN)))

(defmethod button= (state &optional (button nil))
  (if (eq state button)
      button
      (if (keywordp state)
	  state
	  nil)))

(defmethod button= ((state integer) &optional (button nil))
  (if (keywordp button)
      (case button
	(:BUTTON-LEFT 
	 (when (> (logand (sdl-cffi::SDL-BUTTON sdl-cffi::SDL-BUTTON-LEFT)
			  state)
		  0)
	   :BUTTON-LEFT))
	(:BUTTON-MIDDLE
	 (when (> (logand (sdl-cffi::SDL-BUTTON sdl-cffi::SDL-BUTTON-MIDDLE)
			  state)
		  0)
	   :BUTTON-MIDDLE))
	(:BUTTON-RIGHT
	 (when (> (logand (sdl-cffi::SDL-BUTTON sdl-cffi::SDL-BUTTON-RIGHT)
			  state)
		  0)
	   :BUTTON-RIGHT))
	(:WHEEL-UP
	 (when (> (logand (sdl-cffi::SDL-BUTTON sdl-cffi::SDL-BUTTON-WHEEL-UP)
			  state)
		  0)
	   :WHEEL-UP))
	(:WHEEL-DOWN
	 (when (> (logand (sdl-cffi::SDL-BUTTON sdl-cffi::SDL-BUTTON-WHEEL-DOWN)
			  state)
		  0)
	   :WHEEL-DOWN)))
      nil))

;; (defun translate-sdl-key (key-code)
;;   (cond
;;     ((<= key-code 127)
;;      (code-char key-code))
;;     ))

