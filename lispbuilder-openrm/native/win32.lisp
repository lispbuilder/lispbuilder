(in-package #:lispbuilder-openrm)

(defvar *window-proc-hook* nil)

(define-condition opengl-context-not-created-error (error)
  ((self :initarg :self :reader self)))

(define-condition cannot-make-opengl-context-current-error (error)
  ((self :initarg :self :reader self)))

(define-condition cannot-retrieve-device-context (error)
  ((self :initarg :self :reader self)))

(defclass native-pipe (pipe)()
  (:default-initargs
   :target
   :pipe-noplatform ;;:PIPE-WGL
   :processing-mode :PIPE-MULTISTAGE
   :opaque-3d t
   :transparent-3d t
   :opaque-2d t))

(defclass native-window (window)
  ((hwnd
    :reader hwnd
    :initform nil
    :initarg :hwnd)
   (dc
    :reader dc
    :initform nil)
   (hrc
    :reader hrc
    :initform nil)
   (timer
    :accessor timer
    :initform nil
    :initarg :timer)
   (timer-function
    :accessor timer-function
    :initform nil
    :initarg :timer-function))
  (:default-initargs
   :pipe (make-instance 'native-pipe)))

(defun create-win32-window (x y width height)
  (let ((class-name (symbol-name (gensym "lisp-window-class-")))
	(hInstance (lispbuilder-windows:GetModuleHandle (cffi:null-pointer))))
    (cffi:with-foreign-object (class 'lispbuilder-windows::WNDCLASS)
      (zero-mem class lispbuilder-windows:WNDCLASS)
      (cffi:with-foreign-slots ((lispbuilder-windows:style
                                 lispbuilder-windows:lpfnwndproc
                                 lispbuilder-windows:hinstance
                                 lispbuilder-windows:hcursor
                                 lispbuilder-windows:lpszclassname) class lispbuilder-windows::WNDCLASS)
        (setf lispbuilder-windows:style (logior lispbuilder-windows:CS_HREDRAW lispbuilder-windows:CS_VREDRAW))
        (setf lispbuilder-windows:lpfnwndproc (cffi:callback window-proc))
        (setf lispbuilder-windows:hinstance hInstance)
        (setf lispbuilder-windows:hcursor (lispbuilder-windows:LoadCursor (cffi:null-pointer) lispbuilder-windows:IDC_ARROW))
        (setf lispbuilder-windows:lpszclassname class-name))
      (lispbuilder-windows:registerclass class))

    (lispbuilder-windows:CreateWindowEx 0 class-name "OpenRM"
                                        (logand (logior lispbuilder-windows:WS_VISIBLE
                                                        lispbuilder-windows:WS_OVERLAPPEDWINDOW
                                                        lispbuilder-windows:WS_CLIPCHILDREN
                                                        lispbuilder-windows:WS_CLIPSIBLINGS
                                                        lispbuilder-windows:WS_SIZEBOX)
                                                (lognot lispbuilder-windows:WS_MAXIMIZEBOX)
                                                (lognot lispbuilder-windows:WS_MINIMIZEBOX))
                                        x y width height
                                        (cffi:null-pointer) (cffi:null-pointer)
                                        hInstance
                                        (cffi:null-pointer))))

(defun set-win32-pixel-format (window)
  (let ((hdc (lispbuilder-windows::get-dc (hwnd window))))
    (when (cffi:null-pointer-p hdc)
      (error 'cannot-retrieve-device-context :self window))
    (cffi:with-foreign-object (p-f-d 'lispbuilder-windows:pixelformatdescriptor)
      (zero-mem p-f-d lispbuilder-windows:pixelformatdescriptor)
      (cffi:with-foreign-slots ((lispbuilder-windows::nSize
                                 lispbuilder-windows::nVersion
                                 lispbuilder-windows::dwFlags
                                 lispbuilder-windows::iPixelType
                                 lispbuilder-windows::cColorBits
                                 lispbuilder-windows::cDepthBits
                                 lispbuilder-windows::iLayerType)
                                p-f-d lispbuilder-windows:pixelformatdescriptor)
        (setf lispbuilder-windows::nSize (cffi:foreign-type-size 'lispbuilder-windows:pixelformatdescriptor)
              lispbuilder-windows::nVersion 1
              lispbuilder-windows::dwFlags (logior lispbuilder-windows:PFD_DRAW_TO_WINDOW
                                                   lispbuilder-windows:PFD_SUPPORT_OPENGL
                                                   lispbuilder-windows:PFD_DOUBLEBUFFER)
              lispbuilder-windows::iPixelType lispbuilder-windows:PFD_TYPE_RGBA
              lispbuilder-windows::cColorBits 24
              lispbuilder-windows::cDepthBits 16
              lispbuilder-windows::iLayerType lispbuilder-windows:PFD_MAIN_PLANE))
      (let ((iFormat (lispbuilder-windows:Choose-Pixel-Format hdc p-f-d)))
        (lispbuilder-windows:set-pixel-format hdc iFormat p-f-d)))
    hdc))

(defmethod initialize-instance :after ((window native-window)
				       &key (x 0) (y 0))

  (setf (hwnd window) (create-win32-window x y (slot-value window 'width) (slot-value window 'height)))
  (setf (slot-value window 'dc) (set-win32-pixel-format window))
  
  (when (eq :PIPE-NOPLATFORM (target-platform (pipe window)))
    ;; These steps are mandatory when :PIPE-NOPLATFORM is set!
    (let ((HGLRC (rm-cffi::wgl-Create-Context (dc window))))
      (when (cffi:null-pointer-p hglrc)
        (error 'opengl-context-not-created-error :self window))

      (unless (rm-cffi::wgl-make-current (dc window) HGLRC)
        (error 'cannot-make-opengl-context-current-error :self window))

      (unless (rm-cffi::rm-pipe-set-context (fp (pipe window)) hglrc)
        (error 'set-opengl-context-error :self (pipe window)))

      (setf (slot-value window 'hrc) HGLRC)))

  ;; Note that for win32, rm-pipe-set-window/rm-pipe-set-window-size has to be called before
  ;; the OpenGL context is created. For Unix this is reversed.
  ;; But only in the case of :PIPE-WGL and :PIPE-GLX. This is not the case for :PIPE-NOPLATFORM.      
  (when (eq :PIPE-NOPLATFORM (target-platform (pipe window)))
    (if (text-support-p (pipe window))
      (unless (rm-cffi::rm-pipe-set-window (fp (pipe window)) (hwnd window) (slot-value window 'width) (slot-value window 'height))
        (error 'pipe-set-window-error :self (pipe window)))
      (unless (rm-cffi::rm-pipe-set-window-size (fp (pipe window)) (slot-value window 'width) (slot-value window 'height))
        (error 'pipe-set-window-error :self (pipe window)))))

  (when (eq :PIPE-WGL (target-platform (pipe window)))
    (unless (rm-cffi::rm-pipe-set-window (fp (pipe window)) (hwnd window) (slot-value window 'width) (slot-value window 'height))
      (error 'pipe-set-window-error :self (pipe window))))
      
  (when (eq :PIPE-WGL (target-platform (pipe window)))
    ;; These steps are mandatory when :PIPE-WGL is set!
    (rm-cffi::rm-pipe-create-context (fp (pipe window)))))


        ;;          (pipe (cffi:foreign-slot-value (cffi:make-pointer lparam) 'lispbuilder-windows:CREATESTRUCT
        ;;                                         'lispbuilder-windows::lpCreateParams)))
        ;;      ;;(setf (cffi:foreign-slot-value pipe 'rm-cffi::rm-pipe 'rm-cffi::hdc) hdc
        ;;      ;;      (cffi:foreign-slot-value pipe 'rm-cffi::rm-pipe 'rm-cffi::hrc) HGLRC)
        ;;      (setf (cffi:foreign-slot-value pipe 'rm-cffi::rm-pipe 'rm-cffi::hdc) hdc
        ;;            (cffi:foreign-slot-value pipe 'rm-cffi::rm-pipe 'rm-cffi::h-rc) HGLRC)))))
    


(defun create-pipe-from-lparam (lparam)
  (make-instance 'native-pipe :fp (cffi:foreign-slot-value (cffi:make-pointer lparam) 'lispbuilder-windows:CREATESTRUCT
                                                           'lispbuilder-windows::lpCreateParams)))

(defmethod close-window ((self native-window))
  (log5:log-for (free) "CLOSE-WINDOW: ~A" self)
  ;; Can't delete the OpenGL context here, as it is possible for
  ;; the event loop to continue until WM_DESTROY is received
  ;; Pipe and context deletion are in %on-destroy

  ;; Make the current rendering context not current
  (rm-cffi::wgl-Make-Current (cffi:null-pointer) (cffi:null-pointer))
  ;; Make sure the context has been set using rm-pipe-set-context, or else this won't work.
  (rm-cffi::wgl-Delete-Context ;;(cffi:foreign-slot-value (fp (pipe self)) 'rm-cffi::rm-pipe 'rm-cffi::h-rc)
                               (hrc self))
  (lispbuilder-windows::Release-dc (hwnd self) (dc self))

  (free (pipe self))
  (setf (pipe self) nil))

(defmacro when-window-found ((var hWnd) &body body)
  `(let ((,var (find-window ,hWnd)))
     (when ,var
       ,@body)))

;;; Default win32 events
(defun %on-destroy (hWnd wParam lParam)
  (declare (ignore hwnd wParam lParam))
  (lispbuilder-windows:PostQuitMessage 0)
  t)

(defun %on-paint (hWnd wParam lParam)
  (declare (ignore wParam lParam))
  (when-window-found (window hWnd)
    (dispatch-event window on-paint)
    ;; Dont' think we need to swap the buffers here.
    ;;(if (eq :PIPE-NOPLATFORM (target-platform (pipe window)))
    ;;  (rm-cffi::wgl-swap-buffers (dc window))
    ;;  (rm-cffi::rm-Pipe-Swap-Buffers-Win-32 (fp (pipe window))))
    (lispbuilder-windows:Validate-Rect hWnd (cffi:null-pointer))))

(defun %on-close (hWnd wParam lParam)
  (declare (ignore wParam lParam))
  (lispbuilder-windows:destroy-window hwnd)
  t)

(defun %on-create (hWnd wParam lParam)
  (declare (ignore wParam lParam))
  (let ((window-width nil) (window-height nil))
    (cffi:with-foreign-object (window-rect 'lispbuilder-windows:RECT)
      (lispbuilder-windows:Get-Window-Rect hWnd window-rect)
      (setf window-width (cffi:foreign-slot-value window-rect 'lispbuilder-windows:RECT 'lispbuilder-windows:right)
	    window-height (cffi:foreign-slot-value window-rect 'lispbuilder-windows:RECT 'lispbuilder-windows:bottom))
      (cffi:with-foreign-object (rect 'lispbuilder-windows:RECT)
	(lispbuilder-windows:GetClientRect hWnd rect)
	(let* ((width (cffi:foreign-slot-value rect 'lispbuilder-windows:RECT 'lispbuilder-windows:right))
	       (height (cffi:foreign-slot-value rect 'lispbuilder-windows:RECT 'lispbuilder-windows:bottom))
	       (x0 (floor (- (lispbuilder-windows:GetSystemMetrics lispbuilder-windows:SM_CXSCREEN) window-width) 2))
	       (y0 (floor (- (lispbuilder-windows:GetSystemMetrics lispbuilder-windows:SM_CYSCREEN) window-height) 2))
	       (dx (- window-width width))
	       (dy (- window-height height)))
	  (lispbuilder-windows:MoveWindow hWnd
					  (+ x0 (cffi:foreign-slot-value rect 'lispbuilder-windows:RECT 'lispbuilder-windows:left))
					  (+ y0 (cffi:foreign-slot-value rect 'lispbuilder-windows:RECT 'lispbuilder-windows:top))
					  (+ window-width dx)
					  (+ window-height dy)
					  0)))))
  t)

(defun %on-mouse-move (hwnd button-state x y wparam)
  (declare (ignore wparam))
  (when-window-found (window hwnd)
    (handle-on-mouse-move window button-state x y 0 0)
    t))

(defun %on-mouse-down (hwnd button x y wparam)
  (declare (ignore wparam))
  (when-window-found (window hwnd)
    (handle-on-mouse-down window button x y)
    t))

(defun %on-mouse-up (hwnd button x y wparam)
  (declare (ignore wparam))
  (when-window-found (window hwnd)
    (handle-on-mouse-up window button x y)
    t))

(defun %on-resize (hwnd width height wparam)
  (declare (ignore wparam))
  (when-window-found (window hWnd)
    (handle-on-resize window width height)
    ;; (lispbuilder-windows:Validate-Rect hWnd (cffi:null-pointer))
    t))

(defun %on-idle ()
  ;; if there are no events for any windows on this thread then
  ;; call the idle event for each window.
  (force-output t)
  (loop for (hwnd . window) in *windows* do
       (handle-on-idle window)))

(defmethod %render :after ((self native-window))
  ;; Going to use the windows supplied OpenGL swap buffers function,
  ;; instead of the one provided by OpenRM. Seems to make sense if
  ;; NO-PLATFORM is used.
  (if (eq :PIPE-NOPLATFORM (target-platform (pipe self)))
    (rm-cffi::wgl-swap-buffers (dc self))
    (rm-cffi::rm-Pipe-Swap-Buffers-Win-32 (fp (pipe self))))
  (lispbuilder-windows:Validate-Rect (hWnd self) (cffi:null-pointer)))

(defun default-window-proc (hwnd message wparam lparam)
  (cond
   ((= message lispbuilder-windows:wm_create)
    (%on-create hWnd wParam lParam)
    t)
   ((or (= message lispbuilder-windows:WM_ERASEBKGND)
        (= message lispbuilder-windows:WM_PAINT))
    (%on-paint hWnd wParam lParam))
   ((= message lispbuilder-windows:WM_SHOWWINDOW)
    (%on-paint hWnd wParam lParam))
   ((= message lispbuilder-windows:WM_CLOSE)
    (%on-close hWnd wParam lParam))
   ((= message lispbuilder-windows:WM_DESTROY)
    (%on-destroy hWnd wParam lParam))
   ((= message lispbuilder-windows:WM_SIZE)
    (%on-resize hwnd
                (logand lParam #xffff)
                (logand (ash lParam -16) #xffff) wParam))
   ;;   ((= message WM_COMMAND) (on-command hWnd wParam lParam))
   ((= message lispbuilder-windows:WM_TIMER)
    (format t "%on-timer~%")
    (force-output t)
    (when-window-found (window hWnd)
      (when (timer window)
        (when (eq nil (funcall (timer-function window) window))
          (lispbuilder-windows:Destroy-Window hWnd)
          t))))
   ((= message lispbuilder-windows:WM_MOUSEMOVE)
    (%on-mouse-move hwnd
                    wParam;; (convert-from-win32-codes wParam)
                    (logand lParam #xffff)
                    (logand (ash lParam -16) #xffff)
                    wparam))
   ((or (= message lispbuilder-windows:WM_LBUTTONDOWN)
        (= message lispbuilder-windows:WM_RBUTTONDOWN)
        (= message lispbuilder-windows:WM_MBUTTONDOWN))
    (%on-mouse-down hwnd
                    wParam;; (convert-from-win32-codes wParam)
                    (logand lParam #xffff)
                    (logand (ash lParam -16) #xffff)
                    wparam))
   ((or (= message lispbuilder-windows:WM_LBUTTONUP)
        (= message lispbuilder-windows:WM_RBUTTONUP)
        (= message lispbuilder-windows:WM_MBUTTONUP))
    (%on-mouse-up hwnd
                  wParam;; (convert-from-win32-codes wParam)
                  (logand lParam #xffff)
                  (logand (ash lParam -16) #xffff)
                  wparam))
   (t nil)))

(cffi:defcallback window-proc :long
    ((hWnd :pointer)
     (message :unsigned-int)
     (wParam :unsigned-int)
     (lParam :long))
  (if (funcall (if *window-proc-hook* *window-proc-hook* #'default-window-proc)
               hwnd message wparam lparam)
    0
    (return-from window-proc (lispbuilder-windows:DefWindowProc hWnd message wParam lParam))))

;; (cffi:defcallback window-proc :long
;;     ((hWnd :pointer)
;;      (message :unsigned-int)
;;      (wParam :unsigned-int)
;;      (lParam :long))
;;   (let ((window (find-window hWnd)))
;;     (cond
;;       ((= message lispbuilder-windows:WM_CREATE) (%on-create hWnd wParam lParam))
;;       ((or (= message lispbuilder-windows:WM_ERASEBKGND)
;; 	   (= message lispbuilder-windows:WM_PAINT)) (%on-paint hWnd wParam lParam)
;;        0)
;;       ((= message lispbuilder-windows:WM_SHOWWINDOW) (%on-paint hWnd wParam lParam)
;;        0)
;;       ((= message lispbuilder-windows:WM_DESTROY) (%on-destroy hWnd wParam lParam))
;;       ;;   ((= message WM_COMMAND) (on-command hWnd wParam lParam))
;;       ((= message lispbuilder-windows:WM_TIMER)
;;        (when (timer window)
;; 	 (when (eq nil (funcall (timer-function window) window))
;; 	   (lispbuilder-windows:Destroy-Window hWnd))))
;;       ((= message lispbuilder-windows:WM_MOUSEMOVE)
;;        (when (aux-handlers window)
;; 	 (funcall (aux-handlers window)
;; 		  window
;; 		  (node (find-scene window "default-scene"))
;; 		  'move
;; 		  (convert-from-win32-codes wParam)
;; 		  (logand lParam #xffff) (logand (ash lParam -16) #xffff)))
;;        (when (mouse-move-function window)
;; 	 (funcall (mouse-move-function window)
;; 		  window wparam (logand lParam #xffff) (logand (ash lParam -16) #xffff)
;; 		  (mouse-x-prev window) (mouse-y-prev window)
;; 		  (convert-from-win32-codes wParam)))
;;        (when (or (aux-handlers window) (mouse-move-function window))
;; 	 (setf (mouse-x-prev window) (logand lParam #xffff)
;; 	       (mouse-y-prev window) (logand (ash lParam -16) #xffff))	 
;; 	 (render window)
;; 	 0))
;;       ((or (= message lispbuilder-windows:WM_LBUTTONDOWN)
;; 	   (= message lispbuilder-windows:WM_RBUTTONDOWN)
;; 	   (= message lispbuilder-windows:WM_MBUTTONDOWN))
;;        (when (aux-handlers window)
;; 	 (funcall (aux-handlers window)
;; 		  window
;; 		  (node (find-scene window "default-scene"))
;; 		  'click
;; 		  (convert-from-win32-codes wParam)
;; 		  (logand lParam #xffff) (logand (ash lParam -16) #xffff)))
;;        (when (mouse-down-function window)
;; 	 (funcall (mouse-down-function window)
;; 		  window (logand lParam #xffff) (logand (ash lParam -16) #xffff)
;; 		  (convert-from-win32-codes wParam))
;; 	 0))
;;       ((or (= message lispbuilder-windows:WM_LBUTTONUP)
;; 	   (= message lispbuilder-windows:WM_RBUTTONUP)
;; 	   (= message lispbuilder-windows:WM_MBUTTONUP))
;;        (when (mouse-up-function window)
;; 	 (funcall (mouse-up-function window)
;; 		  window (logand lParam #xffff) (logand (ash lParam -16) #xffff)
;; 		  (convert-from-win32-codes wParam))
;; 	 0))
;;       (t (return-from window-proc (lispbuilder-windows:DefWindowProc hWnd message wParam lParam))))
;;     0))
  
(defmethod install-timer ((self native-window) interval function)
  (when (timer self)
    (lispbuilder-windows:KillTimer (hwnd self) (timer self))
    (setf (timer self) nil
	  (timer-function self) nil))
  (let ((timer (1+ (random 100))))
    (setf (timer self) timer
	  (timer-function self) function)
    (lispbuilder-windows:SetTimer (hwnd self) (timer self) interval (cffi:null-pointer)))
  self)

(defmethod clear-timer ((self native-window))
  (when (timer self)
    (lispbuilder-windows:KillTimer (hwnd self) (timer self))
    (setf (timer self) nil
	  (timer-function self) nil))
  self)

(defmethod update-window ((self native-window))
  (lispbuilder-windows:UpdateWindow (hwnd self)))

(defmethod show-window ((self native-window))
  (lispbuilder-windows:showwindow (hwnd self) lispbuilder-windows:SW_SHOW))

(defun pump-events (window msg)
  (let ((quit nil))
    (loop until (or quit
                    (zerop (lispbuilder-windows::PeekMessage msg window 0 0 lispbuilder-windows::PM_NOREMOVE)))
          do (if (zerop (lispbuilder-windows:GetMessage msg window 0 0))
               (setf quit t) ; WM_QUIT received.
               (progn
                 (lispbuilder-windows:TranslateMessage msg)
                 (lispbuilder-windows:DispatchMessage msg))))
    quit))

(defmethod process-events-poll ((window native-window))
  (declare (ignore windows))
  (let ((hwnd-window (cffi:null-pointer)))
    (cffi:with-foreign-object (msg 'lispbuilder-windows:MSG)
      (loop until (pump-events hwnd-window msg) do
	   (%on-idle)))))

(defmethod process-events-wait ((window native-window))
  (declare (ignore window))
  (let ((hwnd-window (cffi:null-pointer)))
    (cffi:with-foreign-object (msg 'lispbuilder-windows:MSG)
      (loop until (zerop (lispbuilder-windows:GetMessage msg hwnd-window 0 0))
	   do (progn
		(lispbuilder-windows:TranslateMessage msg)
		(lispbuilder-windows:DispatchMessage msg))))))

;;(defmethod process-events-poll ((window native-window))
;;  (declare (ignore window))
;;  (let ((null-pointer (cffi:null-pointer)))
;;    (cffi:with-foreign-object (msg 'lispbuilder-windows:msg)
;;      (loop until nil do
;;           (progn
;;              (format t "peekmessage: ~A~%" (lispbuilder-windows::PeekMessage msg null-pointer 0 0 lispbuilder-windows::PM_NOREMOVE))
;;              (force-output t))
;;           (if (> 0 (lispbuilder-windows::PeekMessage msg null-pointer 0 0 lispbuilder-windows::PM_REMOVE))
;;              (if (= lispbuilder-windows:WM_QUIT
;;                     (cffi:foreign-slot-value msg 'lispbuilder-windows:msg 'lispbuilder-windows::message))
;;                (progn
;;                  (format t "loop finish~%")
;;                  (force-output t)
;;                  (loop-finish))
;;                (progn
;;                  (format t "translate~%")
;;                  (force-output t)
;;                  (lispbuilder-windows:TranslateMessage msg)
;;                  (lispbuilder-windows:DispatchMessage msg)))
;;                (%on-idle))))))

(defun convert-from-win32-codes (value)
  (let ((winval #x0))
    ;; Convert mouse buttons
    (when (> (logand value lispbuilder-windows:MK_LBUTTON) 0)
      (setf winval (logior value +mouse-left-button+)))
    (when (> (logand value lispbuilder-windows:MK_MBUTTON) 0)
      (setf winval (logior value +mouse-middle-button+)))
    (when (> (logand value lispbuilder-windows:MK_RBUTTON) 0)
      (setf winval (logior value +mouse-right-button+)))
    ;; Convert key modifiers
    (when (> (logand value lispbuilder-windows:MK_SHIFT) 0)
      (setf winval (logior value +key-modifier-shift+)))
    (when (> (logand value lispbuilder-windows:MK_CONTROL) 0)
      (setf winval (logior value +key-modifier-control+)))
    winval))

(defmethod button= ((state integer) &optional (button nil))
  ;; Convert mouse buttons
  (if (eq button (cond
		   ((> (logand state lispbuilder-windows:MK_LBUTTON) 0)
		    :BUTTON-LEFT)
		   ((> (logand state lispbuilder-windows:MK_MBUTTON) 0)
		    :BUTTON-MIDDLE)
		   ((> (logand state lispbuilder-windows:MK_RBUTTON) 0)
		    :BUTTON-RIGHT)
		   (t NIL)))
      button
      nil))

;; (defun make-window-state (&optional (initial-state :start))
;;   (let ((game-score 0)
;; 	(display-score-period 1000)
;; 	(current-state initial-state))
;;     #'(lambda #(window event wparam lparam)
;; 	(setf current-state
;; 	      (case current-state
;; 		(:start (set-frame 0)
;; 			(setf (visible-p obj) nil)
;; 			(show-score game-score)
;; 			:no-score-graphic)
;; 		(:no-score-graphic (when (event= :score-event)
;; 				     (show-score (incf game-score (points event)))
;; 				     (setf (visible actor) t)
;; 				     :show-score-graphic))
;; 		(:show-score-graphic (when (do-* do-after :ticks display-score-period)
;; 				       (setf (visible actor) t)
;; 				       :no-score-graphic)))))))

