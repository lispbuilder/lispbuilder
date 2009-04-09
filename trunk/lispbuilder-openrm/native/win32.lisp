
(in-package #:lispbuilder-openrm)

(defclass native-pipe (pipe)()
  (:default-initargs
   :target :RM-PIPE-WGL
    :processing-mode :RM-PIPE-MULTISTAGE
    :opaque :RM-TRUE
    :transparent :RM-TRUE
    :2D :RM-TRUE))

(defclass native-window (window)
  ((hwnd
    :reader hwnd
    :initform nil
    :initarg :hwnd)
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

(defmethod initialize-instance :around ((window native-window)
					&key)
  (setf *type* :native)
  (call-next-method))

(defmethod initialize-instance :after ((window native-window)
				       &key (x 0) (y 0))
  (let ((class-name (symbol-name (gensym "lisp-window-class-")))
	(hInstance (lispbuilder-windows:GetModuleHandle (cffi:null-pointer))))
    (cffi:with-foreign-object (class 'lispbuilder-windows:WNDCLASS)
      (zero-mem class lispbuilder-windows:WNDCLASS)
      (set-struct-members class lispbuilder-windows:WNDCLASS
	(lispbuilder-windows:style (logior lispbuilder-windows:CS_HREDRAW lispbuilder-windows:CS_VREDRAW))
	(lispbuilder-windows:lpfnWndProc (cffi:callback window-proc))
	(lispbuilder-windows:hInstance hInstance)
	(lispbuilder-windows:hCursor (lispbuilder-windows:LoadCursor (cffi:null-pointer) lispbuilder-windows:IDC_ARROW))
	(lispbuilder-windows:lpszClassName class-name))
      (lispbuilder-windows:RegisterClass class))

    (setf (hwnd window) (lispbuilder-windows:CreateWindowEx 0 class-name "OpenRM"
							    (logand (logior lispbuilder-windows:WS_VISIBLE
									    lispbuilder-windows:WS_OVERLAPPEDWINDOW
									    lispbuilder-windows:WS_CLIPCHILDREN
									    lispbuilder-windows:WS_CLIPSIBLINGS
									    lispbuilder-windows:WS_SIZEBOX)
								    (lognot lispbuilder-windows:WS_MAXIMIZEBOX)
								    (lognot lispbuilder-windows:WS_MINIMIZEBOX))
							    x y (slot-value window 'width) (slot-value window 'height)
							    (cffi:null-pointer) (cffi:null-pointer)
							    hInstance (fp (pipe window))))

    (add-window window)

    (rm-cffi::rm-pipe-set-window (fp (pipe window)) (hwnd window) (slot-value window 'width) (slot-value window 'height))
    (rm-cffi::rmw-pipe-create-context (fp (pipe window)))
    (rm-cffi::rm-Pipe-Make-Current (fp (pipe window)))

    (enable-event window 'on-paint)
    (enable-event window 'on-resize)
      ;;	(render window)
      ))

(defmethod close-window ((self native-window) hwnd)
  (log5:log-for (free) "CLOSE-WINDOW: ~A" self)
  (rm-cffi::wgl-Delete-Context (cffi:foreign-slot-value (fp (pipe self)) 'rm-cffi::rm-pipe 'rm-cffi::h-rc))
  (delete-window hwnd)
  ;; (lispbuilder-windows:PostQuitMessage 0)
  (process-events-wait))

(defmethod close-windows ()
  (loop for (hwnd . window) in *windows* do
       (close-window window hwnd)))

(defmacro when-window-found ((var hWnd) &body body)
  `(let ((,var (find-window ,hWnd)))
     (when ,var
       ,@body)))

;;; Default win32 events
(defun %on-destroy (hWnd wParam lParam)
  (declare (ignore wParam lParam))
  (let ((window (find-window hwnd)))
    (when window
      (rm-cffi::wgl-Delete-Context (cffi:foreign-slot-value (fp (pipe window)) 'rm-cffi::rm-pipe 'rm-cffi::h-rc))
      (delete-window hwnd)))
  (lispbuilder-windows:PostQuitMessage 0))

(defun %on-paint (window hWnd wParam lParam)
  (declare (ignore wParam lParam))
  (dispatch-event window on-paint)
  ;;(rm-cffi::rm-Pipe-Swap-Buffers-Win-32 (fp (pipe window)))
  (lispbuilder-windows:Validate-Rect hWnd (cffi:null-pointer)))

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
					  0))))
    ;(dispatch-event window on-create)
;;     (let ((hdc (lispbuilder-windows::get-dc hwnd)))
;;       (declare (ignore hdc))
;;       (cffi:with-foreign-object (p-f-d 'lispbuilder-windows:pixelformatdescriptor)
;; 	(zero-mem p-f-d lispbuilder-windows:pixelformatdescriptor)
;; 	(cffi:with-foreign-slots ((lispbuilder-windows::nSize
;; 				   lispbuilder-windows::nVersion
;; 				   lispbuilder-windows::dwFlags
;; 				   lispbuilder-windows::iPixelType
;; 				   lispbuilder-windows::cColorBits
;; 				   lispbuilder-windows::cDepthBits
;; 				   lispbuilder-windows::iLayerType)
;; 				  p-f-d lispbuilder-windows:pixelformatdescriptor)
;; 	  (setf lispbuilder-windows::nSize (cffi:foreign-type-size 'lispbuilder-windows:pixelformatdescriptor)
;; 		lispbuilder-windows::nVersion 1
;; 		lispbuilder-windows::dwFlags (logior lispbuilder-windows:PFD_DRAW_TO_WINDOW
;; 						     lispbuilder-windows:PFD_SUPPORT_OPENGL
;; 						     lispbuilder-windows:PFD_DOUBLEBUFFER)
;; 		lispbuilder-windows::iPixelType lispbuilder-windows:PFD_TYPE_RGBA
;; 		lispbuilder-windows::cColorBits 24
;; 		lispbuilder-windows::cDepthBits 16
;; 		lispbuilder-windows::iLayerType lispbuilder-windows:PFD_MAIN_PLANE))
;; 	(let ((iFormat (lispbuilder-windows:Choose-Pixel-Format hdc p-f-d)))
;; 	  (lispbuilder-windows:set-pixel-format hdc iFormat p-f-d)
;; 	  (let ((hrc (rm-cffi::wgl-Create-Context hdc))
;; 		(pipe (cffi:foreign-slot-value (cffi:make-pointer lparam) 'lispbuilder-windows:CREATESTRUCT
;; 					       'lispbuilder-windows::lpCreateParams)))  
;; 	    (setf (cffi:foreign-slot-value pipe 'rm-cffi::rm-pipe 'rm-cffi::hdc) hdc
;; 		  (cffi:foreign-slot-value pipe 'rm-cffi::rm-pipe 'rm-cffi::hrc) hrc)))))
	))

(defun %on-mouse-move (window button-state x y wparam)
  (declare (ignore wparam))
  (handle-on-mouse-move window button-state x y 0 0))

(defun %on-mouse-down (window button x y wparam)
  (declare (ignore wparam))
  (handle-on-mouse-down window button x y))

(defun %on-mouse-up (window button x y wparam)
  (declare (ignore wparam))
  (handle-on-mouse-up window button x y))

(defun %on-resize (window width height wparam)
  (declare (ignore wparam))
  (handle-on-resize window width height)
  ;; (lispbuilder-windows:Validate-Rect hWnd (cffi:null-pointer))
  )


(defun %on-idle ()
  ;; if there are no events for any windows on this thread then
  ;; call the idle event for each window.
  (loop for (hwnd . window) in *windows* do
       (handle-on-idle window)))

(cffi:defcallback window-proc :long
    ((hWnd :pointer)
     (message :unsigned-int)
     (wParam :unsigned-int)
     (lParam :long))
  ;;(funcall (state window) window message wparam lparam)
  (cond
    ((= message lispbuilder-windows:WM_CREATE)
     (%on-create hWnd wParam lParam))
    ((or (= message lispbuilder-windows:WM_ERASEBKGND)
	 (= message lispbuilder-windows:WM_PAINT))
     (when-window-found (window hWnd)
       (%on-paint window hWnd wParam lParam)) 
     0)
    ((= message lispbuilder-windows:WM_SHOWWINDOW)
     (when-window-found (window hWnd)
       (%on-paint window hWnd wParam lParam))
     0)
    ((= message lispbuilder-windows:WM_DESTROY)
     (%on-destroy hWnd wParam lParam))
    ((= message lispbuilder-windows:WM_SIZE)
     (when-window-found (window hWnd)
       (%on-resize window
		   (logand lParam #xffff)
		   (logand (ash lParam -16) #xffff) wParam)))
    ;;   ((= message WM_COMMAND) (on-command hWnd wParam lParam))
    ((= message lispbuilder-windows:WM_TIMER)
     (when-window-found (window hWnd)
       (when (timer window)
	 (when (eq nil (funcall (timer-function window) window))
	   (lispbuilder-windows:Destroy-Window hWnd)))))
    ((= message lispbuilder-windows:WM_MOUSEMOVE)
     (when-window-found (window hWnd)
       (%on-mouse-move window
		       wParam;; (convert-from-win32-codes wParam)
		       (logand lParam #xffff)
		       (logand (ash lParam -16) #xffff)
		       wparam))
     0)
    ((or (= message lispbuilder-windows:WM_LBUTTONDOWN)
	 (= message lispbuilder-windows:WM_RBUTTONDOWN)
	 (= message lispbuilder-windows:WM_MBUTTONDOWN))
     (when-window-found (window hWnd)
       (%on-mouse-down window
		      wParam;; (convert-from-win32-codes wParam)
		      (logand lParam #xffff)
		      (logand (ash lParam -16) #xffff)
		      wparam))
     0)
    ((or (= message lispbuilder-windows:WM_LBUTTONUP)
	 (= message lispbuilder-windows:WM_RBUTTONUP)
	 (= message lispbuilder-windows:WM_MBUTTONUP))
     (when-window-found (window hWnd)
       (%on-mouse-up window
		     wParam;; (convert-from-win32-codes wParam)
		     (logand lParam #xffff)
		     (logand (ash lParam -16) #xffff)
		     wparam))
     0)
;;     ((= message lispbuilder-windows:WM_QUiT)
;;      (when-window-found (window hWnd)
;;        (%on-quit window wParam))
;;      0)
    (t (return-from window-proc (lispbuilder-windows:DefWindowProc hWnd message wParam lParam))))
  0)

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

(defun pump-events (null-pointer msg)
  (let ((quit nil))
    (loop until (eq 0 (lispbuilder-windows::PeekMessage msg null-pointer 0 0 lispbuilder-windows::PM_NOREMOVE))
       do (progn
	    (if (> (lispbuilder-windows:GetMessage msg null-pointer 0 0) 0)
		(progn
		  (lispbuilder-windows:TranslateMessage msg)
		  (lispbuilder-windows:DispatchMessage msg))
		(progn
		 (setf quit t)
		 (loop-finish)))))
    quit))

(defmethod process-events-wait ((type (eql :NATIVE)))
  (let ((null-pointer (cffi:null-pointer)))
    (cffi:with-foreign-object (msg 'lispbuilder-windows:MSG)
      (loop until (zerop (lispbuilder-windows:GetMessage msg null-pointer 0 0))
	   do (progn 
		(lispbuilder-windows:TranslateMessage msg)
		(lispbuilder-windows:DispatchMessage msg))))))

(defmethod process-events-poll ((type (eql :NATIVE)))
  (let ((null-pointer (cffi:null-pointer)))
    (cffi:with-foreign-object (msg 'lispbuilder-windows:MSG)
      (loop until (pump-events null-pointer msg) do
	   (%on-idle)))))

(defun process-events (&optional (type :poll))
  (case type
    (:poll (process-events-poll *type*))
    (:wait (process-events-wait *type*))
    (t (error "PROCESS-EVENTS: TYPE must be :POLL or :WAIT"))))

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

