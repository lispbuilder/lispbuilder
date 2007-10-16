#| start like this:

(pushnew "/data/projects/asdf/lispbuilder-windows/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :lispbuilder-windows)
(asdf:operate 'asdf:load-op :lispbuilder-windows-examples)
(in-package :lispbuilder-windows-examples)
(gui-eval)

|#

(in-package :lispbuilder-windows-examples)

(defparameter *width* 600)
(defparameter *height* 400)
(defparameter *text-height* 100)
(defparameter *button-width* 100)
(defparameter *client-height* (- *height* *text-height*))
(defparameter *framebuffer-bitmap* nil)
(defparameter *framebuffer-dc* nil)
(defparameter *framebuffer* nil)
(defparameter *background-color* #xd8e9ec)
(defparameter *title* "GUI Eval")

(defparameter *hwnd* nil)
(defparameter *edit-wnd* nil)
(defparameter *button-wnd* nil)

(defparameter *timer-function* nil)
(defparameter *timer-defined* nil)

(defparameter *mouse-click-handler* nil)
(defparameter *mouse-move-handler* nil)
(defparameter *mouse-down-handler* nil)
(defparameter *mouse-up-handler* nil)

(defconstant +button-id+ 100)
(defconstant +timer-id+ 101)

(defmacro set-struct-members (object type &body values)
  (let ((elements (loop for i in values collect (car i))))
    `(with-foreign-slots (,elements ,object ,type)
       ,@(loop for (name value) in values collect `(setf ,name ,value)))))

(defmacro zero-mem (object type)
  (let ((i (gensym)))
    `(loop for ,i from 0 below (foreign-type-size (quote ,type)) do
           (setf (mem-aref ,object :char ,i) 0))))

(defun create-framebuffer ()
  (with-foreign-object (bmi 'BITMAPINFO)
    (zero-mem bmi BITMAPINFOHEADER)
    (set-struct-members bmi BITMAPINFOHEADER
      (biSize 40)
      (biWidth *width*)
      (biHeight (- *client-height*))
      (biPlanes 1)
      (biBitCount 32)
      (biCompression BI_RGB))
    (with-foreign-object (framebuffer-pointer ':int)
      (setf (mem-ref framebuffer-pointer ':int) 0)
      (setf *framebuffer-dc* (CreateCompatibleDC (null-pointer))
            *framebuffer-bitmap* (CreateDIBSection *framebuffer-dc*
                                                   bmi
                                                   DIB_RGB_COLORS
                                                   framebuffer-pointer
                                                   (null-pointer)
                                                   0)
            *framebuffer* (make-pointer (mem-ref framebuffer-pointer ':int)))
      (SetMapMode *framebuffer-dc* MM_TEXT)
      (SetBkColor *framebuffer-dc* *background-color*)
      (set-font 20)
      (SelectObject *framebuffer-dc* *framebuffer-bitmap*)
      (SelectObject *framebuffer-dc* (GetStockObject BLACK_PEN))
      (clear))))

(defun on-create (hWnd wParam lParam)
  (declare (ignore wParam lParam))
  (with-foreign-object (rect 'RECT)
    (GetClientRect hWnd rect)
    (let* ((width (foreign-slot-value rect 'RECT 'right))
           (height (foreign-slot-value rect 'RECT 'bottom))
           (x0 (floor (- (GetSystemMetrics SM_CXSCREEN) *width*) 2))
           (y0 (floor (- (GetSystemMetrics SM_CYSCREEN) *height*) 2))
           (dx (- *width* width))
           (dy (- *height* height)))
      (MoveWindow hWnd
                  (+ x0 (foreign-slot-value rect 'RECT 'left))
                  (+ y0 (foreign-slot-value rect 'RECT 'top))
                  (+ *width* dx)
                  (+ *height* dy)
                  0)
      (setf *edit-wnd* (CreateWindowEx
                        0 "EDIT" "(timer)"
                        (logior WS_VISIBLE WS_CHILD WS_BORDER
                                ES_LEFT ES_MULTILINE ES_WANTRETURN WS_VSCROLL)
                        0 *client-height* (- *width* *button-width*) *text-height*
                        hWnd (null-pointer) (GetModuleHandle (null-pointer)) (null-pointer)))
      (setf *button-wnd* (CreateWindowEx 0 "BUTTON" "Eval"
                                         (logior WS_VISIBLE WS_CHILD WS_BORDER)
                                         (- *width* *button-width*) *client-height*
                                         *button-width* *text-height*
                                         hWnd (make-pointer +button-id+)
                                         (GetModuleHandle (null-pointer))
                                         (null-pointer))))))

(defun on-paint (hWnd wParam lParam)
  (declare (ignore wParam lParam))
  (with-foreign-object (ps 'PAINTSTRUCT)
    (let ((hdc (BeginPaint hWnd ps)))
      (BitBlt hdc 0 0 *width* *height* *framebuffer-dc* 0 0 SRCCOPY)
      (EndPaint hWnd ps))))

(defun on-destroy (hWnd wParam lParam)
  (declare (ignore hWnd wParam lParam))
  (PostQuitMessage 0))

(defun get-edit-text ()
  (let ((text-length (1+ (SendMessage *edit-wnd* WM_GETTEXTLENGTH 0 0))))
    (with-foreign-pointer-as-string (buf text-length)
      (GetWindowText *edit-wnd* buf text-length))))

(defun on-command (hWnd wParam lParam)
  (declare (ignore hWnd lParam))
  (let ((id (logand wParam #xffff)))
    (cond
     ((= id +button-id+)
      (let ((text (get-edit-text)))
        (ignore-errors
          (with-input-from-string (stream text)
            (loop do
                  (let ((form (read stream nil)))
                    (unless form (loop-finish))
                    (eval form))))))))))

(defcallback window-proc :long
    ((hWnd :pointer)
     (message :unsigned-int)
     (wParam :unsigned-int)
     (lParam :long))
  (cond
   ((= message WM_CREATE) (on-create hWnd wParam lParam))
   ((or (= message WM_ERASEBKGND) (= message WM_PAINT)) (on-paint hWnd wParam lParam))
   ((= message WM_DESTROY) (on-destroy hWnd wParam lParam))
   ((= message WM_COMMAND) (on-command hWnd wParam lParam))
   ((= message WM_TIMER) (when *timer-function* (funcall *timer-function*)))
   ((= message WM_MOUSEMOVE)
    (when *mouse-move-handler*
      (funcall *mouse-move-handler*
               (logand lParam #xffff) (logand (ash lParam -16) #xffff))))
   ((= message WM_LBUTTONDOWN)
    (when *mouse-down-handler*
      (funcall *mouse-down-handler*
               (logand lParam #xffff) (logand (ash lParam -16) #xffff))))
   ((= message WM_LBUTTONUP)
    (when *mouse-up-handler*
      (funcall *mouse-up-handler*
               (logand lParam #xffff) (logand (ash lParam -16) #xffff))))
   (t (return-from window-proc (DefWindowProc hWnd message wParam lParam))))
  0)

(defun repaint ()
  (with-foreign-object (rect 'RECT)
    (set-struct-members rect RECT
      (left 0)
      (top 0)
      (right *width*)
      (bottom *client-height*))
    (InvalidateRect *hwnd* rect 0)))

(defun move-to (x y)
  (MoveToEx *framebuffer-dc* x y (null-pointer)))

(defun line-to (x y)
  (LineTo *framebuffer-dc* x y))

(defun clear ()
  (with-foreign-object (rect 'RECT)
    (set-struct-members rect RECT
      (left 0)
      (top 0)
      (right *width*)
      (bottom *client-height*))
    (let ((brush (CreateSolidBrush *background-color*)))
      (FillRect *framebuffer-dc* rect brush)
      (DeleteObject brush))))

(defun install-timer (intervall function)
  (when *timer-defined* (KillTimer *hwnd* +timer-id+))
  (SetTimer *hwnd* +timer-id+ intervall (null-pointer))
  (setf *timer-function* function)
  (setf *timer-defined* t))

(defun install-mouse-click-handler (function)
  (setf *mouse-click-handler* function))

(defun install-mouse-move-handler (function)
  (setf *mouse-move-handler* function))

(defun install-mouse-up-handler (function)
  (setf *mouse-up-handler* function))

(defun install-mouse-down-handler (function)
  (setf *mouse-down-handler* function))

(defun draw-string (x y text)
  (ExtTextOut *framebuffer-dc* x y ETO_OPAQUE (null-pointer) text (length text) (null-pointer)))

(defparameter *old-font* nil)

(defun set-font (height &optional (name "Arial"))
  (let ((font (CreateFont height 0
                          0 0 FW_NORMAL 0 0 0
                          DEFAULT_CHARSET OUT_DEFAULT_PRECIS
                          CLIP_DEFAULT_PRECIS DEFAULT_QUALITY
                          DEFAULT_PITCH name)))
    (SelectObject *framebuffer-dc* font)
    (when *old-font*
      (DeleteObject *old-font*))
    (setf *old-font* font)))

(defun message-box (text)
  (MessageBox *hwnd* text *title* MB_ICONINFORMATION))

(defun gui-eval ()
  (create-framebuffer)
  (let ((class-name (symbol-name (gensym "lisp-window-class-")))
        (hInstance (GetModuleHandle (null-pointer))))
    (with-foreign-object (class 'WNDCLASS)
      (zero-mem class WNDCLASS)
      (set-struct-members class WNDCLASS
        (style (logior CS_HREDRAW CS_VREDRAW))
        (lpfnWndProc (callback window-proc))
        (hInstance hInstance)
        (hCursor (LoadCursor (null-pointer) IDC_ARROW))
        (lpszClassName class-name))
      (RegisterClass class))
    (setf *hwnd* (CreateWindowEx 0 class-name *title*
                                 (logand (logior WS_VISIBLE WS_OVERLAPPEDWINDOW)
                                         (lognot WS_MAXIMIZEBOX)
                                         (lognot WS_MINIMIZEBOX)
                                         (lognot WS_SIZEBOX))
                                 0 0 *width* *height*
                                 (null-pointer) (null-pointer) hInstance (null-pointer)))
    (ShowWindow *hwnd* SW_SHOW)
    (UpdateWindow *hwnd*)
    (with-foreign-object (msg 'MSG)
      (loop with v = 0 do
            (setf v (GetMessage msg (null-pointer) 0 0))
            (when (zerop v) (loop-finish))
            (TranslateMessage msg)
            (DispatchMessage msg)))))

#+lispworks
(defun gui-eval-async ()
  (mp:process-run-function "window-loop"
                           '()
                           'show-window))

(defun hello ()
  (clear)
  (draw-string 250 180 "Hello World!")
  (move-to 200 200)
  (line-to 390 200)
  (repaint))

(defun painter ()
  (let ((is-down nil))
    (install-mouse-down-handler #'(lambda (x y)
                                    (move-to x y)
                                    (setf is-down t)))
    (install-mouse-up-handler #'(lambda (x y)
                                  (declare (ignore x y))
                                  (setf is-down nil)))
    (install-mouse-move-handler #'(lambda (x y)
                                    (when is-down
                                      (line-to x y)
                                      (repaint))))))

(defun timer ()
  (set-font 200)
  (let ((start (get-internal-real-time)))
    (install-timer 100
                   #'(lambda ()
                       (let ((elapsed (- (get-internal-real-time) start)))
                         (draw-string 100 100
                                      (format nil "~a"
                                              (floor elapsed
                                                     INTERNAL-TIME-UNITS-PER-SECOND)))
                         (repaint))))))
