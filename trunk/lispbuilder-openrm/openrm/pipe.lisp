
(in-package #:rm)

(defclass rm-pipe (openrm-object) ()
  (:default-initargs
   :free (simple-free #'rm-cffi::rm-pipe-delete 'rm-pipe)))

(defclass pipe (rm-pipe) ()
  (:default-initargs
   :target :RM-PIPE-NOPLATFORM
   :processing-mode :RM-PIPE-MULTISTAGE
   :opaque :RM-TRUE
   :transparent :RM-TRUE
   :2D :RM-TRUE
   :swap-buffers t
   :display-list t))

(defmethod initialize-instance :around ((self pipe) &key
					(channel-format nil) (dimensions nil)
                                        (display-list nil) swap-buffers)
  (call-next-method)

  (setf (swap-buffers self) swap-buffers)
  (when channel-format
    (setf (channel-format self) channel-format))
  (when dimensions
    (setf (dimensions self) dimensions))
  (setf (display-list self) display-list))

(defmethod initialize-instance :after ((pipe pipe) &key
				       target processing-mode opaque transparent 2D)
  (setf (slot-value pipe 'foreign-pointer-to-object) (rm-cffi::rm-Pipe-New target))

  (log5:log-for (create) "initialize-instance.RM-PIPE: ~A, ~A, ~A"
                pipe (id pipe) (this-fp pipe))
  
  (if (cffi:null-pointer-p (fp pipe))
      (error "Cannot create OpenRM Pipe: ~A" (fp pipe))
      (progn 
	(unless (rm-cffi::rm-Pipe-Set-Render-Pass-Enable (fp pipe) opaque
                                                         transparent 2D)
	  (error "Cannot create OpenRM Pipe: ~A" (fp pipe)))
	(unless (rm-cffi::rm-Pipe-Set-Processing-Mode (fp pipe) processing-mode)
	  (error "Cannot create OpenRM Pipe: ~A" (fp pipe))))))

(defmethod (setf swap-buffers)(value (self pipe))
  (if (null value)
    (rm-cffi::rm-Pipe-Set-Swap-Buffers-Func (fp self) (cffi:null-pointer))
    (if (cffi:pointerp value)
      (rm-cffi::rm-Pipe-Set-Swap-Buffers-Func (fp self) value)))
  self)
  
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

(defmethod display-list ((self rm-pipe))
  (rm-cffi::rm-pipe-get-display-list-enable (fp self)))
(defmethod (setf display-list) (value (self rm-pipe))
  (rm-cffi::rm-pipe-set-display-list-enable (fp self) value))

(defmethod frame-rate ((self rm-pipe))
  (rm-cffi::rm-pipe-get-frame-rate (fp self)))
(defmethod (setf frame-rate) (value (self rm-pipe))
  (rm-cffi::rm-pipe-set-frame-rate (fp self) value))