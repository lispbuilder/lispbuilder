
(in-package #:rm)

(defmethod clean-up (&optional (quit-rm nil))
  (delete-scene-graph)
  (close-windows)
  (when (and *initialised* quit-rm)
    ;; A bug in OpenRM means that we can't call RM-FINISH to clean up
    ;; or the library will crash.       
    (setf *initialised* (rm-cffi::rm-finish))))

(defmacro with-init ((&optional type) &body body)
  "Attempts to initialize the OpenRM library."
  `(block nil
     (unwind-protect
	  (progn
            (unless *initialised*
	      (setf *initialised* t)
	      (rm-cffi::rm-Init))
	    ,@body)
       (progn
	 (clean-up))
       ;; A bug in OpenRM means that we can't call RM-FINISH to clean up
       ;; or the library will crash.
       ;; (when *initialised* (setf *initialised* (rm-cffi:::rm-finish)))
       )))
