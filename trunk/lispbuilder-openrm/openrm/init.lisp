
(in-package #:rm)

(defun init-rm ()
  "Initializes the OpenRM library, if not already initialized."
  (unless *initialised*
    (rm-cffi::rm-Init)
    (setf *initialised* t)))

(defun quit-rm ()
  "Closes the OpenRM library."
  (rm-cffi::rm-finish)
  (setf *initialised* nil))

(defmethod clean-up (&optional (quit-rm nil))
  (delete-scene-graph)
  (close-windows)
  ;; A bug in OpenRM means that we can't call RM-FINISH to clean up
  ;; or the library will crash.       
  (when (and *initialised* quit-rm)
    (quit-rm)))

(defmacro with-init ((&optional type) &body body)
  "Attempts to initialize the OpenRM library."
  `(block nil
     (declare (ignore ,type))
     (unwind-protect
         (progn
           (init-rm)
           ,@body)
       (progn
	 (clean-up))
       ;; A bug in OpenRM means that we can't call RM-FINISH to clean up
       ;; or the library will crash.
       ;; (when *initialised* (setf *initialised* (rm-cffi:::rm-finish)))
       )))

