
(in-package #:lispbuilder-openrm)

(defun process-events (&optional (type :poll))
  (case type
    (:poll (process-events-poll *window*))
    (:wait (process-events-wait *window*))
    (t (error "PROCESS-EVENTS: TYPE must be :POLL or :WAIT"))))

(defclass event () ())

(defclass event-handler (event)
  ((event-handlers :initform nil)
   (event-handlers-filters :initform nil)
   (listeners :accessor listeners :initform nil)))

;; (defun its-name (obj)
;;   "Returns the name of the class..
;; From http://lispdoc.com/?q=class."
;;   (class-name (class-of obj)))

(defmethod initialize-instance :after ((self event-handler) &key
				       (events nil))
  (loop for event in events do
       (enable-event self event)))

(defun has-event-handler? (obj ev)
  (cdr (assoc ev (slot-value obj 'event-handlers))))

(defun register-event-handler (obj ev handler)
  (when (has-event-handler? obj ev)
      (unregister-event-handler obj ev))
  (setf (slot-value obj 'event-handlers) (acons ev handler (slot-value obj 'event-handlers))))

(defun unregister-event-handler (obj ev)
  (when (has-event-handler? obj ev)
    (setf (slot-value obj 'event-handlers) (rassoc ev (slot-value obj 'event-handlers)))))

(defun add-event-handler-filter (obj ev)
  (unless (find ev (slot-value obj 'event-handlers-filters))
    (setf (slot-value obj 'event-handlers-filters) (cons ev (slot-value obj 'event-handlers-filters)))))

(defun remove-event-handler-filter (obj ev)
  (when (find ev (slot-value obj 'event-handlers-filters))
    (setf (slot-value obj 'event-handlers-filters) (cons ev (slot-value obj 'event-handlers-filters)))))

(defun event-enabled? (obj ev)
  (find ev (slot-value obj 'event-handlers-filters)))

(defun enable-event (obj ev)
  (add-event-handler-filter obj ev))

(defun disable-event (obj ev)
  (remove-event-handler-filter obj ev))

(defmacro dispatch-event (obj ev &rest args)
  (let ((handler (gensym "handler-")))
    ;; Is the event enabled?
    `(when ,obj
       (when (event-enabled? ,obj ',ev)
	 (let ((,handler (has-event-handler? ,obj ',ev)))
	   (if ,handler
	       (funcall ,handler ,obj ,@args)
	       (,ev ,obj ,@args)))))))

(defmethod add-event-listener ((parent event-handler) (child event-handler))
  (pushnew child (listeners parent)))

(defmethod remove-event-listener ((parent event-handler) (child event-handler))
  (setf (listeners parent) (remove child (listeners parent))))

;;;
;;; Event Handlers

(defmacro define-event-handlers (&body event-specs)
  `(progn
     ,@(loop for (name (window-arg . args)) in event-specs
	  collect
	  `(progn
	     (defgeneric ,(intern
			   (concatenate 'string "HANDLE-" (symbol-name name)))
		 (,window-arg ,@args))
	     (defmethod ,(intern
			  (concatenate 'string "HANDLE-" (symbol-name name)))
		 ((,window-arg event-handler) ,@args)
	       ;; (dispatch-event ,window-arg ,name;; (intern (concatenate 'string "ON-" (symbol-name name)))
;; 			       ,@args)
;; 	       (loop for child in (listeners ,window-arg)
;; 		  do (,(intern (concatenate 'string "HANDLE-" (symbol-name name))) child ,@args))
	       (unless (dispatch-event ,window-arg ,name;; (intern (concatenate 'string "ON-" (symbol-name name)))
				       ,@args)
		 (loop for child in (listeners ,window-arg)
		    do (,(intern (concatenate 'string "HANDLE-" (symbol-name name))) child ,@args))))
	     (defgeneric ,name (,window-arg ,@args))
	     (defmethod ,name ((,window-arg event) ,@args))))))

(define-event-handlers
  (on-idle              (obj))
  (on-create            (obj))
  (on-destroy           (obj))
  (on-paint             (obj))
  (on-resize            (obj width height))
  (on-expose            (obj))
  (on-quit              (obj))
  (on-wm-event          (obj))
  (on-user              (obj type code data1 data2))
  (on-mouse-move        (obj button x y x-rel y-rel))
  (on-mouse-up          (obj button x y))
  (on-mouse-down        (obj button x y))
  (on-key-down          (obj state scancode key mod unicode))
  (on-key-up            (obj state scancode key mod unicode))
  (on-active            (obj gain state))
  (on-joy-axis-motion   (obj which axis value))
  (on-joy-button-down   (obj which button state))
  (on-joy-button-up     (obj which button state))
  (on-joy-hat-motion    (obj which axis value))
  (on-joy-ball-motion   (obj which ball x-rel y-rel)))
