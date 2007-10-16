;;;; wget example
;;;; (C) 2007 Frank Buss <fb@frank-buss.de>

(in-package #:lispbuilder-net-examples) 

(defmacro with-net (&body body)
  `(block nil
     (unwind-protect
	 (progn
	   (if (= (netStartup) 0)
               (progn
                 ,@body)
             (format t "startup failed~%"))
           (unless (= (netCleanup) 0)
             (format t "cleanup failed~%"))))))

(defparameter *sockets* nil)
(defparameter *command* nil)
(defparameter *command-index* nil)
(defparameter *buffer-size* 1024)
(defparameter *socket-closed* nil)
(defparameter *buffer* (foreign-alloc :char :count *buffer-size*))
(defparameter *newline* (format nil "~C~C" #\RETURN #\LINEFEED))

(defcallback on-read :void
    ((socket :int))
  (let ((result (netRead socket *buffer* *buffer-size*)))
    (cond ((> result 0)
           (loop for i from 0 below result do
                 (princ (code-char (mem-aref *buffer* :char i)))))
          ((= result 0)
           (setf *socket-closed* t))
          (t (format t "read error: ~a~%" result)
             (setf *socket-closed* t)))))

(defcallback on-write :void
    ((socket :int))
  (let ((len (length *command*)))
    (when (< *command-index* len)
      (let ((size 0))
        (loop for i from *command-index* below len
              for j from 0 below *buffer-size* do
              (setf (mem-aref *buffer* :char j) (char-code (aref *command* i)))
              (incf size))
        (let ((result (netWrite socket *buffer* size)))
          (if (>= result 0)
              (incf *command-index* result)
            (format t "write error~%")))))))

(defcallback on-error :void
    ((socket :int))
  (declare (ignore socket))
  (format t "on-error~%"))

(defun wget (host page)
  (setf *sockets* (make-hash-table))
  (setf *command* (concatenate 'string
                               "GET " page " HTTP/1.1" *newline*
                               "host: " host " " *newline*
                               "connection: close" *newline*
                               "user-agent: Lisp" *newline* *newline*))
  (setf *command-index* 0)
  (setf *socket-closed* nil)
  (with-net
    (netSleep 1000)
    (netRegisterReadCallback (callback on-read))
    (netRegisterWriteCallback (callback on-write))
    (netRegisterErrorCallback (callback on-error))
    (with-foreign-object (socket-pointer ':int)
      (netCreateSocket socket-pointer)
      (let ((socket (mem-ref socket-pointer ':int))
            (start-time (get-universal-time))
            (timeout 5))
        (netConnect socket host 80)
        (loop repeat 5 do
              (netWait 1000)
              (let ((run-time (- (get-universal-time) start-time)))
                (when (> run-time timeout)
                  (format t "timeout~%")
                  (loop-finish))
                (when *socket-closed*
                  (format t "socket closed~%")
                  (loop-finish))
                (format t "tick~%")))))))

; (wget "www.frank-buss.de" "/lisp.txt")
