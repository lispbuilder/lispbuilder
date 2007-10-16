;;;; webserver example
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

(defparameter *server-socket* nil)
(defparameter *buffer-size* 1024)
(defparameter *shutdown* nil)
(defparameter *connections* nil)
(defparameter *socket-closed* nil)
(defparameter *buffer* (foreign-alloc :char :count *buffer-size*))
(defparameter *newline* (format nil "~C~C" #\return #\linefeed))
(defparameter *htdocs* nil)

(defun make-string-buffer ()
  (make-array 0
              :element-type 'base-char
              :fill-pointer 0
              :adjustable t))

(defclass connection ()
  ((socket :accessor socket :initform 0 :initarg :socket)
   (line :accessor line :initform (make-string-buffer))
   (return-read :accessor return-read :initform nil :initarg :return-read)
   (requested-page :accessor requested-page)
   (result :accessor result :initform nil)
   (result-index :accessor result-index)
   (page-name :accessor page-name :initform nil)))

(defmethod handle-write ((c connection))
  (when (result c)
    (let ((len (length (result c))))
      (loop do
            (when (< (result-index c) len)
              (let ((size 0))
                (loop for i from (result-index c) below len
                      for j from 0 below *buffer-size* do
                      (setf (mem-aref *buffer* :char j) (aref (result c) i))
                      (incf size))
                (let ((result (netWrite (socket c) *buffer* size)))
                  (cond ((= result (- NET_WOULD_BLOCK))
                         (loop-finish))
                        ((>= result 0)
                         (incf (result-index c) result))
                        (t
                         (format t "write error~%")
                         (loop-finish))))))
            (when (>= (result-index c) len)
              (format t "closing connection~%")
              (netCloseSocket (socket c))
              (loop-finish))))))

(defun extract-get-page-name (get-header)
  (when (> (length get-header) 4)
    (when (string= (subseq get-header 0 4) "GET ")
      (let ((page-name (subseq get-header 4)))
        (let ((space (position #\space page-name)))
          (if space
              (subseq page-name 0 space)
            page-name))))))

(defconstant +content-types+
  '(("png" . "image/png")
    ("gif" . "image/gif")
    ("jpg" . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("text" . "plain/text")))

(defun get-content-type (name)
  (let ((point (position #\. name :from-end t)))
    (when point
      (let ((suffix (subseq name (1+ point))))
        (let ((found-type (assoc suffix +content-types+ :test 'equal)))
          (when found-type
            (return-from get-content-type (cdr found-type)))))))
  "text/html")

(defmethod evaluate-line ((c connection))
  (unless (page-name c)
    (setf (page-name c) (extract-get-page-name (line c)))
    (when (string= (page-name c) "/shutdown")
      (setf *shutdown* t))
    (when (string= (page-name c) "/")
      (setf (page-name c) "index.html")))
  (when (= (length (line c)) 0)
    (when (char= (elt (page-name c) 0) #\/)
      (setf (page-name c) (subseq (page-name c) 1)))
    (setf (page-name c) (merge-pathnames (page-name c) *htdocs*))
    (format t "file request: ~a~%" (page-name c))
    (when (probe-file (page-name c))
      (with-open-file (input (page-name c) :direction :input
                             :element-type '(unsigned-byte 8))
        (let ((header 
               (concatenate 'string
                            "HTTP/1.1 200 OK" *newline*
                            "Server: Lisp" *newline*
                            "Connection: close" *newline*
                            "Content-Type: " (get-content-type (file-namestring (page-name c)))
                            *newline* *newline*))
              (file-length (file-length input))
              (i 0))
          (setf (result c) (make-array (+ (length header) file-length)
                                       :element-type '(unsigned-byte 8)))
          (loop for char across header do
                (setf (aref (result c) i) (char-code char))
                (incf i))
          (loop repeat file-length do
                (setf (aref (result c) i) (read-byte input))
                (incf i))
          (setf (result-index c) 0)
          (handle-write c))))))

(defmethod handle-read ((c connection))
  (let ((result (netRead (socket c) *buffer* *buffer-size*)))
    (cond ((> result 0)
           (loop for i from 0 below result do
                 (let ((char (mem-aref *buffer* :char i)))
                   (cond ((= char 13)
                          (setf (return-read c) t))
                         ((= char 10)
                          (evaluate-line c)
                          (setf (line c) (make-string-buffer))
                          (setf (return-read c) nil))
                         (t
                          (setf (return-read c) nil)
                          (vector-push-extend (code-char char) (line c)))))))
          ((= result 0)
           (setf *socket-closed* t))
          ((= result NET_WOULD_BLOCK))
          (t (format t "read error: ~a~%" result)
             (setf *socket-closed* t)))))

(defun accept-connection ()
  (with-foreign-objects ((socket-pointer ':int) (ip ':pointer))
    (setf (mem-ref ip :pointer) (null-pointer))
    (let ((result (netAccept *server-socket* ip socket-pointer)))
      (cond ((= result 0)
             (let ((string (mem-ref ip :pointer)))
               (unless (null-pointer-p string)
                 (format t "incoming connection from ip: ~a~%" (foreign-string-to-lisp string))
                 (netFree string)))
             (let ((client-socket (mem-ref socket-pointer ':int)))
               (setf (gethash client-socket *connections*)
                     (make-instance 'connection :socket client-socket))))
            (t (format t "accept error: ~a~%" result))))))

(defcallback on-read :void
    ((socket :int))
  (if (= socket *server-socket*)
      (accept-connection)
    (let ((connection (gethash socket *connections*)))
      (when connection
        (handle-read connection)))))

(defcallback on-write :void
    ((socket :int))
  (let ((connection (gethash socket *connections*)))
    (when connection
      (handle-write connection))))

(defcallback on-error :void
    ((socket :int))
  (declare (ignore socket))
  (format t "on-error~%"))

(defun webserver (directory)
  (setf *connections* (make-hash-table))
  (setf *htdocs* directory)
  (format t "starting server, htdocs: ~a~%" *htdocs*)
  (with-net
    (netRegisterReadCallback (callback on-read))
    (netRegisterWriteCallback (callback on-write))
    (netRegisterErrorCallback (callback on-error))
    (with-foreign-object (socket-pointer ':int)
      (netCreateServerSocket (null-pointer) 80 socket-pointer)
      (setf *server-socket* (mem-ref socket-pointer ':int))
      (netListen *server-socket* 5)
      (loop do
            (netWait 1000)
            (when *shutdown*
              (format t "webserver shutdown~%")
              (loop-finish))
            (format t "tick~%")))
    (netCloseSocket *server-socket*)))

; (webserver "/data/projects/lispbuilder/trunk/lispbuilder-net/documentation/")

; use e.g. http://localhost/index.html for the documentation page
; and http://localhost/shutdown for stopping the server
