;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful code for managing key presses

(in-package #:lispbuilder-sdl)

(defparameter *input-util-initialised* nil)
(defparameter *key-status-table* nil)

(defstruct key-status status time prev-time)

(defun get-or-create-key-status(key)
  (let ((status (gethash key *key-status-table*)))
    (if (key-status-p status)
	status
	(setf (gethash key *key-status-table*) 
	      (make-key-status :status 'unknown :time 0.0 :prev-time 0.0)))))

(defun initialise-input-util()
  "set up the input system"
  (unless *input-util-initialised*
    (setf *input-util-initialised* t)
    (setf *key-status-table* (make-hash-table))))

(defun debug-view-keys()
  (loop for key being the hash-keys of *key-status-table* do
       (let ((status (gethash key *key-status-table*)))
	 (format t "key ~a status ~a time ~a prev time ~a~%" key (key-status-status status) 
		 (key-status-time status) (key-status-prev-time status)))))

(defun update-input-util(time)
;  (debug-view-keys)
  (loop for key-status being the hash-values of *key-status-table* do
       (incf (key-status-time key-status) time)))

(defun quit-input-util()
  (setf *input-util-initialised* nil)
  (setf *key-status-table* nil))

(defun handle-key-up(key)
  (let ((status (get-or-create-key-status key)))
    (setf (key-status-status status) 'released)
    (setf (key-status-prev-time status) (key-status-time status))
    (setf (key-status-time status) 0.0)))

(defun handle-key-down(key)
  (let ((status (get-or-create-key-status key)))
    (setf (key-status-status status) 'pressed)
    (setf (key-status-prev-time status) (key-status-time status))
    (setf (key-status-time status) 0.0)))

(defun key-held-p(key)
  (let ((status (gethash key *key-status-table*)))
    (if (key-status-p status)
	(equal 'pressed (key-status-status status))
	nil)))

(defun key-held-time(key)
  (let ((status (gethash key *key-status-table*)))
    (if (key-status-p status)
	(key-status-time status)
	0.0)))

(defun key-pressed-p(key)
  (let ((status (gethash key *key-status-table*)))
    (if (and (key-status-p status)
	     (eq (key-status-status status) 'pressed)
	     (= 0.0 (key-status-time status)))
	t
	nil)))


	
	     

