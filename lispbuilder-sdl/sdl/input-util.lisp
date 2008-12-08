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
  "gets the status of a key. if the key has never been pressed it creates
a new status (unknown state) and adds that to the hash table"
  (let ((status (gethash key *key-status-table*)))
    (if (key-status-p status)
	status
	(setf (gethash key *key-status-table*) 
	      (make-key-status :status 'unknown :time 0.0 :prev-time 0.0)))))

(defun initialise-input-util()
  "Initialises the input util system. This just creates the data structure that key press information 
is stored in, and maintains a global variable to track initialisation status.

##### Parameters

##### Returns
"
  (unless *input-util-initialised*
    (setf *input-util-initialised* t)
    (setf *key-status-table* (make-hash-table))))

(defun debug-view-keys()
  (loop for key being the hash-keys of *key-status-table* do
       (let ((status (gethash key *key-status-table*)))
	 (format t "key ~a status ~a time ~a prev time ~a~%" key (key-status-status status) 
		 (key-status-time status) (key-status-prev-time status)))))

(defun update-input-util(time)
  "This must be called once for each time period you are updating the application, in order 
to update keypress information. 

##### Parameters
* `TIME` is the time in seconds this update represents

##### Returns
"
;  (debug-view-keys)
  (loop for key-status being the hash-values of *key-status-table* do
       (incf (key-status-time key-status) time)))

(defun quit-input-util()
  "This is called when you quit your app to free up the key information data

##### Parameters

##### Returns
"
  (setf *input-util-initialised* nil)
  (setf *key-status-table* nil))

(defun handle-key-up(key)
  "You must call this when a key up event occurs

##### Parameters
* `KEY` is the SDL key definition for the key that is now up (for example :SDL-KEY-ESCAPE)
##### Returns
"
  (let ((status (get-or-create-key-status key)))
    (setf (key-status-status status) 'released)
    (setf (key-status-prev-time status) (key-status-time status))
    (setf (key-status-time status) 0.0)))

(defun handle-key-down(key)
  "You must call this when a key up event occurs

##### Parameters
* `KEY` is the SDL key definition for the key that is now down (for example :SDL-KEY-ESCAPE)
##### Returns
"
  (let ((status (get-or-create-key-status key)))
    (setf (key-status-status status) 'pressed)
    (setf (key-status-prev-time status) (key-status-time status))
    (setf (key-status-time status) 0.0)))

(defun key-held-p(key)
  "Returns true if a key is currently held, which means it has either
just been pressed, or it has been pressed and held for a while.

##### Parameters
* `KEY` is the SDL key definition (for example :SDL-KEY-ESCAPE)
##### Returns
True if the key is held
"
  (let ((status (gethash key *key-status-table*)))
    (if (key-status-p status)
	(equal 'pressed (key-status-status status))
	nil)))

(defun key-time-in-current-state(key)
  "Returns time a key has been in current state

##### Parameters
* `KEY` is the SDL key definition (for example :SDL-KEY-ESCAPE)
##### Returns
Time key is in current state
"
  (let ((status (gethash key *key-status-table*)))
    (if (key-status-p status)
	(key-status-time status)
	0.0)))

(defun key-time-in-previous-state(key)
  "Returns time key was in a previous state

##### Parameters
* `KEY` is the SDL key definition (for example :SDL-KEY-ESCAPE)
##### Returns
Time key was in previous state
"
  (let ((status (gethash key *key-status-table*)))
    (if (key-status-p status)
	(key-status-prev-time status)
	0.0)))


(defun key-pressed-p(key)
  "Returns true if a key has just been pressed

##### Parameters
* `KEY` is the SDL key definition (for example :SDL-KEY-ESCAPE)
##### Returns
True if the key was just pressed
"
  (let ((status (gethash key *key-status-table*)))
    (if (and (key-status-p status)
	     (eq (key-status-status status) 'pressed)
	     (= 0.0 (key-status-time status)))
	t
	nil)))

(defun key-released-p(key)
  "Returns true if a key has just been released

##### Parameters
* `KEY` is the SDL key definition (for example :SDL-KEY-ESCAPE)
##### Returns
True if the key was just pressed
"
  (let ((status (gethash key *key-status-table*)))
    (if (and (key-status-p status)
	     (eq (key-status-status status) 'released)
	     (= 0.0 (key-status-time status)))
	t
	nil)))




	
	     

