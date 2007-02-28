
(in-package #:lispbuilder-sdl)

(defclass rwops ()
  ((foreign-pointer-to-rwops :accessor fp :initform nil :initarg :rwops)))

(defmethod free-rwops ((rwops rwops))
  (sdl-cffi::SDL-Free-RW (fp rwops))
  (tg:cancel-finalization rwops))

(defun create-RWops-from-file (filename path)
  (let ((rwops (sdl-base::create-RWops-from-file filename path)))
    (if (sdl-base::is-valid-ptr rwops)
	(make-instance 'rwops :rwops rwops)
	nil)))
