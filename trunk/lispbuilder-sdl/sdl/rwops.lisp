
(in-package #:lispbuilder-sdl)

(defclass rwops ()
  ((foreign-pointer-to-rwops :accessor fp :initform nil :initarg :rwops))
  (:documentation "A wrapper around a foreign SDL_RWops object."))

(defmethod free-rwops ((rwops rwops))
  "Free's the wrapped foreign SDL_rwops object."
  (sdl-cffi::SDL-Free-RW (fp rwops))
  (tg:cancel-finalization rwops))

(defun create-RWops-from-file (filename)
  "Creates and returns a new `RWOPS` object from the file at location `FILENAME`."
  (let ((rwops (sdl-base::create-RWops-from-file filename)))
    (if (sdl-base::is-valid-ptr rwops)
	(make-instance 'rwops :rwops rwops)
	nil)))
