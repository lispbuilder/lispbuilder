
(in-package :lispbuilder-sdl)

(defclass gfx-bitmap-font (bitmap-font foreign-object)
  ((font-default :accessor default-font-p :initform nil :initarg :default-p))
  (:default-initargs
   :gc t
    :free #'cffi:foreign-free))

(defmethod initialize-instance :after ((self gfx-bitmap-font)
				       &key (font-definition *gfx-font-8x8*))
  (setf (slot-value self 'foreign-pointer-to-object)
        (cffi:foreign-alloc :unsigned-char
                            :initial-contents (loop for i across (data font-definition)
                                                    collect i))))

(defmethod set-default-font ((font gfx-bitmap-font))
  (gfx-Primitives-Set-Font (fp font)
                           (char-width font)
                           (char-height font))
  (when (typep *default-font* 'gfx-bitmap-font)
    (setf (default-font-p *default-font*) nil))
  (setf (default-font-p font) t
	*default-font* font)
  font)

(defmethod initialise-font ((self gfx-font-definition))
  (make-instance 'sdl:gfx-bitmap-font :font-definition self))

