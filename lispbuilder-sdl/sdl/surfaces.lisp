;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

(defclass sdl-surface ()
  ((foreign-pointer-to-surface :reader fp :initform nil :initarg :surface)
   (foreign-pointer-to-position-rect :accessor fp-position :initform (cffi:foreign-alloc 'sdl-cffi::sdl-rectangle) :initarg :position)))

;;; An object of type display should never be finalized by CFFI
;;; at time of garbage collection.
(defclass display-surface (sdl-surface) ())

;;; An object of type is finalized by CFFI
;;; at time of garbage collection.
(defclass surface (sdl-surface) ())

(defclass rectangle-array ()
  ((foreign-pointer-to-rectangle :reader fp :initform nil :initarg :rectangle)
   (length :reader len :initform nil :initarg :length)))

(defun surface (surface-fp &optional (display nil))
  (if (sdl-base::is-valid-ptr surface-fp)
      (let ((surf (make-instance (if display
				     'display-surface
				     'surface)
				 :surface surface-fp)))
 	(setf (x surf) 0
	      (y surf) 0
	      (width surf) (sdl-base::rect-w (fp surf))
 	      (height surf) (sdl-base::rect-h (fp surf)))
	surf)
      (error "SURFACE: SURFACE-FP must be a foreign pointer.")))

(defmacro with-surface ((var &optional surface (free-p t))
			&body body)
  (let ((surface-ptr (gensym "surface-prt-"))
	(body-value (gensym "body-value-")))
    `(symbol-macrolet ((,(intern (string-upcase (format nil "~A.width" var))) (width ,var))
		       (,(intern (string-upcase (format nil "~A.height" var))) (height ,var))
		       (,(intern (string-upcase (format nil "~A.x" var))) (x ,var))
		       (,(intern (string-upcase (format nil "~A.y" var))) (y ,var)))
       (let* ((,@(if surface
		     `(,var ,surface)
		     `(,var ,var)))
	      (*default-surface* ,var)
	      (,body-value nil))
	 (sdl-base::with-surface (,surface-ptr (fp ,var) nil)
	   (setf ,body-value (progn ,@body))
	   (if ,free-p
	       (free-surface ,var)))
	 ,body-value))))

(defmacro with-surface-slots ((var &optional surface)
			      &body body)
  `(with-surface (,var ,surface nil)
     ,@body))

(defmacro with-surfaces (bindings &rest body)
  (if bindings
      (return-with-surface bindings body)))

(defun return-with-surface (bindings body)
  (if bindings
      `(with-surface (,@(car bindings))
	 ,(return-with-surface (cdr bindings) body))
      `(progn ,@body)))

(defmacro with-locked-surface ((var &optional surface)
			       &body body)
  (let ((surface-ptr (gensym "surface-prt-"))
	(body-value (gensym "body-value-")))
    `(let* ((,@(if surface
		   `(,var ,surface)
		   `(,var ,var)))
	    (*default-surface* ,var)
	    (,body-value nil))
       (sdl-base::with-locked-surface (,surface-ptr (fp ,var))
	 (setf ,body-value (progn ,@body)))
       ,body-value)))

(defmacro with-locked-surfaces (bindings &rest body)
  (if bindings
      (return-with-locked-surfaces bindings body)))

(defun return-with-locked-surfaces (bindings body)
  (if bindings
      `(with-locked-surface (,@(car bindings))
	 ,(return-with-locked-surfaces (cdr bindings) body))
      `(progn ,@body)))


(defmethod free-surface ((surface sdl-surface)) nil)

(defmethod free-surface ((self surface))
  (let ((foreign-pointer (fp self)))
    (setf (slot-value self 'foreign-pointer-to-surface) nil)
    (sdl-cffi::sdl-free-surface foreign-pointer))
  #-clisp(cffi:cancel-finalization self)
  )

(defmethod width ((surface sdl-surface))
  (sdl-base::surf-w (fp surface)))
(defmethod (setf width) (w-val (surface sdl-surface))
  (setf (sdl-base::rect-w (fp-position surface)) w-val))

(defmethod height ((surface sdl-surface))
  (sdl-base::surf-h (fp surface)))
(defmethod (setf height) (h-val (surface sdl-surface))
  (setf (sdl-base::rect-h (fp-position surface)) h-val))

(defmethod x ((surface sdl-surface))
  (sdl-base::rect-x (fp-position surface)))
(defmethod (setf x) (x-val (surface sdl-surface))
  (setf (sdl-base::rect-x (fp-position surface)) x-val))

(defmethod y ((surface sdl-surface))
  (sdl-base::rect-y (fp-position surface)))
(defmethod (setf y) (y-val (surface sdl-surface))
  (setf (sdl-base::rect-y (fp-position surface)) y-val))

(defmethod xy ((surface sdl-surface))
  (vector (x surface) (y surface)))
(defmethod set-xy ((surface sdl-surface) x y)
  (setf (x surface) x
	(y surface) y))

(defun clear-color-key (&key (surface *default-surface*) (rle-accel t))
  (sdl-base::clear-color-key (fp surface) rle-accel))

(defun set-color-key (color &key (surface *default-surface*) (rle-accel t))
  (sdl-base::set-color-key (fp surface) (map-color color surface ) rle-accel))
  
(defun set-alpha (alpha &key (surface *default-surface*) (rle-accel nil))
  (sdl-base::set-alpha (fp surface) alpha rle-accel))

(defun get-clip-rect (&key (surface *default-surface*) (rectangle (rectangle)))
  (sdl-base::get-clip-rect (fp surface) (fp rectangle))
  rectangle)

(defun set-clip-rect (rectangle &key (surface *default-surface*))
  (sdl-base::set-clip-rect (fp surface) (fp rectangle))
  rectangle)

(defun get-surface-rect (&key (surface *default-surface*) (rectangle (rectangle)))
  (sdl-base::get-surface-rect (fp surface) (fp rectangle))
  rectangle)

(defun convert-surface (&key (surface *default-surface*) key-color alpha-value (free-p nil))
  (let ((surf (sdl-base::convert-surface-to-display-format (fp surface)
							   :key-color (when key-color (map-color key-color surface))
							   :alpha-value (when alpha-value alpha-value)
							   :free-p nil)))
    (unless (sdl-base::is-valid-ptr surf)
      (error "ERROR, CONVERT-SURFACE: Cannot convert the surface."))
    (when free-p
      (free-surface surface))
    (surface surf)))

(defun copy-surface (&key (surface *default-surface*) key-color alpha-value (type :sw) rle-accel)
  (let ((surf (sdl-base::copy-surface (fp surface)
				      :color-key key-color
				      :alpha alpha-value
				      :type type
				      :rle-accel rle-accel)))
    (unless (sdl-base::is-valid-ptr surf)
      (error "ERROR, copy-surface: Cannot copy the surface."))
    (setf surf (surface surf))
    (when key-color
      (set-color-key key-color :surface surf :rle-accel rle-accel))
    (when alpha-value
      (set-alpha alpha-value :surface surf :rle-accel rle-accel))
    surf))

(defun create-surface (width height &key
		       (bpp 32) (surface *default-surface*) key-color alpha-value (type :sw) (rle-accel t))
  (let ((surf (sdl-base::create-surface width height
					:bpp bpp
					:surface (fp surface)
					:color-key key-color
					:alpha alpha-value
					:type type
					:rle-accel rle-accel)))
    (unless (sdl-base::is-valid-ptr surf)
      (error "ERROR, CREATE-SURFACE: Cannot create a new surface."))
    (setf surf (surface surf))
    (when key-color
      (set-color-key key-color :surface surf :rle-accel rle-accel))
    (when alpha-value
      (set-alpha alpha-value :surface surf :rle-accel rle-accel))
    surf))

(defun update-surface (surface &key template x y w h)
  (if template
      (if (typep template 'rectangle-array)
	  (sdl-base::update-surface (fp surface) :template (fp template) :number (len template))
	  (sdl-base::update-surface (fp surface) :template (fp template)))
      (sdl-base::update-surface (fp surface) :x x :y y :w w :h h))
  surface)

(defun blit-surface (src &optional (dst *default-surface*))
  (sdl-base::blit-surface (fp src) (fp dst) (cffi:null-pointer) (fp-position src))
  src)

(defun draw-surface (src &key (dst *default-surface*) (position nil))
  (when position
    (setf (x src) (x position)
	  (y src) (y position)))
  (sdl-base::blit-surface (fp src) (fp dst) (cffi:null-pointer) (fp-position src))
  src)

(defun fill-surface (color &key (dst *default-surface*) (template nil) (update-p nil) (clipping-p t))
  "fill the entire surface with the specified R G B A color.
   Use :template to specify the SDL_Rect to be used as the fill template.
   Use :update-p to call SDL_UpdateRect, using :template if provided. This allows for a 
   'dirty recs' screen update."
  (sdl-base::fill-surface (fp dst)
			  (map-color color dst)
			  :template (if template
					(fp template)
					(cffi::null-pointer))
			  :clipping-p clipping-p
			  :update-p update-p)
  dst)
