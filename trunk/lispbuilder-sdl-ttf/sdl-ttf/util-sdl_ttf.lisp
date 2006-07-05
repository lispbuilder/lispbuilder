;; This file contains some useful functions for using SDL_ttf from Common lisp
;; 2006 (c) Rune Nesheim, see LICENCE.

(in-package #:lispbuilder-sdl-ttf)

;;;; Globals

(defvar *default-font* nil)
(defvar *library-initialized* 0)

;;; Macros

;;; w

(defmacro with-init (() &body body)
  `(unwind-protect
	(progn
	  (when (= sdl-ttf::*library-initialized* 0)
	    (incf sdl-ttf::*library-initialized*)
	    (init))
	  ,@body)
     (decf sdl-ttf::*library-initialized*)
     (when (= sdl-ttf::*library-initialized* 0)
       (Quit))))

(defmacro with-open-font ((font-name size &optional font-path) &body body)
  `(sdl-ttf:with-init ()
     (let ((*default-font* (sdl-ttf:open-font (namestring (if ,font-path
							      (merge-pathnames ,font-name ,font-path)
							      ,font-name))
					      ,size)))
       (if *default-font*
	   (progn
	     ,@body
	     (sdl-ttf:close-font *default-font*))))))

;;; Functions

;;; d

(defun draw-text-solid (text &key
			(font sdl-ttf:*default-font*)
			(position sdl:*default-position*)
			(surface sdl:*default-surface*)
			(color sdl:*default-color*)
			update-p)
  (sdl:with-surface ((Render-Text-Solid font text color))
    (sdl:blit-surface sdl:*default-surface* surface :dst-rect position :update-p update-p)))

(defun draw-text-UTF8-solid (text &key
			     (font sdl-ttf:*default-font*)
			     (position sdl:*default-position*)
			     (surface sdl:*default-surface*)
			     (color sdl:*default-color*)
			     update-p)
  (sdl:with-surface ((Render-UTF8-Solid font text color))
    (sdl:blit-surface sdl:*default-surface* surface :dst-rect position :update-p update-p)))

;; (defun draw-text-UNICODE-solid (surface font text color position &key update-p)
;;   (let ((text-surf (RenderUNICODE-Solid font text color)))
;;     (if text-surf
;; 	(sdl:blit-surface text-surf surface :dst-rect position :update-p update-p))))

(defun draw-text-GLYPH-solid (ch &key
			      (font sdl-ttf:*default-font*)
			      (position sdl:*default-position*)
			      (surface sdl:*default-surface*)
			      (color sdl:*default-color*)
			      update-p)
  (sdl:with-surface ((Render-Glyph-Solid font ch color))
    (sdl:blit-surface sdl:*default-surface* surface :dst-rect position :update-p update-p)))

(defun draw-text-shaded (text fg-color bg-color &key
			 (font sdl-ttf:*default-font*)
			 (position sdl:*default-position*)
			 (surface sdl:*default-surface*)
			 update-p)
  (sdl:with-surface ((Render-Text-shaded font text fg-color bg-color))
    (sdl:blit-surface sdl:*default-surface* surface :dst-rect position :update-p update-p)))

(defun draw-text-UTF8-shaded (text fg-color bg-color &key
			      (font sdl-ttf:*default-font*)
			      (position sdl:*default-position*)
			      (surface sdl:*default-surface*)
			      update-p)
  (sdl:with-surface ((Render-UTF8-shaded font text fg-color bg-color))
    (sdl:blit-surface sdl:*default-surface* surface :dst-rect position :update-p update-p)))

;; (defun draw-text-UNICODE-solid (surface font text color position &key update-p)
;;   (let ((text-surf (RenderUNICODE-Solid font text color)))
;;     (if text-surf
;; 	(sdl:blit-surface text-surf surface :dst-rect position :free-src t :update-p update-p))))

(defun draw-text-GLYPH-shaded (ch fg-color bg-color &key
			       (font sdl-ttf:*default-font*)
			       (position sdl:*default-position*)
			       (surface sdl:*default-surface*)
			       update-p)
  (sdl:with-surface ((Render-Glyph-shaded font ch fg-color bg-color))
    (sdl:blit-surface sdl:*default-surface* surface :dst-rect position  :update-p update-p)))

(defun draw-text-blended (text &key
			  (font sdl-ttf:*default-font*)
			  (position sdl:*default-position*)
			  (surface sdl:*default-surface*)
			  (color sdl:*default-color*)
			  update-p)
  (sdl:with-surface ((Render-Text-blended font text color))
    (sdl:blit-surface sdl:*default-surface* surface :dst-rect position  :update-p update-p)))

(defun draw-text-UTF8-blended (text &key
			       (font sdl-ttf:*default-font*)
			       (position sdl:*default-position*)
			       (surface sdl:*default-surface*)
			       (color sdl:*default-color*)
			       update-p)
  (sdl:with-surface ((Render-UTF8-blended font text color))
    (sdl:blit-surface sdl:*default-surface* surface :dst-rect position  :update-p update-p)))

;; (defun draw-text-UNICODE-solid (surface font text color position &key update-p)
;;   (let ((text-surf (RenderUNICODE-Solid font text color)))
;;     (if text-surf
;; 	(sdl:blit-surface text-surf surface :dst-rect position  :update-p update-p))))

(defun draw-text-GLYPH-blended (ch &key
				(font sdl-ttf:*default-font*)
				(position sdl:*default-position*)
				(surface sdl:*default-surface*)
				(color sdl:*default-color*)
				update-p)
  (sdl:with-surface ((Render-Glyph-blended font ch color))
    (sdl:blit-surface sdl:*default-surface* surface :dst-rect position  :update-p update-p)))

;;; g

(defun get-Glyph-Metric (ch &key metric (font sdl-ttf:*default-font*))
  (let ((p-minx (cffi:null-pointer))
	(p-miny (cffi:null-pointer))
	(p-maxx (cffi:null-pointer))
	(p-maxy (cffi:null-pointer))
	(p-advance (cffi:null-pointer))
	(val nil)
	(r-val nil))
    (cffi:with-foreign-objects ((minx :int) (miny :int) (maxx :int) (maxy :int) (advance :int))
      (case metric
	(:minx (setf p-minx minx))
	(:miny (setf p-miny miny))
	(:maxx (setf p-maxx maxx))
	(:maxy (setf p-maxy maxy))
	(:advance (setf p-advance advance)))
      (setf r-val (GlyphMetrics font ch p-minx p-maxx p-miny p-maxy p-advance))
      (if r-val
	  (cond
	    ((sdl:is-valid-ptr p-minx)
	     (setf val (cffi:mem-aref p-minx :int)))
	    ((sdl:is-valid-ptr miny)
	     (setf val (cffi:mem-aref p-miny :int)))
	    ((sdl:is-valid-ptr maxx)
	     (setf val (cffi:mem-aref p-maxx :int)))
	    ((sdl:is-valid-ptr maxy)
	     (setf val (cffi:mem-aref p-maxy :int)))
	    ((sdl:is-valid-ptr advance)
	     (setf val (cffi:mem-aref p-advance :int))))
	  (setf val r-val)))
    val))

(defun get-Size-Text (text &key size (font sdl-ttf:*default-font*))
  (let ((p-x (cffi:null-pointer))
	(p-y (cffi:null-pointer))
	(val nil)
	(r-val nil))
    (cffi:with-foreign-objects ((x :int) (y :int))
      (case size
	(:x (setf p-x x))
	(:y (setf p-y y)))
      (setf r-val (SizeText font text p-x p-y))
      (if r-val
	  (cond
	    ((sdl:is-valid-ptr p-x)
	     (setf val (cffi:mem-aref p-x :int)))
	    ((sdl:is-valid-ptr p-y)
	     (setf val (cffi:mem-aref p-y :int))))
	  (setf val r-val)))
    val))

(defun get-Size-UTF8 (text &key size (font sdl-ttf:*default-font*))
  (let ((p-x (cffi:null-pointer))
	(p-y (cffi:null-pointer))
	(val nil)
	(r-val nil))
    (cffi:with-foreign-objects ((x :int) (y :int))
      (case size
	(:x (setf p-x x))
	(:y (setf p-y y)))
      (setf r-val (SizeText font text p-x p-y))
      (if r-val
	  (cond
	    ((sdl:is-valid-ptr p-x)
	     (setf val (cffi:mem-aref p-x :int)))
	    ((sdl:is-valid-ptr p-y)
	     (setf val (cffi:mem-aref p-y :int))))
	  (setf val r-val)))
    val))

(defun get-Size-UNICODE (text &key size (font sdl-ttf:*default-font*))
  (let ((p-x (cffi:null-pointer))
	(p-y (cffi:null-pointer))
	(val nil)
	(r-val nil))
    (cffi:with-foreign-objects ((x :int) (y :int))
      (case size
	(:x (setf p-x x))
	(:y (setf p-y y)))
      (setf r-val (SizeText font text p-x p-y))
      (if r-val
	  (cond
	    ((sdl:is-valid-ptr p-x)
	     (setf val (cffi:mem-aref p-x :int)))
	    ((sdl:is-valid-ptr p-y)
	     (setf val (cffi:mem-aref p-y :int))))
	  (setf val r-val)))
    val))

;;; m

(defun make-text-surface (text &key (font sdl-ttf:*default-font*) (color sdl:*default-color*))
  (Render-Text-Solid font text color))

;;; o

(defun open-font (filename size)
  "You must close the font to free it"
    (if (and (stringp filename) (probe-file filename))
	(OpenFont filename size)
	(error (concatenate 'string "Failed to open font in location: " filename))))


