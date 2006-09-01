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

(defun get-Font-Size (text &key size type (font sdl-ttf:*default-font*))
  (let ((p-w (cffi:null-pointer))
	(p-h (cffi:null-pointer))
	(val nil)
	(r-val nil))
    (cffi:with-foreign-objects ((w :int) (h :int))
      (case size
	(:w (setf p-w w))
	(:h (setf p-h h)))
      (case type
	(:TEXT (setf r-val (SizeText font text p-w p-h)))
	(:UTF8 (setf r-val (SizeUTF8 font text p-w p-h))))
      (if r-val
	  (cond
	    ((sdl:is-valid-ptr p-w)
	     (setf val (cffi:mem-aref p-w :int)))
	    ((sdl:is-valid-ptr p-h)
	     (setf val (cffi:mem-aref p-h :int))))
	  (setf val r-val)))
    val))

(defun get-font-style (&key (font sdl-ttf:*default-font*))
  (GetFontStyle font))

(defun get-font-height (&key (font sdl-ttf:*default-font*))
  (GetFontheight font))

(defun get-font-ascent (&key (font sdl-ttf:*default-font*))
  (GetFontAscent font))

(defun get-font-descent (&key (font sdl-ttf:*default-font*))
  (GetFontDescent font))

(defun get-font-line-skip (&key (font sdl-ttf:*default-font*))
  (GetFontLineSkip font))

(defun get-font-faces (&key (font sdl-ttf:*default-font*))
  (GetFontfaces font))

(defun is-font-face-fixed-width (&key (font sdl-ttf:*default-font*))
  (GetFontfaceisfixedwidth font))

(defun get-font-face-family-name (&key (font sdl-ttf:*default-font*))
  (GetFontfaceFamilyName font))

(defun get-font-face-style-name (&key (font sdl-ttf:*default-font*))
  (GetFontfaceStyleName font))

;;; o

(defun open-font (filename size)
  "You must close the font to free it"
    (if (and (stringp filename) (probe-file filename))
	(OpenFont filename size)
	(error (concatenate 'string "Failed to open font in location: " filename))))

;;; r

(defun render-font-solid (text &key
			  (type :text)
			  (font sdl-ttf:*default-font*)
			  (position sdl:*default-position*)
			  (surface sdl:*default-surface*)
			  (color sdl:*default-color*)
			  update-p)
  (case type
      (:text
       (sdl:with-surface ((Render-Text-Solid font text color))
	 (sdl:blit-surface :src sdl:*default-surface* :dst surface :dst-rect position :update-p update-p)))
      (:UTF8
       (sdl:with-surface ((Render-UTF8-Solid font text color))
	 (sdl:blit-surface :src sdl:*default-surface* :dst surface :dst-rect position :update-p update-p)))
      (:GLYPH
       (sdl:with-surface ((Render-Glyph-Solid font text color))
	 (sdl:blit-surface :src sdl:*default-surface* :dst surface :dst-rect position :update-p update-p)))
      (:UNICODE
       ;; (defun draw-text-UNICODE-solid (surface font text color position &key update-p)
       ;;   (let ((text-surf (RenderUNICODE-Solid font text color)))
       ;;     (if text-surf
       ;; 	(sdl:blit-surface text-surf surface :dst-rect position :update-p update-p))))
       nil)))

(defun render-font-shaded (text fg-color bg-color &key
			   (type :text)
			   (font sdl-ttf:*default-font*)
			   (position sdl:*default-position*)
			   (surface sdl:*default-surface*)
			   update-p)
  (case type
    (:text
     (sdl:with-surface ((Render-Text-shaded font text fg-color bg-color))
       (sdl:blit-surface :src sdl:*default-surface* :dst surface :dst-rect position :update-p update-p)))
    (:UTF8
     (sdl:with-surface ((Render-UTF8-shaded font text fg-color bg-color))
       (sdl:blit-surface :src sdl:*default-surface* :dst surface :dst-rect position :update-p update-p)))
    (:GLYPH
     (sdl:with-surface ((Render-Glyph-shaded font text fg-color bg-color))
       (sdl:blit-surface :src sdl:*default-surface* :dst surface :dst-rect position  :update-p update-p)))
    (:UNICODE
     ;; (defun draw-text-UNICODE-solid (surface font text color position &key update-p)
     ;;   (let ((text-surf (RenderUNICODE-Solid font text color)))
     ;;     (if text-surf
     ;; 	(sdl:blit-surface text-surf surface :dst-rect position :free-src t :update-p update-p))))
     nil)))

(defun render-font-blended (text &key
			    (type :text)
			    (font sdl-ttf:*default-font*)
			    (position sdl:*default-position*)
			    (surface sdl:*default-surface*)
			    (color sdl:*default-color*)
			    update-p)
  (case type
    (:text
     (sdl:with-surface ((Render-Text-blended font text color))
       (sdl:blit-surface :src sdl:*default-surface* :dst surface :dst-rect position  :update-p update-p)))
    (:UTF8
     (sdl:with-surface ((Render-UTF8-blended font text color))
       (sdl:blit-surface :src sdl:*default-surface* :dst surface :dst-rect position  :update-p update-p)))
    (:GLYPH
     (sdl:with-surface ((Render-Glyph-blended font text color))
       (sdl:blit-surface :src sdl:*default-surface* :dst surface :dst-rect position  :update-p update-p)))
    (:UNICODE
     ;; (defun draw-text-UNICODE-solid (surface font text color position &key update-p)
     ;;   (let ((text-surf (RenderUNICODE-Solid font text color)))
     ;;     (if text-surf
     ;; 	(sdl:blit-surface text-surf surface :dst-rect position  :update-p update-p))))
     nil)))

;;; s

(defun set-font-style (style &key (font sdl-ttf:*default-font*))
  (SetFontStyle font style))