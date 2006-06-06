;; This file contains some useful functions for using SDL_ttf from Common lisp
;; 2006 (c) Rune Nesheim, see LICENCE.

(in-package #:lispbuilder-sdl-ttf)

;;; Macros

;;; w

(defmacro with-init (&body body)
  `(unwind-protect
     (progn 
       (init)
       ,@body)
     (Quit)))


;defmacro with-open-font
; I sugest a syntax like this, with variable capture of font:
;    (with-open-font ("foo.ttf" 12)
;      (let ((surf (make-text-surface font "There's good intensions
;                                           and there's good deeds,
;	    	                            and they're as far apart 
;                                           as heaven and hell"
;		                      255 255 255)))
;        (bar surf)))
;
; But each time I tried to write it I did something wrong.
; I'm stupid. -RN


;;; Functions

;;; d

(defun draw-text-solid (surface font text color position &key update-p)
  (let ((text-surf (Render-Text-Solid font text color)))
    (if text-surf
	(sdl:blit-surface text-surf surface :dst-rect position :free-src t :update-p update-p))))

(defun draw-text-UTF8-solid (surface font text color position &key update-p)
  (let ((text-surf (Render-UTF8-Solid font text color)))
    (if text-surf
	(sdl:blit-surface text-surf surface :dst-rect position :free-src t :update-p update-p))))

;; (defun draw-text-UNICODE-solid (surface font text color position &key update-p)
;;   (let ((text-surf (RenderUNICODE-Solid font text color)))
;;     (if text-surf
;; 	(sdl:blit-surface text-surf surface :dst-rect position :free-src t :update-p update-p))))

(defun draw-text-GLYPH-solid (surface font ch color position &key update-p)
  (let ((text-surf (Render-Glyph-Solid font ch color)))
    (if text-surf
	(sdl:blit-surface text-surf surface :dst-rect position :free-src t :update-p update-p))))

(defun draw-text-shaded (surface font text fg-color bg-color position &key update-p)
  (let ((text-surf (Render-Text-shaded font text fg-color bg-color)))
    (if text-surf
	(sdl:blit-surface text-surf surface :dst-rect position :free-src t :update-p update-p))))

(defun draw-text-UTF8-shaded (surface font text fg-color bg-color position &key update-p)
  (let ((text-surf (Render-UTF8-shaded font text fg-color bg-color)))
    (if text-surf
	(sdl:blit-surface text-surf surface :dst-rect position :free-src t :update-p update-p))))

;; (defun draw-text-UNICODE-solid (surface font text color position &key update-p)
;;   (let ((text-surf (RenderUNICODE-Solid font text color)))
;;     (if text-surf
;; 	(sdl:blit-surface text-surf surface :dst-rect position :free-src t :update-p update-p))))

(defun draw-text-GLYPH-shaded (surface font ch fg-color bg-color position &key update-p)
  (let ((text-surf (Render-Glyph-shaded font ch fg-color bg-color)))
    (if text-surf
	(sdl:blit-surface text-surf surface :dst-rect position :free-src t :update-p update-p))))

(defun draw-text-blended (surface font text color position &key update-p)
  (let ((text-surf (Render-Text-blended font text color)))
    (if text-surf
	(sdl:blit-surface text-surf surface :dst-rect position :free-src t :update-p update-p))))

(defun draw-text-UTF8-blended (surface font text color position &key update-p)
  (let ((text-surf (Render-UTF8-blended font text color)))
    (if text-surf
	(sdl:blit-surface text-surf surface :dst-rect position :free-src t :update-p update-p))))

;; (defun draw-text-UNICODE-solid (surface font text color position &key update-p)
;;   (let ((text-surf (RenderUNICODE-Solid font text color)))
;;     (if text-surf
;; 	(sdl:blit-surface text-surf surface :dst-rect position :free-src t :update-p update-p))))

(defun draw-text-GLYPH-blended (surface font ch color position &key update-p)
  (let ((text-surf (Render-Glyph-blended font ch color)))
    (if text-surf
	(sdl:blit-surface text-surf surface :dst-rect position :free-src t :update-p update-p))))

;;; g

(defun get-Glyph-Metric (font ch &key metric)
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

(defun get-Size-Text (font text &key size)
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

(defun get-Size-UTF8 (font text &key size)
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

(defun get-Size-UNICODE (font text &key size)
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

(defun make-text-surface (font text color)
  (Render-Text-Solid font text color))

;;; o

(defun open-font (filename size)
  "You must close the font to free it"
    (if (and (stringp filename) (probe-file filename))
	(OpenFont filename size)
	(error (concatenate 'string "Failed to open font in location: " filename))))


