;; This file contains some useful functions for using SDL_ttf from Common lisp
;; 2006 (c) Rune Nesheim, see LICENCE.
;; 2007 (c) Luke Crook

(in-package #:lispbuilder-sdl-ttf)

;;; Add INIT-TTF to LISPBUILDER-SDL's external initialization list.
;;; Functions in this list are called within the macro SDL:WITH-INIT, and the function SDL:INIT-SDL 
(pushnew 'init-ttf sdl:*external-init-subsystems-on-startup*)

;;; Add the QUIT-TTF to LISPBUILDER-SDL's external uninitialization list.
;;; Functions in this list are called when the macro SDL:WITH-INIT exits, and the function SDL:QUIT-SDL 
(pushnew 'quit-ttf sdl:*external-quit-subsystems-on-exit*)

(defun is-init ()
  "Queries the initialization status of the `SDL_TTF` truetype library. 
Returns the current *GENERATION* if this library is already initialized and `NIL` if uninitialized."
  (if (sdl-ttf-cffi::ttf-was-init)
      *generation*
      nil))

(defun open-font (definition)
  "Creates a new `FONT` from the file loaded at the location specified by `FILENAME`. 
NOTE: Does not bind the new `FONT` to `\*DEFAULT-FONT\*`. Does not attempt to initialize the `SDL_TTF` 
truetype library if the library is uninitialised. 

##### Parameters

* `FILENAME` is the name of the truetype font to be loaded, of type `STRING` 
* `SIZE` is the size of the `FONT`, as an `INTEGER`. 

##### Returns

* Returns new `FONT` object if successful, returns `NIL` if unsuccessful."
  (make-instance 'sdl:ttf-font
                 :fp (sdl-ttf-cffi::ttf-Open-Font(sdl::filename definition)
                                                 (sdl::char-size definition))
                 :font-definition definition))

;;; Functions

(defun init-ttf ()
  "Initializes the `SDL_TTF` font library if uninitialized. Increments and returns *GENERATION* if the library 
was uninitialized and is successfully initialized, or else returns `NIL` if uninitialized."
  (if (is-init)
      *generation*
      (progn
	(if (sdl-ttf-cffi::ttf-init)
	    (incf *generation*)
	    nil))))

(defun quit-ttf ()
  "Uninitializes the `SDL_TTF` font library if already initialized."
  (if (is-init)
      (sdl-ttf-cffi::ttf-quit)))

(defun close-font (&key font sdl:*default-font*)
  "Closes the font `FONT` when the `SDL_TTF` font library is intitialized. 
NOTE: `CLOSE-FONT` does not uninitialise the font library, and does not bind `\*DEFAULT-FONT\*` to `NIL`. Returns `T` 
if successful, or `NIL` if the font cannot be closed or the `SDL_TTF` font library is not initialized."
  (check-type font sdl:ttf-font)
  (sdl:free font))

;;; g

(in-package #:lispbuilder-sdl-ttf)

(defmethod _get-Glyph-Metric_ ((font sdl:ttf-font) ch metric)
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
      (setf r-val (sdl-ttf-cffi::ttf-Glyph-Metrics font ch p-minx p-maxx p-miny p-maxy p-advance))
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

(defmethod _get-Font-Size_ ((font sdl:ttf-font) text size)
  (let ((p-w (cffi:null-pointer))
	(p-h (cffi:null-pointer))
	(val nil)
        (r-val nil))
    (cffi:with-foreign-objects ((w :int) (h :int))
      (case size
	(:w (setf p-w w))
	(:h (setf p-h h)))
      (setf r-val (sdl-ttf-cffi::ttf-Size-UTF8 (sdl:fp font) text p-w p-h))
      (if r-val
	  (cond
	    ((sdl:is-valid-ptr p-w)
	     (setf val (cffi:mem-aref p-w :int)))
	    ((sdl:is-valid-ptr p-h)
	     (setf val (cffi:mem-aref p-h :int))))
	  (setf val r-val)))
    val))

(defmethod _get-font-style_ ((font sdl:ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-Style (sdl:fp font)))

(defmethod _get-font-height_ ((font sdl:ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-height (sdl:fp font)))

(defmethod _get-font-ascent_ ((font sdl:ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-Ascent (sdl:fp font)))

(defmethod _get-font-descent_ ((font sdl:ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-Descent (sdl:fp font)))

(defmethod _get-font-line-skip_ ((font sdl:ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-Line-Skip (sdl:fp font)))

(defmethod _get-font-faces_ ((font sdl:ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-faces (sdl:fp font)))

(defmethod _is-font-face-fixed-width_ ((font sdl:ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-face-is-fixed-width (sdl:fp font)))

(defmethod _get-font-face-family-name_ ((font sdl:ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-face-Family-Name (sdl:fp font)))

(defmethod _get-font-face-style-name_ ((font sdl:ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-face-Style-Name (sdl:fp font)))

;;; s

(defmethod _set-font-style_ ((font sdl:ttf-font) style)
  (sdl-ttf-cffi::ttf-Set-Font-Style (sdl:fp font) style))

(defun create-path (filename)
  (namestring (merge-pathnames filename sdl:*default-font-path*)))
