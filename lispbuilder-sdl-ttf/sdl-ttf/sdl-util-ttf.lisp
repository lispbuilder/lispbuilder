;; This file contains some useful functions for using SDL_ttf from Common lisp
;; 2006 (c) Rune Nesheim, see LICENCE.
;; 2007 (c) Luke Crook

(in-package #:lispbuilder-sdl-ttf)


;;; Add INIT-TTF to LISPBUILDER-SDL's external initialization list.
;;; Functions in this list are called within the macro SDL:WITH-INIT, and the function SDL:INIT-SDL 
(pushnew 'init-ttf sdl:*external-init-on-startup*)

;;; Add the QUIT-TTF to LISPBUILDER-SDL's external uninitialization list.
;;; Functions in this list are called when the macro SDL:WITH-INIT exits, and the function SDL:QUIT-SDL 
(pushnew 'quit-ttf sdl:*external-quit-on-exit*)

(defun is-init ()
  "Queries the initialization status of the `SDL_TTF` truetype library. 
Returns the current *GENERATION* if this library is already initialized and `NIL` if uninitialized."
  (if (sdl-ttf-cffi::ttf-was-init)
      *generation*
      nil))

(defun open-font (filename size)
  "Creates a new `FONT` from the file loaded at the location specified by `FILENAME`. 
NOTE: Does not bind the new `FONT` to `\*DEFAULT-FONT\*`. Does not attempt to initialize the `SDL_TTF` 
truetype library if the library is uninitialised. 

##### Parameters

* `FILENAME` is the name of the truetype font to be loaded, of type `STRING` 
* `SIZE` is the size of the `FONT`, as an `INTEGER`. 

##### Returns

* Returns new `FONT` object if successful, returns `NIL` if unsuccessful."
  (make-instance 'ttf-font :fp (sdl-ttf-cffi::ttf-Open-Font (namestring filename) size)))

(defmacro with-open-font ((font-name size) &body body)
  "This is a convenience macro that will first attempt to initialize the truetype font library and if successful, 
will then open the font in the file `FONT-NAME` prior to evaluating the forms in `BODY`. 
Will exit if the library cannot be initialized or the `FONT` cannot be opened.  Closes `FONT` after the forms in 
`BODY` have evaluated. 

Binds `FONT` to a shadowed instance of `\*DEFAULT-FONT\*` valid within the scope of `WITH-OPEN-FONT`. 
`WITH-OPEN-FONT` calls may be nested.

##### Parameters

* `FONT-NAME` is the file name of the truetype font to be opened, of type `STRING`.
* `SIZE` is the `INTEGER` point size of the font."
  (let ((font (gensym "font-")))
    `(let* ((,font (initialise-font ,font-name ,size)))
       (sdl:with-default-font (,font)
         ,@body)
       (sdl:free ,font)
       (when (eq sdl:*default-font* ,font)
         (setf sdl:*default-font* nil)))))

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

(defun initialise-font (filename size)
  "Creates a new `FONT` object of size `SIZE` loaded from the file at location `FILENAME`.
Automatically initialises the `SDL_TTF` truetype font library if the library is uninitialised when `FONT` is loaded. 

##### Parameters

* `FILENAME` is the file name of the `FONT`, of type `STRING`.
* `SIZE` is the `INTEGER` point size of the new `FONT`.

##### Returns

* Returns a new `FONT`, or `NIL` if unsuccessful."
  (unless (is-init)
    (init-ttf))
  (open-font filename size))

(defun initialise-default-font (&key (filename (merge-pathnames "Vera.ttf" sdl:*default-font-path*)) (size 32))
  "Creates a new `FONT` object that is loaded from the file at location `FILENAME`, and binds this to the symbol 
`\*DEFAULT-FONT\*`. Although several truetype fonts may used within a single SDL application, only a single 
`FONT` may be bound to the symbol `*\DEFAULT-FONT\*` at any one time.

##### Parameters

* `FILENAME` is the optional file name of the font, as a STRING. Default value is `Vera.ttf`.
* `SIZE` is the `INTEGER` point size of the new `FONT`.

##### Returns

* Returns a new `FONT`, or `NIL` if unsuccessful."
  (set-default-font (initialise-font filename size)))

(defun close-font (&key font sdl:*default-font*)
  "Closes the font `FONT` when the `SDL_TTF` font library is intitialized. 
NOTE: `CLOSE-FONT` does not uninitialise the font library, and does not bind `\*DEFAULT-FONT\*` to `NIL`. Returns `T` 
if successful, or `NIL` if the font cannot be closed or the `SDL_TTF` font library is not initialized."
  (check-type font ttf-font)
  (sdl:free font))

;;; g

(in-package #:lispbuilder-sdl-ttf)

(defmethod _get-Glyph-Metric_ ((font ttf-font) ch metric)
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

(defmethod _get-Font-Size_ ((font ttf-font) text size)
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

(defmethod _get-font-style_ ((font ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-Style (sdl:fp font)))

(defmethod _get-font-height_ ((font ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-height (sdl:fp font)))

(defmethod _get-font-ascent_ ((font ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-Ascent (sdl:fp font)))

(defmethod _get-font-descent_ ((font ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-Descent (sdl:fp font)))

(defmethod _get-font-line-skip_ ((font ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-Line-Skip (sdl:fp font)))

(defmethod _get-font-faces_ ((font ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-faces (sdl:fp font)))

(defmethod _is-font-face-fixed-width_ ((font ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-face-is-fixed-width (sdl:fp font)))

(defmethod _get-font-face-family-name_ ((font ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-face-Family-Name (sdl:fp font)))

(defmethod _get-font-face-style-name_ ((font ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-face-Style-Name (sdl:fp font)))

;;; s

(defmethod _set-font-style_ ((font ttf-font) style)
  (sdl-ttf-cffi::ttf-Set-Font-Style (sdl:fp font) style))

(defun create-path (filename)
  (namestring (merge-pathnames filename sdl:*default-font-path*)))
