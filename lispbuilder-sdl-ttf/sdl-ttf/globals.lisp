
(in-package #:lispbuilder-sdl-ttf)

;;;; Globals

(defvar *default-font* nil
  "When a LISPBUILDER-SDL-TTF function or macro has an &OPTIONAL or :KEYword argument 
FONT, the default action is to bind to *default-font*.
*default-font* is set to NIL initially.
WITH-OPEN-FONT will bind *default-font* to a Truetype font.")

;; (defvar *default-font-path* (or *load-truename* *default-pathname-defaults*)
;;   "Path to the default font, \"Bitstream Vera\"")

(defvar *default-font-path* (make-pathname :host (pathname-host #.(or *compile-file-truename*
								      *load-truename*))
					   :directory (pathname-directory #.(or *compile-file-truename*
										*load-truename*)))
  "The path to the default font, \"Bitstream Vera\"")

(defvar *ttf-init-on-startup* nil
  "The function called in SDL:INIT-SDL. Initialises the font library.")

(defvar *ttf-quit-on-exit* nil
  "The function called in SDL:QUIT-SDL. Uninitialises the font library.")