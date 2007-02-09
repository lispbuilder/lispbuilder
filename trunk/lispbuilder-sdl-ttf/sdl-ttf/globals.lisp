
(in-package #:lispbuilder-sdl-ttf)

;;;; Globals

(defvar *default-font* nil
  "When a LISPBUILDER-SDL-TTF function or macro has an &OPTIONAL or :KEYword argument 
FONT, the default action is to bind to *DEFAULT-FONT*.
*DEFAULT-FONT* is set to NIL initially.
WITH-OPEN-FONT, INITIALISE-FONT, AND INITIALISE-DEFAULT-FONT will bind *default-font* to a Truetype font FONT.")

;; (defvar *default-font-path* (or *load-truename* *default-pathname-defaults*)
;;   "Path to the default font, \"Bitstream Vera\"")

(defvar *default-font-path* (make-pathname :host (pathname-host #.(or *compile-file-truename*
								      *load-truename*))
					   :directory (pathname-directory #.(or *compile-file-truename*
										*load-truename*)))
  "The path to the default font, \"Bitstream Vera\"")

(defvar *generation* 0
  "Stores the number of times QUIT-TTF is called.")