
(in-package #:lispbuilder-sdl-ttf)

;;;; Globals

(defvar *default-font* nil
  "When a `LISPBUILDER-SDL-TTF` function or macro has an `OPTIONAL` or `KEY`word argument 
`FONT`, the default action is to bind to `\*DEFAULT-FONT\*`. `\*DEFAULT-FONT\*` is set to `NIL` initially.
A font must be initialized by calling [WITH-OPEN-FONT](#with-open-font), 
[INITIALISE-FONT](#initialise-font), or [INITIALISE-DEFAULT-FONT](#initialise-default-font). 
These will bind `\*DEFAULT-FONT\*` to a Truetype font `FONT`.")

;; (defvar *default-font-path* (or *load-truename* *default-pathname-defaults*)
;;   "Path to the default font, \"Bitstream Vera\"")

(defvar *default-font-path* (make-pathname :host (pathname-host #.(or *compile-file-truename*
								      *load-truename*))
					   :directory (pathname-directory #.(or *compile-file-truename*
										*load-truename*)))
  "The path to the default Truetype font, `\"Bitstream Vera\"`")

(defvar *generation* 0
  "Stores the number of times that `QUIT-TTF` is called.")