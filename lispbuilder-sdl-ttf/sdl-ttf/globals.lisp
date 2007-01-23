
(in-package #:lispbuilder-sdl-ttf)

;;;; Globals

(defvar *default-font* nil
  "Whenever a LISPBUILDER-SDL-TTF function or macro has an optional or keyword argument named 
FONT, the default action is to bind to *default-font*.
*default-font* is set to NIL initially.
WITH-OPEN-FONT will bind *default-font* to a Truetype font.")
