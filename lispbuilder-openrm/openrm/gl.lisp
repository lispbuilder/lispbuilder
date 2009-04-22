
(in-package #:rm)

;; From http://cl-cookbook.sourceforge.net/strings.html
(defun split-by-one-space (string)
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

(defmethod gl-get-string (name)
  (rm-cffi::glgetstring name))

(defmethod glu-get-string (name)
  (rm-cffi::glugetstring name))

(defmethod gl-version ()
  (gl-get-string rm-cffi::gl_version))
    
(defmethod gl-vendor ()
  (gl-get-string rm-cffi::gl_vendor))

(defmethod gl-renderer ()
  (gl-get-string rm-cffi::gl_renderer))

(defmethod gl-extensions ()
  (split-by-one-space (gl-get-string rm-cffi::gl_extensions)))

(defmethod glu-version ()
  (glu-get-string rm-cffi::glu_version))

(defmethod glu-extensions ()
  (split-by-one-space (glu-get-string rm-cffi::glu_extensions)))
