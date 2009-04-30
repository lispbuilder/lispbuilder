
(in-package #:rm)

;; (defgeneric native-event-handler ()
;;   (:documentation "Selects the OS native event handler and windowing system.
;; Passed to RM:WITH-INIT as a parameter."))
;; (defgeneric generic-event-handler ()
;;   (:documentation "Selects the SDL event handler and windowing system.
;; Passed to RM:WITH-INIT as a parameter."))

(defgeneric make-window (event-handler width height &key pipe x y node parent-node))

(defgeneric free (obj))

(defgeneric width (obj))
(defgeneric height (obj))

(defgeneric assign-defaults (obj))

(defgeneric lum (color))
(defgeneric (setf lum) (value color))
(defgeneric alpha (color))
(defgeneric (setf alpha) (value color))
(defgeneric r (color))
(defgeneric (setf r) (value color))
(defgeneric g (color))
(defgeneric (setf g) (value color))
(defgeneric b (color))
(defgeneric (setf b) (value color))
(defgeneric a (color))
(defgeneric (setf a) (value color))

(defgeneric copy-color-to-foreign (color foreign &optional index))

(defgeneric copy-data (object)
  (:documentation "Returns the copy status of `OBJECT` as either `:RM-COPY-DATA`, or `:RM-DONT-COPY-DATA`."))

(defgeneric copy-p (object)
  (:documentation "Returns the copy status of objects as `T` if `:RM-COPY-DATA` is set,
or `NIL` if `:RM-DONT-COPY-DATA` is set."))

(defgeneric (setf copy-p) (value object)
  (:documentation "Sets `OBJECT`s copy status to `:RM-COPY-DATA` when `T` and `:RM-DONT-COPY-DATA` when `NIL`.
Turns on automatic garbage collection when `T`. Turns off automatic garbage collection when `NIL`"))

(defgeneric enable (obj))
(defgeneric disable (obj))
(defgeneric enable-p (obj))

(defgeneric ambient-color (obj))
(defgeneric (setf ambient-color) (color obj))
(defgeneric diffuse-color (obj))
(defgeneric (setf diffuse-color) (color obj))
(defgeneric specular-color (obj))
(defgeneric (setf specular-color) (color obj))
(defgeneric direction (obj))
(defgeneric (setf direction) (color obj))
(defgeneric cutoff (obj))
(defgeneric (setf cutoff) (color obj))
(defgeneric exponent (obj))
(defgeneric (setf exponent) (color obj))


(defgeneric attach-to-root-node (obj))
(defgeneric add-to-node (parent child))
;; (defgeneric add-node (parent child &key union compute-center))
;; (defgeneric add-primitive (node primitive &key compute-bounds center))
(defgeneric find-named-node (parent child))
(defgeneric delete-node (obj))
(defgeneric remove-child-node (parent child))
(defgeneric remove-sub-tree (this &optional parent))

(defgeneric remove-all-primitives (obj))
(defgeneric nodes-p (obj))
(defgeneric primitives-p (obj))
(defgeneric nth-node (node index))
(defgeneric nth-primitive (node index))
(defgeneric union-all-boxes (obj))
(defgeneric compute-center-from-bounding-box (obj))
(defgeneric bounding-box (obj))
(defgeneric (setf bounding-box) (bounds obj))

(defgeneric add-bounds (node &optional parent))

(defgeneric background-color (obj))
(defgeneric (setf background-color) (color obj))
(defgeneric unlit-color (obj))
(defgeneric (setf unlit-color) (color obj))
(defgeneric specular-exponent (obj))
(defgeneric (setf specular-exponent) (color obj))

(defgeneric normalize-normals (obj))
(defgeneric (setf normalize-normals) (color obj))

(defgeneric compute-bounding-box (obj))

(defgeneric traverse-p (obj))
(defgeneric (setf traverse-p) (color obj))

(defgeneric pick-p (obj))
(defgeneric (setf pick-p) (color obj))

(defgeneric (setf rgb/a) (color obj))
(defgeneric (setf xy/z) (vertex obj))

(defgeneric orientation (obj))
(defgeneric (setf orientation) (value obj))

(defgeneric (setf normals) (normals obj))

(defgeneric display-list-enable (obj value))

(defgeneric (setf radius) (radius obj))
(defgeneric (setf tesselate) (radius obj))

(defgeneric %render (obj))

(defgeneric add-scene (scene window &optional add-node))
(defgeneric find-scene (window id))
(defgeneric delete-scene (window scene))



(defgeneric compute-view-from-geometry (camera obj))

(defgeneric (setf camera) (camera scene))
(defgeneric camera (scene type))

(defgeneric assign-default-camera (node camera))

(defgeneric dimensions-from-window (window scene))

(defgeneric assign-default-lighting (obj))
(defgeneric light-p (node &optional light-source))
(defgeneric light (node light-source))
(defgeneric get-light (node light-source))
(defgeneric (setf light) (light obj))
(defgeneric remove-light (obj &optional light-source))

(defgeneric x (vertex))
(defgeneric (setf x) (value vertex))
(defgeneric y (vertex))
(defgeneric (setf y) (value vertex))
(defgeneric z (vertex))
(defgeneric (setf z) (value vertex))
(defgeneric w (vertex))
(defgeneric (setf w) (value vertex))

(defgeneric copy-vertex-to-foreign (vertex foreign &key index))

(defgeneric set-current (obj))

(defgeneric viewport (object))
(defgeneric (setf viewport) (value object))

(defgeneric gc-p (object))
(defgeneric (setf gc-p) (value object))

(defgeneric install-timer (window interval function))
(defgeneric clear-timer (window))

(defgeneric install-mouse-up-handler (window function))
(defgeneric install-mouse-down-handler (window function))
(defgeneric install-mouse-move-handler (window function))

(defgeneric wrap-node (parent this-node))

(defgeneric v2d* (size &key initial-element initial-contents))
(defgeneric v3d* (size &key initial-element initial-contents))
(defgeneric v4d* (size &key initial-element initial-contents))

(defgeneric c3d* (size &key initial-element initial-contents))
(defgeneric c4d* (size &key initial-element initial-contents))

(defgeneric copy-vertex (dst src &key start end))

(defgeneric new-quad-mesh-primitive (bounds &key primitive color orientation subdivisions sign))

(defgeneric init-meta-node (meta target info))

(defgeneric (setf display-info-p) (value meta))
(defgeneric display-info-p (meta))

(defgeneric pick-type (self))
(defgeneric picked-list (self))
(defgeneric (setf picked-list) (value self))
(defgeneric add-pick (meta &optional type))
(defgeneric remove-pick (meta))
(defgeneric delete-pick (meta))

(defgeneric clear-picked ())
(defgeneric get-picked (window node x y &key z-order))
(defgeneric find-node (meta))
(defgeneric set-client-data (meta client-data callback))
(defgeneric get-client-data (meta))

(defgeneric meta-p (node))

(defgeneric new-meta-node (&key name dims opacity))
(defgeneric add-to-meta-node (parent child))

(defgeneric (setf display-bounds-p) (value meta-node))
(defgeneric (setf display-name-p) (value meta-node))

(defgeneric reset-info-node (meta-node))

(defgeneric get-children (obj))
(defgeneric insert-node (target parent child))
(defgeneric dump-scene-graph (node))

(defgeneric (setf text) (text obj))

(defgeneric display-list-p (node))
(defgeneric (setf display-list-p) (value node))

(defgeneric (setf app-display-list) (value node))

(defgeneric nth-vertex (vertex-array index))

(defgeneric color-array (colors))
(defgeneric nth-color (colors index))
(defgeneric copy-color (dst src &key start end))

(defgeneric image-data (obj))
