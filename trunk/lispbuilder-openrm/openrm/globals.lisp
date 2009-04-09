
(in-package #:rm)

(defvar *rm-root-node* nil)
(defvar *current-window* nil)
(defvar *current-camera* nil)

(defvar *default-primitive* nil)
(defvar *default-node* nil)
(defvar *in-default-node-scope* nil)

(defvar *primitive-list* nil)
(defvar *node-list* nil)
(defvar *parent-node* nil)

(defvar *window* nil)
(defvar *windows* nil)
(defvar *rm-objects* (make-hash-table))
(defvar *rm-picked* (make-hash-table))
(defvar *initialised* nil)
(defvar *type* nil)


(defconstant +mouse-left-button+ #x1)
(defconstant +mouse-right-button+ #x2)
(defconstant +mouse-middle-button+ #x4)

(defconstant +key-modifier-shift+ #x8)
(defconstant +key-modifier-control+ #x10)


(defun set-compute-bounding-box (&optional (obj (or *default-primitive* *default-node*)))
  (declare (ignore obj))
  (error "SET-COMPUTE-BOUNDING-BOX must be called within the scope of WITH-DEFAULT-NODE."))

(defun set-center (vertex &optional (node (or *default-node* *default-primitive*)))
  (declare (ignore vertex node))
  (error "SET-CENTER must be called within the scope of WITH-DEFAULT-NODE."))

(defun set-rgb/a (color &optional (primitive *default-primitive*))
  (declare (ignore color primitive))
  (error "SET-RGB/A must be called within the scope of WITH-DEFAULT-PRIMITIVE."))

(defun set-xy/z (vertex &optional (primitive *default-primitive*))
  (declare (ignore vertex primitive))
  (error "SET-XY/Z must be called within the scope of WITH-DEFAULT-PRIMITIVE."))

(defun set-normals (normals &optional (primitive *default-primitive*))
  (declare (ignore normals primitive))
  (error "SET-NORMALS must be called within the scope of WITH-DEFAULT-PRIMITIVE."))

(defun set-bounding-box (min max &optional (primitive *default-primitive*))
  (declare (ignore min max primitive))
  (error "SET-BOUNDING-BOX must be called within the scope of WITH-DEFAULT-PRIMITIVE."))
(defun get-bounding-box (&optional (primitive *default-primitive*))
  (declare (ignore primitive))
  (error "GET-BOUNDING-BOX must be called within the scope of WITH-DEFAULT-PRIMITIVE."))

(defun set-radius (radius &optional bounding-box (primitive *default-primitive*))
  (declare (ignore radius bounding-box primitive))
  (error "SET-RADIUS must be called within the scope of WITH-DEFAULT-PRIMITIVE."))

(defun set-tesselate (tesselate &optional (primitive *default-primitive*))
  (declare (ignore tesselate primitive))
  (error "SET-TESSELATE must be called within the scope of WITH-DEFAULT-PRIMITIVE."))

(defun set-normalize-normals (&optional (node *default-node*))
  (declare (ignore node))
  (error "SET-NORMALIZE-NORMALS must be called within the scope of WITH-DEFAULT-PRIMITIVE."))

(defun set-compute-center-from-bounding-box (&optional (node *default-node*))
  (declare (ignore node))
  (error "SET-COMPUTE-CENTER-FROM-BOUNDING-BOX must be called within the scope of WITH-DEFAULT-PRIMITIVE."))

(defun set-union-all-boxes (&optional (node *default-node*))
  (declare (ignore node))
  (error "SET-UNION-ALL-BOXES must be called within the scope of WITH-DEFAULT-PRIMITIVE."))

(defun set-delete-all-primitives (&optional (node *default-node*))
  (declare (ignore node))
  (error "SET-DELETE-ALL-PRIMITIVES must be called within the scope of WITH-DEFAULT-PRIMITIVE."))

(defun set-delete-all-nodes (&optional (node *default-node*))
  (declare (ignore node))
  (error "SET-DELETE-ALL-NODES must be called within the scope of WITH-DEFAULT-PRIMITIVE."))

(defun set-add-to-node (obj &optional (node (or *default-node* *default-primitive*)))
  (declare (ignore obj node))
  (error "SET-ADD-TO-NODE must be called within the scope of WITH-DEFAULT-PRIMITIVE."))

(defun set-copy-data (obj &optional (primitive *default-primitive*))
  (declare (ignore obj primitive))
  (error "SET-COPY-DATA must be called within the scope of WITH-DEFAULT-PRIMITIVE."))

(defun set-text (obj &optional (primitive *default-primitive*))
  (declare (ignore obj primitive))
  (error "SET-TEXT must be called within the scope of WITH-DEFAULT-PRIMITIVE."))

(defun set-display-list-p (obj &optional (primitive *default-primitive*))
  (declare (ignore obj primitive))
  (error "SET-DISPLAY-LIST-P must be called within the scope of WITH-DEFAULT-PRIMITIVE."))

(defun set-app-display-list (obj &optional (primitive *default-primitive*))
  (declare (ignore obj primitive))
  (error "SET-APP-DISPLAY-LIST must be called within the scope of WITH-DEFAULT-PRIMITIVE."))

