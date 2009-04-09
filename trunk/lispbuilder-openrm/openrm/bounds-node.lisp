
(in-package #:lispbuilder-openrm)

(defclass bounds-node (node)
  ((bounds-node :accessor bounds-node :initarg :bounds-node)
   (target-node :accessor target-node :initarg :target-node)))

(defmethod add-bounds ((data node) &optional (root-node (make-instance 'node)))
  (let* ((bounds (make-instance 'node))
	 (wrapper (make-instance 'bounds-node
				 :root-node root-node
				 :bounds-node bounds
				 :target-node data)))
    ;; Turn of picking for the bounds node.
    (rm-cffi::rm-node-set-pick-enable (fp bounds) t)

    (add-to-node root-node bounds)
    (add-to-node root-node data)
    
    (let ((bounds-prim (make-instance 'rm::box-wire-primitive)))
      (compute-bounding-box data)
      (setf (xy/z bounds-prim) (bounding-box data))
      (add-to-node bounds bounds-prim))
    
    (compute-bounding-box data)
    (union-all-boxes root-node)
    (compute-center-from-bounding-box root-node)
    wrapper))

(defmethod fp ((self bounds-node))
  (fp (root-node self)))

(defmethod (setf bounds-p) (value (self bounds-node))
  (rm-cffi::rm-node-set-traverse-enable (fp self) value))
(defmethod bounds-p ((self bounds-node))
  (rm-cffi::rm-node-get-traverse-enable (fp self)))
