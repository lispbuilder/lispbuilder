
(in-package #:lispbuilder-openrm)

(defclass node-with-bounds (node)
  ((root-node :accessor root-node :initarg :root-node)
   (bounds-node :accessor bounds-node :initarg :bounds-node)
   (data-node :accessor data-node :initarg :data-node)))

(defmethod bounds-node ((root rm-node) (data node))
  (let* ((bounds (make-instance 'node))
	 (wrapper (make-instance 'node-with-bounds
				 :root-node root
				 :bounds-node bounds
				 :data-node data)))
    ;; Turn of picking for the bounds node.
    (rm-cffi::rm-node-set-pick-enable (fp bounds) nil)

    (add-to-node root bounds)
    (add-to-node root data)
    
    (let ((bounds-prim (make-instance 'rm::box-wire-primitive)))
      (compute-bounding-box data)
      (setf (xy/z bounds-prim) (bounding-box data))
      (add-to-node bounds bounds-prim))
    
    (compute-bounding-box data)
    (union-all-boxes root)
    (compute-center-from-bounding-box root)
    wrapper))

(defmethod fp ((self node-with-bounds))
  (fp (root-node self)))

(defmethod (setf bounds-p) (value (self bounds-node))
  (rm-cffi::rm-node-set-traverse-enable (fp self) (value)))
(defmethod bounds-p ((self bounds-node))
  (rm-cffi::rm-node-get-traverse-enable (fp self)))

