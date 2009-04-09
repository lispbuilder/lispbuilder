(in-package #:lispbuilder-openrm)

(defclass meta-node (node)
  ((target-node
    :accessor target-node
    :initform nil)
   (info-node
    :accessor info-node
    :initform nil)
   (display-meta
    :reader display-meta-p
    :initform nil))
  (:default-initargs
   :gc t))

(defclass meta-bounds-node (meta-node)
  ((bounds-primitive
    :accessor bounds-primitive
    :initform (make-instance 'box-wire-primitive))
   (display-bounds
    :reader display-bounds-p
    :initform nil
    :initarg :display-bounds))
  (:default-initargs
   :gc t
    :display-meta t
    :display-bounds t))

(defclass meta-spotlight-node (meta-node)
  ((light-primitive
    :accessor light-primitive
    :initform nil)
   (display-cone
    :reader display-cone-p
    :initform nil
    :initarg :display-cone)
   (cone-light-source
    :reader cone-light-source
    :initform nil
    :initarg :cone-light-source))
  (:default-initargs
   :gc t
    :display-meta t
    :display-cone t
    :dims :rm-renderpass-3d
    :opacity :rm-renderpass-opaque))

(defmethod initialize-instance :around ((self meta-node)
				       &key (display-meta nil))
  (call-next-method)
  (setf (display-meta-p self) display-meta))

(defmethod initialize-instance :after ((self meta-node)
				       &key)
  (add-target-to-meta-node self (make-instance 'node
					       :name (format nil "target:~A" (name self))
					       :dims (dims self)
					       :opacity (opacity self)))
  (add-info-to-meta-node self (make-instance 'node
					     :name (format nil "info:~A" (name self))
					     :dims (dims self)
					     :opacity (opacity self))))

(defmethod initialize-instance :after ((self meta-bounds-node)
				       &key (display-bounds nil))

  (add-to-node (info-node self) (bounds-primitive self))
  (setf (display-bounds-p self) display-bounds))


(defmethod fp ((self meta-node))
  (fp (target-node self)))

(defmethod (setf xy/z) ((translate v3d) (node meta-node))
  (rm-cffi::rm-node-set-translate-vector (this-fp node) (fp translate)))
(defmethod (setf xy/z+) ((xyz v3d) (node meta-node))
  (rm-base:with-v3d (translate nil t)
    (when (rm-cffi::rm-Node-Get-Translate-Vector (this-fp node) translate)
      (rm-cffi::rm-Node-Set-Translate-Vector (this-fp node) (rm-base::v3d+ translate xyz)))))

(defun add-target-to-meta-node (meta target)
  (setf (target-node meta) target)
  ;; Must be added to *rm-objects* or the fiinalization may be called
  ;; out of order.
  (add-to-rm-objects target)
  (rm-cffi::rm-Node-Add-Child (this-fp meta) (this-fp target)))

(defun add-info-to-meta-node (meta info)
  (setf (info-node meta) info)
  ;; Must be added to *rm-objects* or the fiinalization may be called
  ;; out of order.
  (add-to-rm-objects info)
  (rm-cffi::rm-Node-Add-Child (this-fp meta) (this-fp info)))

(defmethod add-to-node ((parent rm-node) (child meta-node))
  (rm-cffi::rm-Node-Add-Child (fp parent) (this-fp child))
  (setf (gethash (id child) *rm-objects*) child)
  (log5:log-for (info) "ADD-TO-NODE\(META-NODE\):NODE->CHILD: parent: ~A, child: ~A" (name parent) (name child))
  parent)

(defmethod delete-node ((self meta-node))
  ;; Remove target and info nodes from the meta-node
  (rm-cffi::rm-node-remove-child (this-fp self) (fp (target-node self)))    
  (rm-cffi::rm-node-remove-child (this-fp self) (fp (info-node self)))
  ;; Delete the target and info nodes
  (delete-node (target-node self))
  (delete-node (info-node self))
  ;; Finally delete the meta-node
  (rm-cffi::rm-node-delete (this-fp self))
  (when (= 0 (cffi:foreign-slot-value (this-fp self) 'rm-cffi::rm-node 'rm-cffi::refcount))
    ;; (remhash (id self) *rm-picked*)
    ;; (remhash (id self) *rm-objects*)
    )
  self)

(defmethod remove-child-node ((parent rm-node) (child meta-node))
  "Remove CHILD META-NODE from PARENT."
  (log5:log-for (info) "remove-child-node.META-NODE:PARENT:\(~A:~A\), CHILD:\(~A:~A\)" parent (name parent) child (name child))
  (if (rm-cffi::rm-node-remove-child (fp parent) (this-fp child))
      (when (= 0 (cffi:foreign-slot-value (this-fp child) 'rm-cffi::rm-node 'rm-cffi::refcount))
	;; (remhash (id child) *rm-picked*)
;; 	(remhash (id child) *rm-objects*)
	)
      nil))

(defmethod remove-sub-tree ((this meta-node) &optional parent)
  "Removes the META-NODE below THIS from the scene graph.
Optionally removes THIS as well."
  (log5:log-for (info) "remove-sub-tree.META-NODE: \(~A:~A\) &optional \(~A:~A\)" this (name this) parent (when parent (name parent)))
  (cons (list parent this) (loop for child in (get-children this)
			      appending (remove-sub-tree child this))))

(defmethod list-tree ((this meta-node) &optional parent all)
  "Returns as a LIST each PARENT->CHILD relationship in the scene-graph, rooted at SELF."
  (log5:log-for (info) "list-tree.META-NODE: \(~A:~A\) &optional \(~A:~A\)" this (name this) parent (when parent (name parent)))
  (if parent
    (cons (list parent this) (loop for child in (if all
						    (get-meta-children this)
						    (get-children this))
				appending (list-tree child this all)))
    (loop for child in (if all
			   (get-meta-children this)
			   (get-children this))
       appending (list-tree child this all))))

(defmethod meta-nodes-p ((self meta-node))
  (rm-cffi::rm-Node-Get-Num-Children (this-fp self)))

(defmethod meta-nth-node ((self meta-node) index)
  (let ((n (rm-cffi::rm-Node-Get-Ith-Child (this-fp self) index)))
    (if (cffi:null-pointer-p n)
	nil
	(find-node (get-client-data n)))))

(defmethod get-meta-children ((self meta-node))
  (remove nil (loop for i from 0 below (meta-nodes-p self)
		 collecting (meta-nth-node self i))))

(defmethod compute-bounding-box :after ((self meta-bounds-node))
  ;; Reconfigure the info-node according
  ;; to the new bounds of the target-node.
  ;;(setf (requires-update? self) t)
  (refresh-info-node self)
  self)

(defmethod (setf bounding-box) :after ((bounds v3d*) (self meta-bounds-node))
  ;; Reconfigure the info-node according
  ;; to the new bounds of the target-node.
  ;(setf (requires-update? self) t)
  (refresh-info-node self)
  self)

(defmethod (setf display-bounds-p) (value (self meta-node))
  (setf (slot-value self 'display-bounds) value)
  (when value
    (refresh-info-node self))
  self)

;; (defmethod (setf display-name-p) (value (self meta-node))
;;   (setf (slot-value self 'display-name) value)
;;   (when value
;;     (refresh-info-node self))
;;   self)

(defmethod refresh-info-node ((meta-node meta-node)))

(defmethod refresh-info-node :after ((meta-node meta-bounds-node))
  (let ((info-node (info-node meta-node))
	(target-node (target-node meta-node)))
    (declare (ignore info-node))

    (when (and (display-meta-p meta-node)
	       (bounding-box target-node))
      
      (when (display-bounds-p meta-node)
	(let ((bounds-primitive (bounds-primitive meta-node)))
	  ;; Set the TARGET-NODE bounding box wireframe.    
	  (setf (xy/z bounds-primitive) (bounding-box target-node))
	  ;; (setf (rgb/a bounds-primitive) (rm::c4d 0.0 0.0 0.0 1.0))
	  (compute-bounding-box bounds-primitive))))))

;; (defmethod refresh-info-node :after ((meta-node meta-spotlight-node))
;;   (let ((info-node (info-node meta-node))
;; 	(target-node (target-node meta-node)))
;;     (when (display-meta-p meta-node)    
;;       (when (display-cone-p meta-node)
;; 	(fformat "   show-info-node display-spotlight-p")
;; 	(let ((light-primitive (light-primitive meta-node))
;; 	      (spotlight (light target-node)))
;; 	  (setf (xy/z light-primitive) (xy/z spotlight))
;; 	  (setf (diffuse-color info-node) (diffuse-color target-node)
;; 		(ambient-color info-node) (ambient-color target-node)
;; 		(specular-color info-node) (specular-color target-node))
	  
;; 	  (compute-bounding-box bounds-primitive))))))


;; (defmethod refresh-info-node ((meta-node meta-node))
;;   (let ((info-node (info-node meta-node))
;; 	(target-node (target-node meta-node)))
;;     (declare (ignore info-node))

;;     (when (and (display-meta-p meta-node)
;; 	       (bounding-box target-node))
      
;;       (when (display-bounds-p meta-node)
;; 	(fformat "   show-info-node display-bounds-p")
;; 	(let ((bounds-primitive (bounds-primitive meta-node)))
;; 	  ;; Set the TARGET-NODE bounding box wireframe.    
;; 	  (setf (xy/z bounds-primitive) (bounding-box target-node))
;; 	  (setf (rgb/a bounds-primitive) (rm::c4d 0.0 0.0 0.0 1.0))
;; 	  (compute-bounding-box bounds-primitive)))

;;       (when (display-name-p meta-node)
;; 	(fformat "   show-info-node: display-name-p")
;; 	(let ((name-primitive (name-primitive meta-node)))
;; 	  ;; Set the TARGET-NODE name.
;; 	  (setf (xy/z name-primitive) (bounding-box target-node))
;; 	  (setf (text name-primitive) (list (name meta-node)
;; 					    (name meta-node)))
;; 	  (compute-bounding-box name-primitive))))))

(defmethod (setf display-meta-p) (value (self meta-node))
  (setf (slot-value self 'display-meta) value)
  (if value
      (progn
	(setf (traverse (info-node self)) t)
	(refresh-info-node self))
      (setf (traverse (info-node self)) nil))
  self)

(defmethod meta-p ((node rm-node)) nil)
(defmethod meta-p ((node meta-node)) t)

;;; Matrix Operations



