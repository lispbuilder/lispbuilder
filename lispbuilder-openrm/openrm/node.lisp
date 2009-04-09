(in-package #:rm)

(defclass rm-node (openrm-object)
  ((name
    :reader name
    :initform ""
    :initarg :name)))

(defclass node (rm-node)
  ((dims
    :reader dims
    :initform :rm-renderpass-all
    :initarg :dims)
   (opacity
    :reader opacity
    :initform :rm-renderpass-all
    :initarg :opacity))
  (:default-initargs
   :gc t
   :free (simple-free 'rm-cffi::rm-node-delete 'rm-node)))

(defclass root-node (rm-node) ()
  (:default-initargs
   :name "root-node"
   :gc nil
    :free #'(lambda (node-fp)
	      (declare (ignore node-fp))
	      ;; Do Nothing. We should not be here.
	      nil)))

(defun free-node (node-fp)
  (when (is-valid-ptr node-fp)
    (when (cffi:pointer-eq (rm-cffi::rm-root-node) node-fp)
      (error "Cannot delete the RM Root Node."))
    (rm-cffi::rm-node-delete node-fp)))

(defmethod free ((self root-node))
  "A primitive is eligible for automatic garbage collection up until it is added to the scene graph.
Thereafter the primitive must be explicitly freed by the user. The node is again eligible for garbage collection
when subsequently removed from the scene graph and its reference count is zero."
  nil)

(defmethod initialize-instance :after ((self node)
                                       &key (node nil))
  (if (this-fp self)
    (log5:log-for (create) "initialize-instance.NODE from :FP: ~A, ~A, ~A"
                  self (id self) (this-fp self))
    (progn
      (if node
        (setf (slot-value self 'foreign-pointer-to-object) (this-fp node))
        (setf (slot-value self 'foreign-pointer-to-object) (rm-cffi::rm-Node-New (name self)
                                                                                 (dims self)
                                                                                 (opacity self))))
      (log5:log-for (create) "initialize-instance.NODE: ~A, ~A, ~A, ~A"
                    self (id self) (name self) (this-fp self))
      (if (or (root-node-p self) node)
        (setf (gc-p self) nil)
        (rm-cffi::rm-Node-Set-Client-Data (this-fp self)
                                          (cffi::make-pointer (id self))
                                          (cffi:callback client-data-proc)))))
  (when (root-node-p self)
    (log5:log-for (create) "NODE set to ROOT-NODE: ~A, ~A, ~A" self (id self) (name self))))

(defmethod initialize-instance :after ((self root-node) &key)
  (setf (slot-value self 'foreign-pointer-to-object) (rm-cffi::rm-root-node)))

(defmethod initialize-instance :around ((self rm-node)
					&key (traverse t) (pick t) (center nil) (compute-center nil)
					(bounding-box nil) (normalize-normals nil)
					(background-color) (unlit-color nil)
                                        (ambient-color nil) (diffuse-color nil) (specular-color nil)
					(xy/z nil) (viewport nil) (lights nil) (default-lighting nil)
					(light-model nil) (specular-exponent nil)
					(primitives nil) (children nil) (compute-bounding-box nil) (union-all nil)
                                        (camera nil) compute-view-from-geometry)
  (unless *initialised*
    (setf *initialised* t)
    (rm-cffi::rm-Init))
  
  (call-next-method)

  (loop for child in (if (listp children) children (list children)) :do
        (add-to-node self child))
  (loop for prim in primitives :do
        (add-to-node self prim))

  (loop for light in lights do
       (setf (light self) light))
  
  (setf (pick-p self) pick)
  (setf (traverse self) traverse)
  
  (when bounding-box
    (setf (bounding-box self) bounding-box))
  (when normalize-normals
    (setf (normalize-normals self) normalize-normals))
  (when background-color
    (setf (background-color self) background-color))
  (when unlit-color
    (setf (unlit-color self) unlit-color))
  (when ambient-color
    (setf (ambient-color self) ambient-color))
  (when diffuse-color
    (setf (diffuse-color self) diffuse-color))
  (when specular-color
    (setf (specular-color self) specular-color))
  (when xy/z
    (setf (xy/z self) xy/z))
  (when viewport
    (setf (viewport self) viewport))
  (when (and default-lighting (not light-model)
    (assign-default-lighting self)))
  (when light-model
    (setf (light-model self) light-model))
  (when specular-exponent
    (setf (specular-exponent self) specular-exponent))
  (when (and compute-bounding-box (not bounding-box))
    (compute-bounding-box self))
  (when center
    (setf (center self) center))
  (when (and compute-center (not center))
    (compute-center-from-bounding-box self))
  (when union-all
    (union-all-boxes self))
  (when (and camera compute-view-from-geometry (window self))
    (compute-view-from-geometry camera self))
  (when camera
    (setf (camera self) camera)))

(defun rm-root-node ()
  ;; TODO: Set *rm-root-node* to NIL when the OpenRM library is closed.
  (if *rm-root-node*
      *rm-root-node*
      (setf *rm-root-node* (make-instance 'root-node))))

(defmethod attach-to-root-node ((node node))
  "Attaches the specified node NODE to the OpenRM root node."
  (add-to-node (rm-root-node) node))

(defmethod add-to-node ((parent rm-node) (child node))
  (rm-cffi::rm-Node-Add-Child (fp parent) (fp child))
  (setf (gc-p child) nil)
  (log5:log-for (info) "ADD-TO-NODE:NODE->CHILD: parent: ~A, child: ~A"
                (name parent) (name child))
  parent)

(defmethod add-to-node :after ((parent rm-node) (child node))
  nil
  ;; (setf (gethash (id child) *rm-objects*) child)
  )

(defmethod add-to-node ((parent rm-node) (primitive primitive))
  (setf (gc-p primitive) nil)
  (rm-cffi::rm-Node-Add-Primitive (fp parent) (fp primitive))
  (log5:log-for (info) "ADD-TO-NODE:NODE->PRIMITIVE: parent: ~A" (name parent))
  parent)

(defmethod find-named-node ((self rm-node) name)
  (rm-cffi::rm-Find-Named-Node (fp self) name))

(defmethod delete-node ((self node))
  (rm-cffi::rm-node-delete (fp self)))

(defmethod remove-child-node ((parent rm-node) (child node))
  "Remove CHILD from PARENT."
  (log5:log-for (info) "remove-child-node.RM-NODE:PARENT:\(~A:~A\), CHILD:\(~A:~A\)" parent (name parent) child (name child))
  (if (rm-cffi::rm-node-remove-child (fp parent) (fp child))
      child
      nil))

(defmethod remove-sub-tree ((this rm-node) &optional parent)
  "Removes the NODES below THIS from the scene graph."
  (log5:log-for (info) "remove-sub-tree.RM-NODE: \(~A:~A\) &optional \(~A:~A\)" this (name this) parent (when parent (name parent)))
  (if parent
    (cons (list parent this) (loop for child in (get-children this)
				appending (remove-sub-tree child this)))
    (loop for child in (get-children this)
       appending (remove-sub-tree child this))))

(defmethod list-tree ((this rm-node) &optional parent all)
  "Returns as a LIST each PARENT->CHILD relationship in the scene-graph, rooted at SELF."
  (if parent
    (cons (list parent this) (loop for child in (get-children this)
				appending (list-tree child this all)))
    (loop for child in (get-children this)
       appending (list-tree child this all))))

(defun delete-scene-graph ()
  "Delete everthing in the scene graph, with
the exception of the root RMnode. Remove cameras and lights from the root node."
  (log5:log-for (free) "DELETE-SCENE-GRAPH")
  (remove-light (rm-root-node))
  (remove-camera (rm-root-node))
  ;; SCENE and WINDOW can both be set to the RM-ROOT-NODE
  ;; therefore to be safe, delete any camera or lights set
  ;; on the root node.
  (loop for node in (remove-duplicates (loop for child in (get-children (rm-root-node))
                                             collecting child)
				       :test #'(lambda (x y)
						 (node-eq x y)))
        do (progn
             (log5:log-for (free) "Remove from ROOT-NODE: ~A, ~A, ~A"
                           node (get-client-data node) (this-fp node))
             (rm-cffi::rm-node-remove-child (fp (rm-root-node)) (this-fp node))
             (log5:log-for (free) "RM-SUB-TREE-DELETE rooted at: ~A, ~A, ~A"
                           node (get-client-data node) (fp node))
             (rm-cffi::rm-sub-tree-delete (this-fp node))))
  ;(rm-cffi::rm-sub-tree-delete (this-fp (rm-root-node)))
  )

(defmethod remove-all-primitives ((self rm-node))
  (rm-cffi::rm-Node-Remove-All-Prims (fp self)))

(defmethod nodes-p ((self rm-node))
  (rm-cffi::rm-Node-Get-Num-Children (fp self)))

(defmethod primitives-p ((self rm-node))
  (let ((num (rm-cffi::rm-Node-Get-Num-prims (fp self))))
    (if (> num 0)
	num
	nil)))

(defmethod nth-node ((self rm-node) index)
  (let ((n (rm-cffi::rm-Node-Get-Ith-Child (fp self) index)))
    (if (cffi:null-pointer-p n)
	nil
	(make-instance 'node :fp n :gc nil))))

(defmethod get-children ((self rm-node))
  (remove nil (loop for i from 0 below (nodes-p self)
		 collecting (nth-node self i)))) 
 
(defmethod nth-primitive ((self rm-node) index)
  (let ((n (rm-cffi::rm-Node-Get-Primitive (fp self) index)))
    (if (cffi:null-pointer-p n)
	nil
	n)))

(defmethod union-all-boxes ((self rm-node))
  (rm-cffi::rm-Node-Union-All-Boxes (fp self)))

(defmethod compute-center-from-bounding-box ((self rm-node))
  (rm-cffi::rm-Node-Compute-Center-From-Bounding-Box (fp self)))

(defmethod center ((self rm-node))
  (let ((v (rm-cffi::rm-vertex-3d-new 0)))
    (rm-cffi::rm-Node-get-Center (fp self) v)
    (v3d nil nil nil v)))
(defmethod (setf center) ((vertex v3d) (self rm-node))
  (rm-cffi::rm-Node-set-center (fp self) (fp vertex)))

(defmethod compute-bounding-box ((self rm-node))
  (rm-cffi::rm-Node-Compute-Bounding-Box (fp self)))

(defmethod (setf bounding-box-min) ((min v3d) (self rm-node))
  (rm-cffi::rm-node-Set-Bounding-Box (fp self) (fp min) (cffi:null-pointer))
  self)
(defmethod (setf bounding-box-max) ((max v3d) (self rm-node))
  (rm-cffi::rm-node-Set-Bounding-Box (fp self) (cffi:null-pointer) (fp max))
  self)
(defmethod (setf bounding-box) ((bounds v3d*) (self rm-node))
  (setf (bounding-box-min self) (nth-vertex bounds 0)
	(bounding-box-max self) (nth-vertex bounds 1))
  self)

(defmethod bounding-box-min ((self rm-node))
  (let ((v (rm-cffi::rm-vertex-3d-new 0)))
    (when (rm-cffi::rm-Node-Get-Bounding-Box (fp self) v (cffi:null-pointer))
      (v3d nil nil nil v))))

(defmethod bounding-box-max ((self rm-node))
  (let ((v (rm-cffi::rm-vertex-3d-new 0)))
    (when (rm-cffi::rm-Node-Get-Bounding-Box (fp self) (cffi:null-pointer) v)
      (v3d nil nil nil v))))

(defmethod bounding-box ((self rm-node))
  (let ((bounds (v3d* nil 2)))
    (if (rm-cffi::rm-Node-Get-Bounding-Box (fp self) (fp (nth-vertex bounds 0)) (fp (nth-vertex bounds 1)))
	bounds
	nil)))

(defmethod (setf background-color) ((color c4d) (self rm-node))
  (rm-cffi::rm-Node-Set-Scene-Background-Color (fp self) (fp color))
  self)
(defmethod background-color ((self rm-node))
  (rm-base:with-c4d (col)
    (rm-cffi::rm-node-get-scene-background-color (fp self) col)
    (color rm-base:r rm-base:g rm-base:b rm-base:a)))

(defmethod (setf normalize-normals) (value (self rm-node))
  (rm-cffi::rm-Node-Set-Normalize-Normals (fp self) value))
(defmethod normalize-normals ((self rm-node))
  (cffi:with-foreign-object (value 'rm-cffi::rm-enum)
    (rm-cffi::rm-Node-Get-Normalize-Normals (fp self) value)))

(defmethod (setf unlit-color) ((color c4d) (self rm-node))
  (rm-cffi::rm-Node-Set-Unlit-Color (fp self) (fp color)))
(defmethod unlit-color ((self rm-node))
  (rm-base:with-c4d (col)
    (rm-cffi::rm-Node-Get-Unlit-Color (fp self) col)
    (color rm-base:r rm-base:g rm-base:b rm-base:a)))

(defmethod (setf ambient-color) ((color c4d) (self rm-node))
    (rm-cffi::rm-Node-Set-Ambient-Color (fp self) (fp color)))
(defmethod ambient-color ((self rm-node))
  (rm-base:with-c4d (col)
    (rm-cffi::rm-Node-Get-Ambient-Color (fp self) col)
    (color rm-base:r rm-base:g rm-base:b rm-base:a)))

(defmethod (setf diffuse-color) ((color c4d) (self rm-node))
  (rm-cffi::rm-Node-Set-Diffuse-Color (fp self) (fp color)))
(defmethod diffuse-color ((self rm-node))
  (rm-base:with-c4d (col)
    (rm-cffi::rm-Node-Get-Diffuse-Color (fp self) col)
    (color rm-base:r rm-base:g rm-base:b rm-base:a)))

(defmethod (setf specular-color) ((color c4d) (self rm-node))
  (rm-cffi::rm-Node-Set-specular-Color (fp self) (fp color)))
(defmethod specular-color ((self rm-node))
  (rm-base:with-c4d (col)
    (rm-cffi::rm-Node-Get-specular-Color (fp self) col)
    (color rm-base:r rm-base:g rm-base:b rm-base:a)))

(defmethod (setf specular-exponent) (value (self rm-node))
  (rm-cffi::rm-Node-Set-Specular-Exponent (fp self) value)
  self)
(defmethod specular-exponent ((self rm-node))
  (cffi:with-foreign-object (v :float)
    (rm-cffi::rm-Node-Get-Specular-Exponent (fp self) v)
    (cffi:mem-aref v :float)))

(defmethod (setf traverse) (value (self rm-node))
  (rm-cffi::rm-Node-Set-Traverse-Enable (fp self) value)
  self)
(defmethod traverse ((self rm-node))
  (rm-cffi::rm-Node-Get-Traverse-Enable (fp self)))
(defmethod traversep ((self rm-node))
  (rm-cffi::rm-Node-Get-Traverse-Enable (fp self)))

(defmethod (setf pick-p) (value (self rm-node))
  (rm-cffi::rm-Node-Set-Pick-Enable (fp self) value)
  self)
(defmethod pick-p ((self rm-node))
  (rm-cffi::rm-Node-Get-Pick-Enable (fp self)))

(defmethod insert-node ((target node) (parent rm-node) (child node))
  (remove-child-node parent child)
  (add-to-node parent target)
  (add-to-node target child)  target)

(cffi:defcallback client-data-proc :pointer
    ((node-fp :pointer)
     (client-data-pointer :pointer))
  "Called when a RMnode is deleted e.g. using (RM-CFFI::RM-NODE-DELETE NODE-FP)."
  (log5:log-for (free) "DEFCALLBACK:CLIENT-DATA-PROC ~A, ~A"
		node-fp
		(cffi:pointer-address client-data-pointer))
  (cffi:null-pointer))

(defmethod get-client-data (fp-node)
  (unless (cffi:pointerp fp-node)
    (error "GET-CLIENT-DATA: fp-node must be a foreign pointer to an RM-NODE."))
  (let ((client-data (rm-cffi::rm-Node-Get-Client-Data fp-node)))
    (if (cffi:null-pointer-p client-data)
	nil
	(cffi:pointer-address client-data))))

(defmethod get-client-data ((self rm-node))
  (get-client-data (this-fp self)))

;; (defmethod find-node (id)
;;   (gethash id *rm-objects*))

(defmethod (setf xy/z) ((translate v2d) (node rm-node))
  (rm-cffi::rm-node-set-translate-vector (fp node) (fp translate)))
(defmethod (setf xy/z+) ((xyz v2d) (node rm-node))
  (rm-base:with-v2d (translate)
    (when (rm-cffi::rm-Node-Get-Translate-Vector (fp node) translate)
      (rm-cffi::rm-Node-Set-Translate-Vector (fp node) (rm-base::v2d+ translate (fp xyz))))))

(defmethod xy/z ((node rm-node))
  (let ((position (v3d 0.0 0.0 0.0)))
    (when (rm-cffi::rm-Node-Get-Translate-Vector (fp node) (fp position))
      position)))

(defmethod (setf xy/z) ((translate vector) (node rm-node))
  (rm-base:with-v3d (v3d)
    (setf rm-base::x (svref translate 0)
          rm-base::y (svref translate 1)
          rm-base::z (svref translate 2))
    (rm-cffi::rm-node-set-translate-vector (fp node) v3d)))

(defmethod (setf xy/z) ((translate v3d) (node rm-node))
  (rm-cffi::rm-node-set-translate-vector (fp node) (fp translate)))
(defmethod (setf xy/z+) ((xyz v3d) (node rm-node))
  (rm-base:with-v3d (translate)
    (when (rm-cffi::rm-Node-Get-Translate-Vector (fp node) translate)
      (rm-cffi::rm-Node-Set-Translate-Vector (fp node) (rm-base::v3d+ translate (fp xyz))))))
(defmethod (setf xy/z+) ((xyz vector) (node rm-node))
  (rm-base:with-v3d (translate)
    (when (rm-cffi::rm-Node-Get-Translate-Vector (fp node) translate)
      (incf rm-base::x (svref xyz 0))
      (incf rm-base::y (svref xyz 1))
      (incf rm-base::z (svref xyz 2))
      (rm-cffi::rm-Node-Set-Translate-Vector (fp node) translate))))

;;(defmethod add-scene ((child rm-node) (parent rm-node))
;;  nil)

(defmethod refcount ((node rm-node))
  (cffi:foreign-slot-value (fp node) 'rm-cffi::rm-node 'rm-cffi::refcount))

;; (defmethod dump-scene-graph ((root-node rm-node))
;;   (labels ((traverse-nodes (node)
;; 	     (log5:log-for (info) "Node: ~A, ~A, ~A"
;; 			   (let ((n (find-node (get-client-data node))))
;; 			(when n
;; 			  (name n)))
;; 		      (get-client-data node)
;; 		      (cffi:foreign-slot-value node 'rm-cffi::rm-node 'rm-cffi::refcount))
;; 	     (dotimes (i (rm-cffi::rm-Node-Get-Num-Children node))
;; 	       (traverse-nodes (rm-cffi::rm-Node-Get-Ith-Child node i)))))
;;     (traverse-nodes (fp root-node))))

(defgeneric root-node-p (node)
  (:documentation "Returns T if NODE is the RM-ROOT-NODE. Returns NIL otherwise"))
(defmethod root-node-p ((node rm-node))
  (if (cffi:pointer-eq (rm-cffi::rm-root-node)
		       (this-fp node))
      t
      nil))

(defgeneric node-eq (node1 node2)
  (:documentation "Returns T if NODE1 and NODE2 contain the same foreign RMNODE. 
Returns NIL otherwise."))
(defmethod node-eq ((node1 rm-node) (node2 rm-node))
  (if (cffi:pointer-eq (this-fp node1) (this-fp node2))
      t
      nil))

(defmethod viewport ((self rm-node))
  (cffi:with-foreign-object (fv :pointer)
    (if (rm-cffi::rm-node-get-scene-viewport (fp self) fv)
	(vector (cffi:mem-aref (cffi:mem-aref fv :pointer) :float 0)
		(cffi:mem-aref (cffi:mem-aref fv :pointer) :float 1)
		(cffi:mem-aref (cffi:mem-aref fv :pointer) :float 2)
		(cffi:mem-aref (cffi:mem-aref fv :pointer) :float 3))
	nil)))

(defmethod (setf viewport) (viewport (self rm-node))
  (if viewport
      (setf (viewport self) #(0.0 0.0 1.0 1.0))
      (if (rm-cffi::rm-node-set-scene-viewport (fp self) (cffi:null-pointer))
	  viewport
	  nil)))

(defmethod (setf viewport) ((viewport vector) (self rm-node))
  ;; Modify an existing viewport, if available.
  ;; Creat a new viewport, if node is not already assigned to the node.
  (cffi:with-foreign-object (fv :pointer)
    (if (rm-cffi::rm-node-get-scene-viewport (fp self) fv)
      (progn
        (dotimes (i 4)
          (setf (cffi:mem-aref (cffi:mem-aref fv :pointer) :float i)
                (svref viewport i)))
	  (if (rm-cffi::rm-node-set-scene-viewport (fp self)
                                                   (cffi:mem-aref fv :pointer))
	      viewport
	      nil))
      (progn
        (cffi:with-foreign-object (fv :float 4)
          (setf (cffi:mem-aref fv :float 0) (svref viewport 0)
                (cffi:mem-aref fv :float 1) (svref viewport 1)
                (cffi:mem-aref fv :float 2) (svref viewport 2)
                (cffi:mem-aref fv :float 3) (svref viewport 3))
          (if (rm-cffi::rm-node-set-scene-viewport (fp self) fv)
            viewport
            nil))))))

(defun print-scene-graph (filename &optional (node (rm::rm-root-node)))
  (rm-cffi::rm-print-scene-graph (rm::fp node) :rm-true filename))


;;;
;;; Camera

(defmethod (setf camera) ((camera camera-2d) (node rm-node))
  (rm-cffi::rm-Node-Set-Scene-Camera-2D (fp node) (fp camera)))

(defmethod (setf camera) ((camera camera-3d) (node rm-node))
  (rm-cffi::rm-Node-Set-Scene-Camera-3D (fp node) (fp camera)))

(defmethod get-camera ((self rm-node) type)
  (cffi:with-foreign-object (c :pointer)
    (cond
      ((and (eq type :3d) (rm-cffi::rm-Node-Get-Scene-Camera-3D (fp self) c))
       (make-instance 'camera-3d :fp (cffi:mem-aref c :pointer) :defaults nil))
      ((and (eq type :2d) (rm-cffi::rm-Node-Get-Scene-Camera-2D (fp self) c))
       (make-instance 'camera-2d :fp (cffi:mem-aref c :pointer) :defaults nil))
      (t nil))))

(defmethod camera-p ((self rm-node))
  (first (remove nil (loop for type in '(:3d :2d)
                           collecting (get-camera self type)))))

(defmethod camera-2d-p ((node rm-node))
  (get-camera node :2d))

(defmethod camera-3d-p ((node rm-node))
  (get-camera node :3d))

(defmethod remove-camera-from-node ((self rm-node) camera)
  ;; Catchall when no camera defined for node.
  nil)

(defmethod remove-camera-from-node ((self rm-node) (camera camera-2d))
  (rm-cffi::rm-Node-Set-Scene-Camera-2d (fp self) (cffi:null-pointer)))

(defmethod remove-camera-from-node ((self rm-node) (camera camera-3d))
  (rm-cffi::rm-Node-Set-Scene-Camera-3d (fp self) (cffi:null-pointer)))

(defmethod remove-camera ((self rm-node))
  (remove-camera-from-node self (camera-p self)))

(defmethod assign-default-camera-2d ((self rm-node))
  (let ((camera (make-instance 'camera-2d :defaults t)))
    (setf (camera self) camera)))

(defmethod assign-default-camera-3d ((self rm-node))
  (let ((camera (make-instance 'camera-3d :defaults t)))
    (setf (camera self) camera)))

;;;
;;; Lighting

(defmethod assign-default-lighting ((self rm-node))
  (rm-cffi::rm-Default-Lighting (fp self)))

(defmethod get-light ((self rm-node) light-source)
  (cffi:with-foreign-object (l :pointer)
    (if (rm-cffi::rm-Node-Get-Scene-Light (fp self) light-source l)
	(let ((fp-light (cffi:mem-aref l :pointer)))
	  (cond
	    ((equal (rm-cffi::rm-Light-Get-Type fp-light) :RM-LIGHT-SPOT)
	     (make-instance 'spotlight :light-source light-source :fp fp-light))
	    ((equal (rm-cffi::rm-Light-Get-Type fp-light) :RM-LIGHT-POINT)
	     (make-instance 'point-light :light-source light-source :fp fp-light))
	    ((equal (rm-cffi::rm-Light-Get-Type fp-light) :RM-LIGHT-DIRECTIONAL)
	     (make-instance 'directional-light :light-source light-source :fp fp-light))))
	nil)))

(defmethod light-p ((node rm-node) &optional light-source)
  (if light-source
      (get-light node light-source)
      (let ((lights nil))
	(rm-cffi::rm-notify-level :rm-notify-silence)
	(setf lights (remove nil
			     (loop for l-source in '(:rm-light-0 :rm-light-1 :rm-light-2 :rm-light-3
						     :rm-light-4 :rm-light-5 :rm-light-6 :rm-light-7)
				collecting (get-light node l-source))))
	(rm-cffi::rm-notify-level :rm-notify-full)
	lights)))

(defmethod light ((node rm-node) light-source)
  (light-p node light-source))

(defmethod (setf light) ((light light) (node rm-node))
  (rm-cffi::rm-Node-Set-Scene-Light (fp node) (light-source light) (fp light)))

(defmethod remove-light ((self rm-node) &optional light-source)
  (loop for light in (light-p self light-source)
     do (rm-cffi::rm-Node-Set-Scene-Light (fp self) (light-source light)
					  (cffi:null-pointer))))

(defmethod light-model ((self rm-node))
  (cffi:with-foreign-object (l :pointer)
    (if (rm-cffi::rm-Node-Get-Scene-Light-Model (fp self) l)
	(make-instance 'light-model :fp (cffi:mem-aref l :pointer))
	nil)))
(defmethod (setf light-model) ((lm light-model) (self rm-node))
  (rm-cffi::rm-Node-Set-Scene-Light-Model (fp self) (fp lm)))
(defmethod (setf light-model) (value (self rm-node))
  (unless value
    (rm-cffi::rm-Node-Set-Scene-Light-Model (fp self) (cffi:null-pointer))))

(defmethod remove-light-model ((self rm-node))
  (when (light-model self)
    (setf (light-model self) nil)))


;;; Matrix Operations

(defmethod point-direction ((self node) (src v3d) &optional (dst (v3d 0.0 0.0 0.0)))
  (cffi:with-foreign-object (matrix 'rm-cffi::rm-matrix)
    (when (rm-cffi::rm-Node-Get-Rotate-Matrix (fp self) matrix)
      (rm-cffi::rm-Point-Matrix-Transform (fp src) matrix (fp dst)))
    dst))

(defmethod rotate ((self node) &key (direction nil) (match nil) (reverse nil))
  (rm-base::rotate-node (this-fp self)
			:direction (if direction (fp direction) nil) 
			:match-node (if match (fp match) nil)
			:reverse-nodes (if (null reverse)
                                         nil
                                         (if (listp reverse)
                                           (loop for node in reverse
                                                 collect (this-fp node))
                                           t))))

;;;
;;;

(defmethod scale ((self rm-node) width-scale height-scale)
  (let ((viewport (viewport self)))
    (unless viewport
      (setf (viewport self) t)
      (setf viewport (viewport self)))
    (let* ((width (- (svref viewport 2)
		     (svref viewport 0)))
	   (height (- (svref viewport 3)
		      (svref viewport 1)))
	   (x-center (+ (* 0.5 width)
			(svref viewport 0)))
	   (y-center (+ (* 0.5 height)
			(svref viewport 1))))
      (setf width (* width width-scale 0.5)
	    height (* height height-scale 0.5))
      (setf (viewport self) (vector (- x-center width)
				    (- y-center height)
				    (+ x-center width)
				    (+ y-center height))))))

(defun add-node (child &optional (parent *default-node*))
  (add-to-node parent child))

(defun add-primitive (prim &optional (node *default-node*))
  (add-to-node node prim))

(defmacro with-default-node ((a-node) &body body)
  (let ((node (gensym "node-"))
	(add-to-parent? (gensym "add-to-parent?-")))
    `(progn
       (let ((,node ,a-node)
	     (,add-to-parent? t))
	 (let ((*default-node* ,node)
	       (*parent-node* ,node))
	   (labels ((set-compute-center-from-bounding-box () (compute-center-from-bounding-box ,node))
		    (set-compute-bounding-box () (compute-bounding-box ,node))
		    (set-union-all-boxes () (union-all-boxes ,node))
		    (set-normalize-normals () (normalize-normals ,node))
		    (set-bounding-box (bounds &optional (node *default-node*))
		      (setf (bounding-box node) bounds))
		    (set-xy/z (vertex &optional (node *default-node*))
		      (setf (xy/z node) vertex))
		    (set-center (vertex &optional (node *default-node*))
		      (setf (center node) vertex))
		    (set-specular-exponent (exp &optional (node *default-node*))
		      (setf (specular-exponent node) exp))
		    (set-specular-color (col &optional (node *default-node*))
		      (setf (specular-color node) col))
		    (set-ambient-color (col &optional (node *default-node*))
		      (setf (ambient-color node) col))
		    (set-diffuse-color (col &optional (node *default-node*))
		      (setf (diffuse-color node) col))
		    (add-this-to-parent (add?)
		      (if add?
			  (setf ,add-to-parent? t)
			  (setf ,add-to-parent? nil)))
		    (set-light (light &optional (node *default-node*))
		      (setf (light node) light))
		    (set-default-camera (&optional (node *default-node*))
		      (assign-default-camera node)))
	     (declare (ignorable #'set-compute-center-from-bounding-box
				 #'set-compute-bounding-box
				 #'set-union-all-boxes
				 #'set-normalize-normals
				 #'set-bounding-box
				 #'set-xy/z
				 #'set-center
				 #'set-specular-exponent
				 #'set-ambient-color
				 #'set-specular-color
				 #'set-diffuse-color
				 #'add-this-to-parent
				 #'set-light
				 #'set-default-camera))
	     ,@body))
	 (when (and *parent-node*
		    ,add-to-parent?)
	   (add-to-node *parent-node* ,node))))))


