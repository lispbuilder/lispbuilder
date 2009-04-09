
(in-package #:lispbuilder-openrm)

(defclass rm-pick (openrm-object)
  ())

(defmethod add-pick (node &optional type)
  nil)
     
(defmethod remove-pick ((self node)))
(defmethod delete-pick ((self node)))

(defmethod clear-picked ()
  (loop for node being the hash-values in *rm-picked*
     do (setf (display-info-p node) nil))
  (clrhash *rm-picked*))

(defmethod add-pick ((self meta-node) &optional (type :single))
  (case type
    (:single (clear-picked)
	     (setf (gethash (id self) *rm-picked*) self)
	     (setf (display-info-p self) t))
    (:multiple (unless (gethash (id self) *rm-picked*)
		 (setf (gethash (id self) *rm-picked*) self
		       (display-info-p self) t)))))

(defmethod delete-pick ((self meta-node))
  (remhash (id self) *rm-picked*))

(defmethod remove-pick ((self meta-node))
  (delete-pick self)
  (setf (display-info-p self) nil))

(defmethod get-single-pick ((window window) (node rm-node) x y))

(defmethod get-mulitiple-pick ((window window) (node rm-node) x y)
  (let* ((pick-list-fp (cffi:foreign-alloc 'rm-cffi::rm-pick))
	 (pick-num (rm-cffi::rm-frame-pick-list (fp (pipe window))
						(fp node) x y
						pick-list-fp)))
    (fformat "   get-picked: x: ~A, y: ~A, num: ~A" x y pick-num)
    (loop for i from 0 below pick-num
       do (fformat "     NODE: ~A" (rm-cffi::rm-picked-node
				    (cffi:mem-aref (cffi:mem-ref pick-list-fp :pointer) 'rm-cffi::rm-pick i))))
	 
    (when (> pick-num 0)
      (let ((pick-list (loop for i from 0 below pick-num
			  collecting (let ((id (get-client-data (rm-cffi::rm-picked-node
								 (cffi:mem-aref (cffi:mem-ref pick-list-fp :pointer) 'rm-cffi::rm-pick i)))))
				       (if (find-node id)
					   (progn
					     (fformat "get-picked: (find-node id) ~A" (name (find-node id)))
					     (find-node id))
					   (progn
					     (let ((picked-node nil))
					       (loop for v being the hash-values in *rm-objects*
						  do (when (and (meta-p v)
								(equal id (get-client-data (target-node v))))
						       (setf picked-node v)
						       (loop-finish)))
					       (fformat "get-picked: picked-node ~A" (name picked-node))
					       picked-node)))))))
	(rm-cffi::rm-pick-delete (cffi:mem-ref pick-list-fp :pointer))
	pick-list))))

(defmethod get-picked ((window window) (node rm-node) x y
		       &key (z-order :ascend) multiple)
  (if multiple
      (if (equal z-order :descend)
	  (reverse (get-mulitiple-pick window node x y))
	  (get-mulitiple-pick window node x y))
      (get-single-pick window node x y)))
    
  


;; (defmethod get-picked ((window window) (node rm-node) x y &key (z-order :ascend))
;;   (let* ((pick-list-fp (cffi:foreign-alloc 'rm-cffi::rm-pick))
;; 	 (pick-num (rm-cffi::rm-frame-pick-list (fp (pipe window))
;; 						(fp node) x y
;; 						pick-list-fp)))
;;     (fformat "get-picked: x: ~A, y: ~A, num: ~A" x y pick-num)
;;     (when (> pick-num 0)
;;       (let ((pick-list (loop for i from 0 below pick-num
;; 			  collecting (let ((id (get-client-data (rm-cffi::rm-picked-node
;; 								 (cffi:mem-aref (cffi:mem-ref pick-list-fp :pointer) 'rm-cffi::rm-pick i)))))
;; 				       (or (find-node id)
;; 					   (let ((picked-node nil))
;; 					     (loop for v being the hash-values in *rm-objects*
;; 						do (when (and (meta-p v)
;; 							      (equal id (get-client-data (target-node v))))
;; 						       (setf picked-node v)
;; 						       (loop-finish)))
;; 					     picked-node))))))
;; 	(rm-cffi::rm-pick-delete (cffi:mem-ref pick-list-fp :pointer))
;; 	(if (equal z-order :descend)
;; 	    (reverse pick-list)
;; 	    pick-list)))))

;; (defgeneric picked-p (window node x y))
;; (defmethod picked-p ((window window) (node rm-node) x y)
;;   (let ((pick (rm-cffi::rm-frame-pick (rm::fp (rm::pipe window)) (rm::fp (rm::node node)) x y)))
;;     (unless (cffi:null-pointer-p pick)
;;       (make-instance 'rm-pick :fp pick))))

;; (defgeneric node-of (pick-list))
;; (defmethod node-of ((picked rm-pick))
;;   (let ((node (rm-cffi::rm-picked-node (fp picked))))
;;     (unless (cffi:null-pointer-p node)
;; 	  (rm::add-pick (rm::find-closest-pick-node (rm::node (rm::find-scene window "default-scene"))
;; 						    (rm::get-client-data node))
;; 			:single))))



;; (let ((node (rm-cffi::rm-picked-node pick)))
;; 	(unless (cffi:null-pointer-p node)
;; 	  (rm::add-pick (rm::find-closest-pick-node (rm::node (rm::find-scene window "default-scene"))
;; 						    (rm::get-client-data node))
;; 			:single)))
;;       (rm-cffi::rm-pick-delete pick)
