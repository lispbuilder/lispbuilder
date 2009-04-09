 
(in-package #:rm)

(defclass vertex (openrm-object)
  ((create-function
    :initform nil 
    :initarg :create))
  (:default-initargs
   :gc t))

(defclass vertex-single (vertex)()
  (:default-initargs
   :free (simple-free #'cffi:foreign-free 'vertex)))

(defclass vertex-array (vertex)
  ((size
    :reader size
    :initform (error "Specify an array size.")
    :initarg :size)))

(defmethod initialize-instance :after ((self vertex-array) &key) 
  (setf (slot-value self 'foreign-pointer-to-object) (funcall (slot-value self 'create-function) (size self))))

(defclass v2d (vertex-single)()
  (:default-initargs
   :fp (cffi:foreign-alloc 'rm-cffi::rm-vertex-2d)))

(defmethod initialize-instance :around ((self v2d) &key x y)
  (call-next-method)
  (when x (setf (cffi:foreign-slot-value (fp self) 'rm-cffi::rm-vertex-2d 'rm-cffi::x) x))
  (when y (setf (cffi:foreign-slot-value (fp self) 'rm-cffi::rm-vertex-2d 'rm-cffi::y) y)))

(defclass v2d* (vertex-array)()
  (:default-initargs
   :create #'rm-cffi::rm-vertex-2d-new
   :free (simple-free #'rm-cffi::rm-vertex-2d-delete 'vertex)))

(defclass v3d (vertex-single)()
  (:default-initargs
   :fp (cffi:foreign-alloc 'rm-cffi::rm-vertex-3d)))

(defmethod initialize-instance :around ((self v3d) &key x y z)
  (call-next-method)
  (when x (setf (cffi:foreign-slot-value (fp self) 'rm-cffi::rm-vertex-3d 'rm-cffi::x) x))
  (when y (setf (cffi:foreign-slot-value (fp self) 'rm-cffi::rm-vertex-3d 'rm-cffi::y) y))
  (when z (setf (cffi:foreign-slot-value (fp self) 'rm-cffi::rm-vertex-3d 'rm-cffi::z) z)))

(defclass v3d* (vertex-array)()
  (:default-initargs
   :create #'rm-cffi::rm-vertex-3d-new
   :free (simple-free #'rm-cffi::rm-vertex-3d-delete 'vertex)))

;; (defclass v4d (vertex-single)
;;   ()
;;   (:default-initargs
;;    :fp (cffi:foreign-alloc 'rm-cffi::rm-vertex-4d)
;;     :free #'(lambda (fp)
;; 	      (cffi:foreign-free fp))))
;; (defclass v4d* (vertex-array)
;;   ()
;;   (:default-initargs
;;    :free #'(lambda (fp)
;; 	     (rm-cffi::rm-vertex-4d-delete fp))))


(defmethod x ((vertex v2d))
  (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-2d 'rm-cffi::x))
(defmethod (setf x) (x-val (vertex v2d))
  (setf (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-2d 'rm-cffi::x)
	x-val))
(defmethod y ((vertex v2d))
  (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-2d 'rm-cffi::y))
(defmethod (setf y) (y-val (vertex v2d))
  (setf (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-2d 'rm-cffi::y)
	y-val))

(defmethod x ((vertex v3d))
  (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-3d 'rm-cffi::x))
(defmethod (setf x) (x-val (vertex v3d))
  (setf (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-3d 'rm-cffi::x)
	x-val))
(defmethod y ((vertex v3d))
  (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-3d 'rm-cffi::y))
(defmethod (setf y) (y-val (vertex v3d))
  (setf (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-3d 'rm-cffi::y)
	y-val))
(defmethod z ((vertex v3d))
  (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-3d 'rm-cffi::z))
(defmethod (setf z) (z-val (vertex v3d))
  (setf (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-3d 'rm-cffi::z)
	z-val))

(defmethod (setf xy/z) ((dims vector float 2) (vertex v2d))
  (setf (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-2d 'rm-cffi::x) (svref dims 0)
	(cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-2d 'rm-cffi::y) (svref dims 1)))
(defmethod (setf xy/z) ((dims vector float 3) (vertex v3d))
  (setf (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-3d 'rm-cffi::x) (svref dims 0)
	(cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-3d 'rm-cffi::y) (svref dims 1)
	(cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-3d 'rm-cffi::z) (svref dims 2)))

;; (defmethod x ((vertex v4d))
;;   (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-4d 'rm-cffi::x))
;; (defmethod (setf x) (x-val (vertex v4d))
;;   (setf (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-4d 'rm-cffi::x)
;; 	x-val))
;; (defmethod y ((vertex v4d))
;;   (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-4d 'rm-cffi::y))
;; (defmethod (setf y) (y-val (vertex v4d))
;;   (setf (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-4d 'rm-cffi::y)
;; 	y-val))
;; (defmethod z ((vertex v4d))
;;   (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-4d 'rm-cffi::z))
;; (defmethod (setf z) (z-val (vertex v4d))
;;   (setf (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-4d 'rm-cffi::z)
;; 	z-val))
;; (defmethod w ((vertex v4d))
;;   (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-4d 'rm-cffi::w))
;; (defmethod (setf w) (w-val (vertex v4d))
;;   (setf (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-4d 'rm-cffi::w)
;; 	w-val))

(defun vertex (x y &optional z)
  (if z
      (make-instance 'v3d :x x :y y :z z)
      (make-instance 'v2d :x x :y y)))

(defun v2d (x y &optional fp)
  (let ((vertex (if fp
		    (make-instance 'v2d :fp fp)
		    (make-instance 'v2d :x x :y y))))
    (log5:log-for (create) "V2D Created: ~A" (fp vertex))
    vertex))
(defun v3d (x y z &optional fp)
  (let ((vertex (if fp
		    (make-instance 'v3d :fp fp)
		    (make-instance 'v3d :x x :y y :z z))))
    (log5:log-for (create) "V3D Created: ~A" (fp vertex))
    vertex))
;; (defun v4d (x y z w &optional fp)
;;   (let ((vertex (if fp
;; 		    (make-instance 'v4d :fp fp)
;; 		    (make-instance 'v4d))))
;;     (unless fp
;;       (setf (x vertex) x
;; 	    (y vertex) y
;; 	    (z vertex) z
;; 	    (w vertex) w))
;;     vertex))


(defmethod v2d* ((vertex v2d) &optional size)
  (when size
    (let ((vertex-array (make-instance 'v2d* :size size)))
      (let ((vertex-array-fp (fp vertex-array))
	    (2d (cffi:foreign-type-size 'rm-cffi::rm-vertex-2d)))
	(loop
	   for i from 0 upto (1- (size vertex-array))
	   do (progn (setf (cffi:foreign-slot-value vertex-array-fp 'rm-cffi::rm-vertex-2d 'rm-cffi::x) (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-2d 'rm-cffi::x)
			   (cffi:foreign-slot-value vertex-array-fp 'rm-cffi::rm-vertex-2d 'rm-cffi::y) (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-2d 'rm-cffi::y))
		     (setf vertex-array-fp (cffi:inc-pointer vertex-array-fp 2d)))))
      (log5:log-for (create) "V2D Created: ~A" (fp vertex-array))
      vertex-array)))
(defmethod v2d* ((vertices list) &optional size)
  (if (consp vertices)
      (let ((vertex-array (make-instance 'v2d* :size (length vertices))))
	(let ((vertex-array-fp (fp vertex-array))
	      (2d (cffi:foreign-type-size 'rm-cffi::rm-vertex-2d)))
	  (loop
	     for i from 0 upto (1- (size vertex-array))
	     for v in vertices
	     do (progn (if (consp v)
			   (setf (cffi:foreign-slot-value vertex-array-fp 'rm-cffi::rm-vertex-2d 'rm-cffi::x) (nth 0 v)
				 (cffi:foreign-slot-value vertex-array-fp 'rm-cffi::rm-vertex-2d 'rm-cffi::y) (nth 1 v))
			   (setf (cffi:foreign-slot-value vertex-array-fp 'rm-cffi::rm-vertex-2d 'rm-cffi::x) (cffi:foreign-slot-value (fp v) 'rm-cffi::rm-vertex-2d 'rm-cffi::x)
				 (cffi:foreign-slot-value vertex-array-fp 'rm-cffi::rm-vertex-2d 'rm-cffi::y) (cffi:foreign-slot-value (fp v) 'rm-cffi::rm-vertex-2d 'rm-cffi::y)))
		       (setf vertex-array-fp (cffi:inc-pointer vertex-array-fp 2d)))))
	(log5:log-for (create) "V2D* Created: ~A" (fp vertex-array))
	vertex-array)
      (when size
	(let ((vertex-array (make-instance 'v2d* :size size)))
	  (log5:log-for (create) "V2D* Created: ~A" (fp vertex-array))
	  vertex-array))))

(defmethod v3d* ((vertex v3d) &optional size)
  (when size
    (let ((vertex-array (make-instance 'v3d* :size size)))
      (let ((vertex-array-fp (fp vertex-array))
	    (3d (cffi:foreign-type-size 'rm-cffi::rm-vertex-3d)))
	(loop
	   for i from 0 upto (1- (size vertex-array))
	   do (progn (setf (cffi:foreign-slot-value vertex-array-fp 'rm-cffi::rm-vertex-3d 'rm-cffi::x) (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-3d 'rm-cffi::x)
			   (cffi:foreign-slot-value vertex-array-fp 'rm-cffi::rm-vertex-3d 'rm-cffi::y) (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-3d 'rm-cffi::y)
			   (cffi:foreign-slot-value vertex-array-fp 'rm-cffi::rm-vertex-3d 'rm-cffi::z) (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-3d 'rm-cffi::z))
		     (setf vertex-array-fp (cffi:inc-pointer vertex-array-fp 3d)))))
      (log5:log-for (create) "V3D Created: ~A" (fp vertex-array))
      vertex-array)))
(defmethod v3d* ((vertices list) &optional size)
  (if (consp vertices)
    (let ((vertex-array (make-instance 'v3d*
                                       :size (length vertices))))
      (let ((vertex-array-fp (fp vertex-array))
            (3d (cffi:foreign-type-size 'rm-cffi::rm-vertex-3d)))
        (loop
         for i from 0 upto (1- (size vertex-array))
         for v in vertices
         do (progn (if (consp v)
                     (setf (cffi:foreign-slot-value vertex-array-fp 'rm-cffi::rm-vertex-3d 'rm-cffi::x) (nth 0 v)
                           (cffi:foreign-slot-value vertex-array-fp 'rm-cffi::rm-vertex-3d 'rm-cffi::y) (nth 1 v)
                           (cffi:foreign-slot-value vertex-array-fp 'rm-cffi::rm-vertex-3d 'rm-cffi::z) (nth 2 v))
                     (setf (cffi:foreign-slot-value vertex-array-fp 'rm-cffi::rm-vertex-3d 'rm-cffi::x) (cffi:foreign-slot-value (fp v) 'rm-cffi::rm-vertex-3d 'rm-cffi::x)
                           (cffi:foreign-slot-value vertex-array-fp 'rm-cffi::rm-vertex-3d 'rm-cffi::y) (cffi:foreign-slot-value (fp v) 'rm-cffi::rm-vertex-3d 'rm-cffi::y)
                           (cffi:foreign-slot-value vertex-array-fp 'rm-cffi::rm-vertex-3d 'rm-cffi::z) (cffi:foreign-slot-value (fp v) 'rm-cffi::rm-vertex-3d 'rm-cffi::z)))
              (setf vertex-array-fp (cffi:inc-pointer vertex-array-fp 3d)))))
      (log5:log-for (create) "V3D* Created: ~A" (fp vertex-array))
      vertex-array) 
    (when size
      (let ((vertex-array (make-instance 'v3d* :size size)))
        (log5:log-for (create) "V3D* Created: ~A" (fp vertex-array))
        vertex-array))))


(defmethod copy-vertex-to-foreign ((vertex v2d) foreign-pointer &key (index 0))
  (declare (ignore index))
  (rm-base:with-v2d (vert foreign-pointer nil)
    (setf rm-base:x (x vertex)
	  rm-base:y (y vertex)))
  foreign-pointer)
(defmethod copy-vertex-to-foreign ((vertex v3d) foreign-pointer &key (index 0))
  (declare (ignore index))
  (rm-base:with-v3d (vert foreign-pointer nil)
    (setf rm-base:x (x vertex)
	  rm-base:y (y vertex)
	  rm-base:z (z vertex)))
  foreign-pointer)
;; (defmethod copy-vertex-to-foreign ((vertex v4d) foreign-pointer &key (index 0))
;;   (declare (ignore index))
;;   (rm-base:with-v4d (vert foreign-pointer nil)
;;     (setf rm-base:x (x vertex)
;; 	  rm-base:y (y vertex)
;; 	  rm-base:z (z vertex)
;; 	  rm-base:w (w vertex)))
;;   foreign-pointer)

(defmethod copy-vertex-to-foreign ((vertex v2d*) foreign-pointer &key (index 0))
  (rm-base:with-v2d (vert foreign-pointer nil)
    (let ((v (nth-vertex vertex index)))
      (setf rm-base:x (x v)
	    rm-base:y (y v))))
  foreign-pointer)
(defmethod copy-vertex-to-foreign ((vertex v3d*) foreign-pointer &key (index 0))
  (rm-base:with-v3d (vert foreign-pointer nil)
    (let ((v (nth-vertex vertex index)))
      (setf rm-base:x (x v)
	    rm-base:y (y v)
	    rm-base:z (z v))))
  foreign-pointer)
;; (defmethod copy-vertex-to-foreign ((vertex v4d*) foreign-pointer &key (index 0))
;;   (rm-base:with-v4d (vert foreign-pointer nil)
;;     (let ((v (nth-vertex vertex index)))
;;       (setf rm-base:x (x v)
;; 	    rm-base:y (y v)
;; 	    rm-base:z (z v)
;; 	    rm-base:w (w v))))
;;   foreign-pointer)

(defmethod nth-vertex ((self v2d*) index)
  (when (>= index (size self))
    (error "NTH-VERTEX, index ~A out of range ~A" index (size self)))
  (make-instance 'v2d :gc nil :fp (cffi:mem-aref (fp self) 'rm-cffi::rm-vertex-2d index)))
(defmethod nth-vertex ((self v3d*) index)
  (when (>= index (size self))
    (error "NTH-VERTEX, index ~A out of range ~A" index (size self)))
  (make-instance 'v3d :gc nil :fp (cffi:mem-aref (fp self) 'rm-cffi::rm-vertex-3d index)))

(defmethod copy-vertex ((dst v2d) (src v2d) &key (start 0) (end nil))
  (declare (ignore start end))
  (setf (x dst) (x src)
	(y dst) (y src)))
(defmethod copy-vertex ((dst v3d) (src v3d) &key (start 0) (end nil))
  (declare (ignore start end))
  (setf (x dst) (x src)
	(y dst) (y src)
	(z dst) (z src)))
;; (defmethod copy-vertex ((dst v4d) (src v4d))
;;   (setf (x dst) (x src)
;; 	(y dst) (y src)
;; 	(z dst) (z src)
;; 	(w dst) (w src)))

(defmethod copy-vertex ((dst v2d) (src list) &key (start 0) (end nil))
  (declare (ignore start end))
  (setf (x dst) (nth 0 src)
	(y dst) (nth 1 src)))
(defmethod copy-vertex ((dst v3d) (src list) &key (start 0) (end nil))
  (declare (ignore start end))
  (setf (x dst) (nth 0 src)
	(y dst) (nth 1 src)
	(z dst) (nth 2 src)))
;; (defmethod copy-vertex ((dst v4d) (src list))
;;   (setf (x dst) (nth 0 src)
;; 	(y dst) (nth 1 src)
;; 	(z dst) (nth 2 src)
;; 	(w dst) (nth 3 src)))

(defmethod copy-vertex ((dst v3d*) (src v3d) &key (start 0) (end nil))
  "Copy the values in the single vertex to elements in the vertex array."
  (let ((fp (fp dst))
	(end (if end end (size dst))))
    (loop for i from start below end do
	 (rm-base::with-v3d (vert (cffi:mem-aref fp 'rm-cffi::rm-vertex-3d i) nil)
	   (setf rm-base::x (x src)
		 rm-base::y (y src)
		 rm-base::z (z src)))))
  dst)
