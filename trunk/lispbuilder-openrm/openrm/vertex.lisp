 
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

(defmethod initialize-instance :after ((self v2d) &key x y)
  (let ((fp (fp self)))
    (when x (setf (cffi:foreign-slot-value fp 'rm-cffi::rm-vertex-2d 'rm-cffi::x) x))
    (when y (setf (cffi:foreign-slot-value fp 'rm-cffi::rm-vertex-2d 'rm-cffi::y) y))))

(defclass v2d* (vertex-array)()
  (:default-initargs
   :create #'rm-cffi::rm-vertex-2d-new
   :free (simple-free #'rm-cffi::rm-vertex-2d-delete 'vertex)))

(defclass v3d (vertex-single)()
  (:default-initargs
   :fp (cffi:foreign-alloc 'rm-cffi::rm-vertex-3d)))

(defmethod initialize-instance :after ((self v3d) &key x y z)
  (let ((fp (fp self)))
    (when x (setf (cffi:foreign-slot-value fp 'rm-cffi::rm-vertex-3d 'rm-cffi::x) x))
    (when y (setf (cffi:foreign-slot-value fp 'rm-cffi::rm-vertex-3d 'rm-cffi::y) y))
    (when z (setf (cffi:foreign-slot-value fp 'rm-cffi::rm-vertex-3d 'rm-cffi::z) z))))

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

(defmethod x ((color vector))
  (svref color 0))
(defmethod y ((color vector))
  (svref color 1))
(defmethod z ((color vector))
  (when (> (length color) 2)
    (svref color 2)))
(defmethod (setf x) (value (color vector))
  (setf (svref color 0) value))
(defmethod (setf y) (value (color vector))
  (setf (svref color 1) value))
(defmethod (setf z) (value (color vector))
  (when (> (length color) 2)
    (setf (svref color 2) value)))

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


(defmethod xy/z ((vertex v2d))
  (vector (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-2d 'rm-cffi::x)
          (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-2d 'rm-cffi::y)))
(defmethod xy/z ((vertex v3d))
  (vector (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-3d 'rm-cffi::x)
          (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-3d 'rm-cffi::y)
          (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-3d 'rm-cffi::z)))

(defmethod (setf xy/z) ((dims vector float 2) (vertex v2d))
  (setf (cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-2d 'rm-cffi::x) (svref dims 0)
	(cffi:foreign-slot-value (fp vertex) 'rm-cffi::rm-vertex-2d 'rm-cffi::y) (svref dims 1)))
(defmethod (setf xy/z) ((dims vector float 3) (vertex v3d))
  (format t "In (dims vector float 3)~%")
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
    (vector x y z)
    (vector x y)))

(defun v2d (x y &optional fp)
  (if fp
    (if (pointerp fp)
      (make-instance 'v2d :fp fp :x x :y y)
      (error "ERROR - V2D: :FP must be of type rm-cffi::rm-vertex-2d and not ~A" fp))
    (make-instance 'v2d :x x :y y)))

(defun v3d (x y z &optional fp)
  (if fp
    (if (pointerp fp)
      (make-instance 'v3d :fp fp :x x :y y :z z)
      (error "ERROR - V3D: :FP must be of type rm-cffi::rm-vertex-3d and not ~A" fp))
    (make-instance 'v3d :x x :y y :z z)))

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

(defmethod vertex* ((vertex vector) &optional size)
  (when size
    (make-array size :initial-element vertex)))

(defmethod v2d* (size &key initial-element initial-contents)
  (unless size
    (when initial-element
      (error "ERROR - V2D*; SIZE must be specified if :INITIAL-ELEMENT is specified."))
    (unless initial-contents
      (error "ERROR - V2D*; SIZE or :INITIAL-CONTENTS must be specified.")))
  (if size
    (let* ((vertex-array (make-instance 'v2d* :size size))
           (fp (fp vertex-array)))
      (when initial-element
        (dotimes (i size)
          (cffi:with-foreign-slots ((rm-cffi::x rm-cffi::y) (cffi:mem-aref fp 'rm-cffi::rm-vertex-2d i) rm-cffi::rm-vertex-2d)
            (setf rm-cffi::x (x initial-element)
                  rm-cffi::y (y initial-element)))))
      vertex-array)
    (let* ((vertex-array (make-instance 'v2d* :size (length initial-contents)))
           (fp (fp vertex-array)))
      (if (vectorp initial-contents)
        ;; Loop over VECTOR
        (loop
         :for i :from 0 :below (size vertex-array)
         :for v :across initial-contents
         :do (cffi:with-foreign-slots ((rm-cffi::x rm-cffi::y) (cffi:mem-aref fp 'rm-cffi::rm-vertex-2d i) rm-cffi::rm-vertex-2d)
               (setf rm-cffi::x (x v)
                     rm-cffi::y (y v))))
        (loop
         ;; Loop over LIST
         :for i :from 0 :below (size vertex-array)
         :for v :in initial-contents
         :do (cffi:with-foreign-slots ((rm-cffi::x rm-cffi::y) (cffi:mem-aref fp 'rm-cffi::rm-vertex-2d i) rm-cffi::rm-vertex-2d)
               (setf rm-cffi::x (x v)
                     rm-cffi::y (y v)))))
      vertex-array)))

(defmethod v3d* (size &key initial-element initial-contents)
  (unless size
    (when initial-element
      (error "ERROR - V3d*; SIZE must be specified if :INITIAL-ELEMENT is specified."))
    (unless initial-contents
      (error "ERROR - V3d*; SIZE or :INITIAL-CONTENTS must be specified.")))
  (if size
    (let* ((vertex-array (make-instance 'v3d* :size size))
           (fp (fp vertex-array)))
      (when initial-element
        (dotimes (i size)
          (cffi:with-foreign-slots ((rm-cffi::x rm-cffi::y rm-cffi::z)
                                    (cffi:mem-aref fp 'rm-cffi::rm-vertex-3d i) rm-cffi::rm-vertex-3d)
            (setf rm-cffi::x (x initial-element)
                  rm-cffi::y (y initial-element)
                  rm-cffi::z (z initial-element)))))
      vertex-array)
    (let* ((vertex-array (make-instance 'v3d* :size (length initial-contents)))
           (fp (fp vertex-array)))
      (if (vectorp initial-contents)
        ;; Loop over VECTOR
        (loop
         :for i :from 0 :below (size vertex-array)
         :for v :across initial-contents
         :do (cffi:with-foreign-slots ((rm-cffi::x rm-cffi::y rm-cffi::z)
                                       (cffi:mem-aref fp 'rm-cffi::rm-vertex-3d i) rm-cffi::rm-vertex-3d)
               (setf rm-cffi::x (x v)
                     rm-cffi::y (y v)
                     rm-cffi::z (z v))))
        (loop
         ;; Loop over LIST
         :for i :from 0 :below (size vertex-array)
         :for v :in initial-contents
         :do (progn
               (format t "~a: x = ~A~%" v (x v))
               (cffi:with-foreign-slots ((rm-cffi::x rm-cffi::y rm-cffi::z)
                                       (cffi:mem-aref fp 'rm-cffi::rm-vertex-3d i) rm-cffi::rm-vertex-3d)
               (setf rm-cffi::x (x v)
                     rm-cffi::y (y v)
                     rm-cffi::z (z v))))))
      vertex-array)))

(defmacro with-copy-vertex-2d-to-foreign ((vertex fp) &body body)
  (let ((body-value (gensym "body-value")))
    `(let ((,body-value nil))
       (cffi:with-foreign-object (,fp 'rm-cffi::rm-vertex-2d)
         (cffi:with-foreign-slots ((rm-cffi::x rm-cffi::y) ,fp rm-cffi::rm-vertex-2d)
           (setf rm-cffi::x (x ,vertex)
                 rm-cffi::y (y ,vertex))
           ,@body)))))

(defmacro with-copy-vertex-3d-to-foreign ((vertex fp) &body body)
  (let ((body-value (gensym "body-value")))
    `(let ((,body-value nil))
       (cffi:with-foreign-object (,fp 'rm-cffi::rm-vertex-3d)
         (cffi:with-foreign-slots ((rm-cffi::x rm-cffi::y rm-cffi::z) ,fp rm-cffi::rm-vertex-3d)
           (setf rm-cffi::x (x ,vertex)
                 rm-cffi::y (y ,vertex)
                 rm-cffi::z (z ,vertex))
           ,@body)))))

(defmacro with-copy-vertex-2d-array-to-foreign ((vertex fp) &body body)
  (let ((i (gensym "i"))
        (v (gensym "v")))
    `(cffi:with-foreign-object (,fp 'rm-cffi::rm-vertex-2d (length ,vertex))
       (loop
        :for ,i :from 0 :below (length ,vertex)
        :for ,v :across vertex
        :do (cffi:with-foreign-slots ((rm-cffi::x rm-cffi::y)
                                      (cffi:mem-aref ,fp 'rm-cffi::rm-vertex-2d ,i) rm-cffi::rm-vertex-2d)
              (setf rm-cffi::x (x ,v)
                    rm-cffi::y (y ,v))))
       ,@body)))

(defmacro with-copy-vertex-3d-array-to-foreign ((vertex fp) &body body)
  (let ((i (gensym "i"))
        (v (gensym "v")))
    `(cffi:with-foreign-object (,fp 'rm-cffi::rm-vertex-3d (length ,vertex))
       (loop
        :for ,i :from 0 :below (length ,vertex)
        :for ,v :across vertex
        :do (cffi:with-foreign-slots ((rm-cffi::x rm-cffi::y rm-cffi::z)
                                      (cffi:mem-aref ,fp 'rm-cffi::rm-vertex-3d ,i) rm-cffi::rm-vertex-3d)
              (setf rm-cffi::x (x ,v)
                    rm-cffi::y (y ,v)
                    rm-cffi::z (z ,v))))
       ,@body)))

(defmethod new-vertex-from-foreign-2d-vertex (fp length)
  (let ((vertex-vector (make-array length)))
    (dotimes (i length)
      (cffi:with-foreign-slots ((rm-cffi::x rm-cffi::y) (cffi:mem-aref fp 'rm-cffi::rm-vertex-2d i) rm-cffi::rm-vertex-2d)
        (setf (svref vertex-vector i) (vertex rm-cffi::x rm-cffi::y))))
    vertex-vector))
(defmethod new-vertex-from-foreign-3d-vertex (fp length)
  (let ((vertex-vector (make-array length)))
    (dotimes (i length)
      (cffi:with-foreign-slots ((rm-cffi::x rm-cffi::y rm-cffi::z) (cffi:mem-aref fp 'rm-cffi::rm-vertex-3d i) rm-cffi::rm-vertex-3d)
        (setf (svref vertex-vector i) (vertex rm-cffi::x rm-cffi::y rm-cffi::z))))
    vertex-vector))

(defmethod new-vertex-from-* ((vertex-array v2d*))
  (let ((vertex-vector (make-array (size vertex-array)))
        (fp (fp vertex-array)))
    (dotimes (i (size vertex-array))
      (cffi:with-foreign-slots ((rm-cffi::x rm-cffi::y) (cffi:mem-aref fp 'rm-cffi::rm-vertex-2d i) rm-cffi::rm-vertex-2d)
        (setf (svref vertex-vector i) (vertex rm-cffi::x rm-cffi::y))))
    vertex-vector))

(defmethod new-vertex-from-* ((vertex-array v3d*))
  (let ((vertex-vector (make-array (size vertex-array)))
        (fp (fp vertex-array)))
    (dotimes (i (size vertex-array))
      (cffi:with-foreign-slots ((rm-cffi::x rm-cffi::y rm-cffi::z) (cffi:mem-aref fp 'rm-cffi::rm-vertex-3d i) rm-cffi::rm-vertex-3d)
        (setf (svref vertex-vector i) (vertex rm-cffi::x rm-cffi::y rm-cffi::z))))
    vertex-vector))

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

(defmethod nth-vertex ((self vector) index)
  (svref self index))

(defmethod nth-vertex ((self v2d*) index)
  (when (>= index (size self))
    (error "NTH-VERTEX, index ~A out of range ~A" index (size self)))
  (make-instance 'v2d :gc nil :fp (cffi:mem-aref (fp self) 'rm-cffi::rm-vertex-2d index)
                 :x nil :y nil))
(defmethod nth-vertex ((self v3d*) index)
  (when (>= index (size self))
    (error "NTH-VERTEX, index ~A out of range ~A" index (size self)))
  (make-instance 'v3d :gc nil :fp (cffi:mem-aref (fp self) 'rm-cffi::rm-vertex-3d index)
                 :x nil :y nil :z nil))

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
