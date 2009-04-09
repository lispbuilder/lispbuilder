
(in-package #:rm)

(defclass color (openrm-object)
  ((create-function
    :initform nil 
    :initarg :create))
  (:default-initargs
   :gc t))

(defclass color-single (color)()
  (:default-initargs
   :free (simple-free #'cffi:foreign-free 'color-single)))

(defclass color-array (color)
  ((size
    :reader size
    :initform (error "Specify an array size.")
    :initarg :size)))

(defmethod initialize-instance :after ((self color-array) &key)
  (setf (slot-value self 'foreign-pointer-to-object) (funcall (slot-value self 'create-function) (size self))))

(defclass c3d (color-single)()
  (:default-initargs
    :fp (cffi:foreign-alloc 'rm-cffi::rm-color-3d)))

(defmethod initialize-instance :around ((self c3d)
				       &key (r 0.0) (g 0.0) (b 0.0))
  (call-next-method)
  (setf (cffi:foreign-slot-value (fp self) 'rm-cffi::rm-color-3d 'rm-cffi::r) r
	(cffi:foreign-slot-value (fp self) 'rm-cffi::rm-color-3d 'rm-cffi::g) g
	(cffi:foreign-slot-value (fp self) 'rm-cffi::rm-color-3d 'rm-cffi::b) b))

(defclass c3d* (color-array)()
  (:default-initargs
   :create #'rm-cffi::rm-color-3d-new
   :free (simple-free #'rm-cffi::rm-color-3d-delete 'c3d*)))

(defclass c4d (color-single)()
  (:default-initargs
    :fp (cffi:foreign-alloc 'rm-cffi::rm-color-4d)))

(defmethod initialize-instance :around ((self c4d)
				       &key (r 0.0) (g 0.0) (b 0.0) (a 0.0))
  (call-next-method)
  (setf (cffi:foreign-slot-value (fp self) 'rm-cffi::rm-color-4d 'rm-cffi::r) r
	(cffi:foreign-slot-value (fp self) 'rm-cffi::rm-color-4d 'rm-cffi::g) g
	(cffi:foreign-slot-value (fp self) 'rm-cffi::rm-color-4d 'rm-cffi::b) b
	(cffi:foreign-slot-value (fp self) 'rm-cffi::rm-color-4d 'rm-cffi::a) a))

(defclass c4d* (color-array)()
  (:default-initargs
   :create #'rm-cffi::rm-color-4d-new
   :free (simple-free #'rm-cffi::rm-color-4d-delete 'c3d*)))

;; (defmethod initialize-instance :before ((self color-1D) &key)
;;   (setf (slot-value self 'foreign-pointer-to-object) (cffi:foreign-alloc 'rm-cffi::rm-color-1d))
;;   (cffi:with-foreign-slots ((rm-cffi::lum) (slot-value self 'foreign-pointer-to-object) rm-cffi::rm-color-1d)
;;     (setf rm-cffi::lum 0.0)))

;; (defmethod initialize-instance :before ((self color-2D) &key)
;;   (setf (slot-value self 'foreign-pointer-to-object) (cffi:foreign-alloc 'rm-cffi::rm-color-2d))
;;   (cffi:with-foreign-slots ((rm-cffi::lum rm-cffi::alpha) (slot-value self 'foreign-pointer-to-object) rm-cffi::rm-color-2d)
;;     (setf rm-cffi::lum 0.0
;; 	  rm-cffi::alpha 0.0)))

;; (defmethod initialize-instance :before ((self color-3D) &key)
;;   (setf (slot-value self 'foreign-pointer-to-object) (cffi:foreign-alloc 'rm-cffi::rm-color-3d))
;;   (cffi:with-foreign-slots ((rm-cffi::r rm-cffi::g rm-cffi::b) (slot-value self 'foreign-pointer-to-object) rm-cffi::rm-color-3d)
;;     (setf rm-cffi::r 0.0
;; 	  rm-cffi::g 0.0
;; 	  rm-cffi::b 0.0)))

;; (defmethod lum ((color color-1d))
;;   (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-1d 'rm-cffi::lum))
;; (defmethod (setf lum) (lum-val (color color-1d))
;;   (setf (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-1d 'rm-cffi::lum)
;; 	lum-val))

;; (defmethod lum ((color color-2d))
;;   (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-2d 'rm-cffi::lum))
;; (defmethod (setf lum) (lum-val (color color-2d))
;;   (setf (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-2d 'rm-cffi::lum)
;; 	lum-val))

;; (defmethod alpha ((color color-2d))
;;   (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-2d 'rm-cffi::alpha))
;; (defmethod (setf alpha) (alpha-val (color color-2d))
;;   (setf (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-2d 'rm-cffi::alpha)
;; 	alpha-val))


(defmethod r ((color c3d))
  (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-3d 'rm-cffi::r))
(defmethod (setf r) (r-val (color c3d))
  (setf (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-3d 'rm-cffi::r)
	r-val))
(defmethod g ((color c3d))
  (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-3d 'rm-cffi::g))
(defmethod (setf g) (g-val (color c3d))
  (setf (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-3d 'rm-cffi::g)
	g-val))
(defmethod b ((color c3d))
  (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-3d 'rm-cffi::b))
(defmethod (setf b) (b-val (color c3d))
  (setf (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-3d 'rm-cffi::b)
	b-val))
(defmethod a ((color c3d)) nil)
(defmethod (setf a) (a-val (color c3d)) nil)

(defmethod r ((color c4d))
  (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-4d 'rm-cffi::r))
(defmethod (setf r) (r-val (color c4d))
  (setf (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-4d 'rm-cffi::r)
	r-val))
(defmethod g ((color c4d))
  (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-4d 'rm-cffi::g))
(defmethod (setf g) (g-val (color c4d))
  (setf (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-4d 'rm-cffi::g)
	g-val))
(defmethod b ((color c4d))
  (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-4d 'rm-cffi::b))
(defmethod (setf b) (b-val (color c4d))
  (setf (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-4d 'rm-cffi::b)
	b-val))
(defmethod a ((color c4d))
  (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-4d 'rm-cffi::a))
(defmethod (setf a) (a-val (color c4d))
  (setf (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-4d 'rm-cffi::a)
	a-val))

;; (defun color-1d (lum &optional fp)
;;   (when fp
;;     (unless (is-valid-ptr fp)
;;       (error "FP must be a foreign pointer.")))
;;   (let ((color (if fp
;; 		   (make-instance 'color-1d :fp fp)
;; 		   (make-instance 'color-1d))))
;;     (unless fp (setf (lum color) lum))
;;     color))

;; (defun color-2d (lum alpha &optional fp)
;;   (when fp
;;     (unless (is-valid-ptr fp)
;;       (error "FP must be a foreign pointer.")))
;;   (let ((color (if fp
;; 		   (make-instance 'color-2d :fp fp)
;; 		   (make-instance 'color-2d))))
;;     (unless fp (setf (lum color) lum
;; 		     (alpha color) alpha))
;;     color))

(defun color (r g b &optional a)
  (if a
      (make-instance 'c4d :r r :g g :b b :a a)
      (make-instance 'c3d :r r :g g :b b)))

(defun c3d (r g b &optional fp)
  (when fp
    (unless (is-valid-ptr fp)
      (error "FP must be a foreign pointer.")))
  (let ((color (if fp
		   (make-instance 'c3d :fp fp)
		   (make-instance 'c3d))))
    (unless fp (setf (r color) r
		     (g color) g
		     (b color) b))
    color))

(defun c4d (r g b a &optional fp)
  (when fp
    (unless (is-valid-ptr fp)
      (error "FP must be a foreign pointer.")))
  (let ((color (if fp
		   (make-instance 'c4d :fp fp)
		   (make-instance 'c4d))))
    (unless fp (setf (r color) r
		     (g color) g
		     (b color) b
		     (a color) a))
    color))

;; (defun color (r g b a &optional (fp nil))
;;   (c4d r g b a fp))

;; (defmethod copy-color-to-foreign ((color color-3d) foreign-pointer)
;;   (rm-base:with-rm-color-3d (col foreign-pointer nil)
;;     (setf rm-base:r (r color)
;; 	  rm-base:g (g color)
;; 	  rm-base:b (b color)))
;;   foreign-pointer)

(defmethod c3d* ((color c3d) &optional size)
  (when size
    (let ((color-array (make-instance 'c3d* :size size)))
      (let ((color-array-fp (fp color-array))
	    (3d (cffi:foreign-type-size 'rm-cffi::rm-color-3d)))
	(loop
	   for i from 0 upto (1- (size color-array))
	   do (progn (setf (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-3d 'rm-cffi::r) (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-3d 'rm-cffi::r)
			   (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-3d 'rm-cffi::g) (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-3d 'rm-cffi::g)
			   (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-3d 'rm-cffi::b) (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-3d 'rm-cffi::b))
		     (setf color-array-fp (cffi:inc-pointer color-array-fp 3d)))))
      color-array)))
(defmethod c3d* ((colors list) &optional size)
  (if (consp colors)
      (let ((color-array (make-instance 'c3d* :size (length colors))))
	(let ((color-array-fp (fp color-array))
	      (3d (cffi:foreign-type-size 'rm-cffi::rm-color-3d)))
	  (loop
	     for i from 0 upto (1- (size color-array))
	     for c in colors
	     do (progn (if (consp c)
			   (setf (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-3d 'rm-cffi::r) (nth 0 c)
				 (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-3d 'rm-cffi::g) (nth 1 c)
				 (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-3d 'rm-cffi::b) (nth 2 c))
			   (setf (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-3d 'rm-cffi::r) (cffi:foreign-slot-value (fp c) 'rm-cffi::rm-color-3d 'rm-cffi::r)
				 (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-3d 'rm-cffi::g) (cffi:foreign-slot-value (fp c) 'rm-cffi::rm-color-3d 'rm-cffi::g)
				 (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-3d 'rm-cffi::b) (cffi:foreign-slot-value (fp c) 'rm-cffi::rm-color-3d 'rm-cffi::b)))
		       (setf color-array-fp (cffi:inc-pointer color-array-fp 3d)))))
	color-array)
      (when size
	(make-instance 'c3d* :size size))))


(defmethod c4d* ((color c4d) &optional size)
  (when size
    (let ((color-array (make-instance 'c4d* :size size)))
      (let ((color-array-fp (fp color-array))
	    (4d (cffi:foreign-type-size 'rm-cffi::rm-color-4d)))
	(loop
	   for i from 0 upto (1- (size color-array))
	   do (progn (setf (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-4d 'rm-cffi::r) (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-4d 'rm-cffi::r)
			   (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-4d 'rm-cffi::g) (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-4d 'rm-cffi::g)
			   (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-4d 'rm-cffi::b) (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-4d 'rm-cffi::b)
			   (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-4d 'rm-cffi::a) (cffi:foreign-slot-value (fp color) 'rm-cffi::rm-color-4d 'rm-cffi::a))
		     (setf color-array-fp (cffi:inc-pointer color-array-fp 4d)))))
      color-array)))
(defmethod c4d* ((colors list) &optional size)
  (if (consp colors)
      (let ((color-array (make-instance 'c4d* :size (length colors))))
	(let ((color-array-fp (fp color-array))
	      (4d (cffi:foreign-type-size 'rm-cffi::rm-color-4d)))
	  (loop
	     for i from 0 upto (1- (size color-array))
	     for c in colors
	     do (progn (if (consp c)
			   (setf (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-4d 'rm-cffi::r) (nth 0 c)
				 (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-4d 'rm-cffi::g) (nth 1 c)
				 (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-4d 'rm-cffi::b) (nth 2 c)
				 (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-4d 'rm-cffi::a) (nth 3 c))
			   (setf (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-4d 'rm-cffi::r) (cffi:foreign-slot-value (fp c) 'rm-cffi::rm-color-4d 'rm-cffi::r)
				 (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-4d 'rm-cffi::g) (cffi:foreign-slot-value (fp c) 'rm-cffi::rm-color-4d 'rm-cffi::g)
				 (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-4d 'rm-cffi::b) (cffi:foreign-slot-value (fp c) 'rm-cffi::rm-color-4d 'rm-cffi::b)
				 (cffi:foreign-slot-value color-array-fp 'rm-cffi::rm-color-4d 'rm-cffi::a) (cffi:foreign-slot-value (fp c) 'rm-cffi::rm-color-4d 'rm-cffi::a)))
		       (setf color-array-fp (cffi:inc-pointer color-array-fp 4d)))))
	color-array)
      (when size
	(make-instance 'c4d* :size size))))

(defmethod color-array ((colors list))
  (c4d* colors))

(defmethod copy-color-to-foreign ((color c4d) foreign-pointer &optional (index 0))
  (declare (ignore index))
  (rm-base:with-c4d (col foreign-pointer nil)
    (setf rm-base:r (r color)
	  rm-base:g (g color)
	  rm-base:b (b color)
	  rm-base:a (a color)))
  foreign-pointer)
(defmethod copy-color-to-foreign ((color c4d*) foreign-pointer &optional (index 0))
  (rm-base:with-c4d (vert foreign-pointer nil)
    (let ((v (nth-color color index)))
      (setf rm-base:r (r v)
	    rm-base:g (g v)
	    rm-base:b (b v)
	    rm-base:a (a v))))
  foreign-pointer)

(defmethod nth-color ((self c4d*) index)
  (when (>= index (size self))
    (error "NTH-COLOR, index ~A out of range ~A" index (size self)))
  (make-instance 'c4d :gc nil :fp (cffi:mem-aref (fp self) 'rm-cffi::rm-color-4d index)))

;; (defmethod copy-color ((dst c4d) (src list))
;;   (setf (r dst) (nth 0 src)
;; 	(g dst) (nth 1 src)
;; 	(b dst) (nth 2 src)
;; 	(a dst) (nth 3 src)))
;; (defmethod copy-color ((dst c4d) (src c4d))
;;   (setf (r dst) (r src)
;; 	(g dst) (g src)
;; 	(b dst) (b src)
;; 	(a dst) (a src)))


(defmethod copy-color ((dst c4d*) (src c4d) &key (start 0) (end nil))
  (let ((fp (fp dst))
	(end (if end end (size dst))))
    (loop for i from start below end do
	 (rm-base:with-c4d (col (cffi:mem-aref fp 'rm-cffi::rm-color-4d i) nil)
	   (setf rm-base:r (r src)
		 rm-base:g (g src)
		 rm-base:b (b src)
		 rm-base:a (a src)))))
  dst)

(defmethod copy-color (dst (src c4d) &key (start 0) (end nil))
  (let ((fp dst)
	(end (if end end (size dst))))
    (loop for i from start below end do
	 (rm-base:with-c4d (col (cffi:mem-aref fp 'rm-cffi::rm-color-4d i) nil)
	   (setf rm-base:r (r src)
		 rm-base:g (g src)
		 rm-base:b (b src)
		 rm-base:a (a src)))))
  dst)

(defmethod copy-color ((dst c3d*) (src c3d) &key (start 0) (end nil))
  (let ((fp (fp dst))
	(end (if end end (size dst))))
    (loop for i from start below end do
	 (rm-base:with-c3d (col (cffi:mem-aref fp 'rm-cffi::rm-color-3d i) nil)
	   (setf rm-base:r (r src)
		 rm-base:g (g src)
		 rm-base:b (b src)))))
  dst)
(defmethod copy-color (dst (src c3d) &key (start 0) (end nil))
  (let ((fp dst)
	(end (if end end (size dst))))
    (loop for i from start below end do
	 (rm-base:with-c3d (col (cffi:mem-aref fp 'rm-cffi::rm-color-3d i) nil)
	   (setf rm-base:r (r src)
		 rm-base:g (g src)
		 rm-base:b (b src)))))
  dst)
