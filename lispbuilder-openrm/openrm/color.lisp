
(in-package #:rm)

(defclass color (foreign-object copyable-object)
  ((create-function
    :initform nil 
    :initarg :create)
   (size
    :reader size
    :initform nil
    :initarg :size))
  (:default-initargs
   :gc t
   :copy-p nil
   :free-on-delete t))

(defclass color-3d (color)())

(defclass color-4d (color)())

(defclass color-single (color)()
  (:default-initargs
   :free (simple-free #'cffi:foreign-free 'color-single)
   :size 1))

(defclass c3d (color-single color-3d)()
  (:default-initargs
   :fp (cffi:foreign-alloc 'rm-cffi::rm-color-3d)
   :r 0.0 :g 0.0 :b 0.0
   :free-on-delete-fn 'color-3d-proc))

(defclass c4d (color-single color-4d)()
  (:default-initargs
   :fp (cffi:foreign-alloc 'rm-cffi::rm-color-4d)
   :r 0.0 :g 0.0 :b 0.0 :a 0.0
   :free-on-delete-fn 'color-4d-proc))

(defclass color-array (color)())

(defclass c3d* (color-array color-3d)()
  (:default-initargs
   :create #'rm-cffi::rm-color-3d-new
   :free (simple-free #'rm-cffi::rm-color-3d-delete 'c3d*)
   :free-on-delete-fn 'color-3d-proc))

(defclass c4d* (color-array color-4d)()
  (:default-initargs
   :create #'rm-cffi::rm-color-4d-new
   :free (simple-free #'rm-cffi::rm-color-4d-delete 'c4d*)
   :free-on-delete-fn 'color-4d-proc))

(defmethod initialize-instance :after ((self color-array) &key)
  (when (and (slot-value self 'create-function) (size self))
    (setf (slot-value self 'foreign-pointer-to-object) (funcall (slot-value self 'create-function) (size self)))))

(defmethod initialize-instance :after ((self c3d)
                                       &key r g b)
  (let ((fp (fp self)))
    (when r
      (setf (cffi:foreign-slot-value fp 'rm-cffi::rm-color-3d 'rm-cffi::r) r))
    (when g
      (setf (cffi:foreign-slot-value fp 'rm-cffi::rm-color-3d 'rm-cffi::g) g))
    (when b
      (setf (cffi:foreign-slot-value fp 'rm-cffi::rm-color-3d 'rm-cffi::b) b))))

(defmethod initialize-instance :after ((self c4d)
                                       &key r g b a)
    (let ((fp (fp self)))
      (when r
        (setf (cffi:foreign-slot-value fp 'rm-cffi::rm-color-4d 'rm-cffi::r) r))
      (when g
        (setf (cffi:foreign-slot-value fp 'rm-cffi::rm-color-4d 'rm-cffi::g) g))
      (when b
        (setf (cffi:foreign-slot-value fp 'rm-cffi::rm-color-4d 'rm-cffi::b) b))
      (when a
        (setf (cffi:foreign-slot-value fp 'rm-cffi::rm-color-4d 'rm-cffi::a) a))))

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

(defmethod r ((color vector))
  (svref color 0))
(defmethod g ((color vector))
  (svref color 1))
(defmethod b ((color vector))
  (svref color 2))
(defmethod a ((color vector))
  (when (> (length color) 3)
    (svref color 3)))
(defmethod (setf r) (value (color vector))
  (setf (svref color 0) value))
(defmethod (setf g) (value (color vector))
  (setf (svref color 1) value))
(defmethod (setf b) (value (color vector))
  (setf (svref color 2) value))
(defmethod (setf a) (value (color vector))
  (when (> (length color) 3)
    (setf (svref color 3) value)))

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

(defmethod rgb/a ((color c3d))
  (cffi:with-foreign-slots ((rm-cffi::r rm-cffi::g rm-cffi::b) (fp color) rm-cffi::rm-color-3d)
    (color rm-cffi::r rm-cffi::g rm-cffi::b)))
(defmethod rgb/a ((color c4d))
  (cffi:with-foreign-slots ((rm-cffi::r rm-cffi::g rm-cffi::b rm-cffi::a) (fp color) rm-cffi::rm-color-4d)
    (color rm-cffi::r rm-cffi::g rm-cffi::b rm-cffi::a)))

(defmethod (setf rgb/a) ((value vector) (color c3d))
  (cffi:with-foreign-slots ((rm-cffi::r rm-cffi::g rm-cffi::b) (fp color) rm-cffi::rm-color-3d)
    (setf rm-cffi::r (r value)
          rm-cffi::g (g value)
          rm-cffi::b (b value))))

(defmethod (setf rgb/a) ((value vector) (color c4d))
  (cffi:with-foreign-slots ((rm-cffi::r rm-cffi::g rm-cffi::b rm-cffi::a) (fp color) rm-cffi::rm-color-4d)
    (setf rm-cffi::r (r value)
          rm-cffi::g (g value)
          rm-cffi::b (b value)
          rm-cffi::a (a value))))

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
      (vector r g b a)
      (vector r g b)))

(defun c3d (r g b &optional fp)
  (if fp
    (make-instance 'c3d :fp fp :r r :g g :b b)
    (make-instance 'c3d :r r :g g :b b)))

(defun c4d (r g b a &optional fp)
  (if fp
    (make-instance 'c4d :fp fp :r r :g g :b b :a a)
    (make-instance 'c4d :r r :g g :b b :a a)))

;; (defmethod copy-color-to-foreign ((color color-3d) foreign-pointer)
;;   (rm-base:with-rm-color-3d (col foreign-pointer nil)
;;     (setf rm-base:r (r color)
;; 	  rm-base:g (g color)
;; 	  rm-base:b (b color)))
;;   foreign-pointer)

(defmethod color* (size &key (initial-element #(0.0 0.0 0.0)) initial-contents)
  "Create an array of vertices of length `SIZE` or `:INITAL-CONTENTS`.
When `SIZE` is specified the array is initialized to `:INITIAL-ELEMENT`.
`:INITIAL-ELEMENT` must be a 3D or 4D color, for example \(color 0.0 0.0 0.0\).
When `SIZE` is not specified the array is initialized from `:INITIAL-CONTENTS`.
`:INITIAL-CONTENTS` can be a list or a vector of 3D or 4D colors, for example
'\(#\(0.0 0.0 0.0\) #\(1.0 1.0 1.0\)\)."
  (unless size
    (when initial-element
      (error "ERROR - COLOR*; SIZE must be specified if :INITIAL-ELEMENT is specified."))
    (unless initial-contents
      (error "ERROR - COLOR*; SIZE or :INITIAL-CONTENTS must be specified.")))
  (unless (vectorp initial-element)
    (error "ERROR - COLOR*; :INITIAL-ELEMENT must be a COLOR."))
  (when initial-contents
    (unless (vectorp (elt initial-contents 0))
      (error "ERROR - COLOR*; :INITIAL-CONTENTS must contain COLORs.")))
  (if size
    (cond
     (initial-contents
      (make-array (length initial-contents) :initial-contents initial-contents))
     (initial-element
      (make-array size :initial-element initial-element)))))

(defmethod c3d* (size &key initial-element initial-contents)
  (unless size
    (when initial-element
      (error "ERROR - C3D*; SIZE must be specified if :INITIAL-ELEMENT is specified."))
    (unless initial-contents
      (error "ERROR - C3D*; SIZE or :INITIAL-CONTENTS must be specified.")))
  (if size
    (let* ((color-array (make-instance 'c3d* :size size))
           (fp (fp color-array)))
      (when initial-element
        (dotimes (i size)
          (cffi:with-foreign-slots ((rm-cffi::r rm-cffi::g rm-cffi::b)
                                    (cffi:mem-aref fp 'rm-cffi::rm-color-3d i) rm-cffi::rm-color-3d)
            (setf rm-cffi::r (r initial-element)
                  rm-cffi::g (g initial-element)
                  rm-cffi::b (b initial-element)))))
      color-array)
    (let* ((color-array (make-instance 'c3d* :size (length initial-contents)))
           (fp (fp color-array)))
      (if (vectorp initial-contents)
        ;; Loop over VECTOR
        (loop
         :for i :from 0 :below (size color-array)
         :for v :across initial-contents
         :do (cffi:with-foreign-slots ((rm-cffi::r rm-cffi::g rm-cffi::b)
                                       (cffi:mem-aref fp 'rm-cffi::rm-color-3d i) rm-cffi::rm-color-3d)
               (setf rm-cffi::r (r v)
                     rm-cffi::g (g v)
                     rm-cffi::b (b v))))
        (loop
         ;; Loop over LIST
         :for i :from 0 :below (size color-array)
         :for v :in initial-contents
         :do (cffi:with-foreign-slots ((rm-cffi::r rm-cffi::g rm-cffi::b)
                                       (cffi:mem-aref fp 'rm-cffi::rm-color-3d i) rm-cffi::rm-color-3d)
               (setf rm-cffi::r (r v)
                     rm-cffi::g (g v)
                     rm-cffi::b (b v)))))
      color-array)))

(defmethod c4d* (size &key initial-element initial-contents)
  (unless size
    (when initial-element
      (error "ERROR - C4d*; SIZE must be specified if :INITIAL-ELEMENT is specified."))
    (unless initial-contents
      (error "ERROR - C4d*; SIZE or :INITIAL-CONTENTS must be specified.")))
  (if size
    (let* ((color-array (make-instance 'c4d* :size size))
           (fp (fp color-array)))
      (when initial-element
        (dotimes (i size)
          (cffi:with-foreign-slots ((rm-cffi::r rm-cffi::g rm-cffi::b rm-cffi::a)
                                    (cffi:mem-aref fp 'rm-cffi::rm-color-4d i) rm-cffi::rm-color-4d)
            (setf rm-cffi::r (r initial-element)
                  rm-cffi::g (g initial-element)
                  rm-cffi::b (b initial-element)
                  rm-cffi::a (a initial-element)))))
      color-array)
    (let* ((color-array (make-instance 'c4d* :size (length initial-contents)))
           (fp (fp color-array)))
      (if (vectorp initial-contents)
        ;; Loop over VECTOR
        (loop
         :for i :from 0 :below (size color-array)
         :for v :across initial-contents
         :do (cffi:with-foreign-slots ((rm-cffi::r rm-cffi::g rm-cffi::b rm-cffi::a)
                                       (cffi:mem-aref fp 'rm-cffi::rm-color-4d i) rm-cffi::rm-color-4d)
               (setf rm-cffi::r (r v)
                     rm-cffi::g (g v)
                     rm-cffi::b (b v)
                     rm-cffi::a (a v))))
        (loop
         ;; Loop over LIST
         :for i :from 0 :below (size color-array)
         :for v :in initial-contents
         :do (cffi:with-foreign-slots ((rm-cffi::r rm-cffi::g rm-cffi::b rm-cffi::a)
                                       (cffi:mem-aref fp 'rm-cffi::rm-color-4d i) rm-cffi::rm-color-4d)
               (setf rm-cffi::r (r v)
                     rm-cffi::g (g v)
                     rm-cffi::b (b v)
                     rm-cffi::a (a v)))))
      color-array)))

(defmethod color-array ((colors list))
  (c4d* colors))

(defmacro with-copy-color-3d-to-foreign ((color fp) &body body)
  `(cffi:with-foreign-object (,fp 'rm-cffi::rm-color-3d)
     (cffi:with-foreign-slots ((rm-cffi::r rm-cffi::g rm-cffi::b)
                               ,fp rm-cffi::rm-color-3d)
       (setf rm-cffi::r (r ,color)
             rm-cffi::g (g ,color)
             rm-cffi::b (b ,color))
       ,@body)))

(defmacro with-copy-color-4d-to-foreign ((color fp) &body body)
  `(cffi:with-foreign-object (,fp 'rm-cffi::rm-color-4d)
     (cffi:with-foreign-slots ((rm-cffi::r rm-cffi::g rm-cffi::b rm-cffi::a)
                               ,fp rm-cffi::rm-color-4d)
       (setf rm-cffi::r (r ,color)
             rm-cffi::g (g ,color)
             rm-cffi::b (b ,color)
             rm-cffi::a (a ,color))
       ,@body)))

(defmacro with-copy-color-3d-array-to-foreign ((color fp) &body body)
  (let ((i (gensym "i"))
        (c (gensym "c")))
    `(cffi:with-foreign-object (,fp 'rm-cffi::rm-color-3d (length ,color))
       (loop
        :for ,i :from 0 :below (length ,color)
        :for ,c :across color
        :do (cffi:with-foreign-slots ((rm-cffi::r rm-cffi::g rm-cffi::b)
                                      (cffi:mem-aref ,fp 'rm-cffi::rm-color-3d ,i) rm-cffi::rm-color-3d)
              (setf rm-cffi::r (r ,c)
                    rm-cffi::g (g ,c)
                    rm-cffi::b (b ,c))))
       ,@body)))

(defmacro with-copy-color-4d-array-to-foreign ((color fp) &body body)
  (let ((i (gensym "i"))
        (c (gensym "c")))
    `(cffi:with-foreign-object (,fp 'rm-cffi::rm-color-4d (length ,color))
       (loop
        :for ,i :from 0 :below (length ,color)
        :for ,c :across color
        :do (cffi:with-foreign-slots ((rm-cffi::r rm-cffi::g rm-cffi::b rm-cffi::a)
                                      (cffi:mem-aref ,fp 'rm-cffi::rm-color-4d ,i) rm-cffi::rm-color-4d)
              (setf rm-cffi::r (r ,c)
                    rm-cffi::g (g ,c)
                    rm-cffi::b (b ,c)
                    rm-cffi::a (a ,c))))
       ,@body)))
 
(defmethod copy-color-to-foreign ((color c4d) foreign-pointer &optional (index 0))
  (declare (ignore index))
  (rm-base:with-c4d (col foreign-pointer nil)
    (setf rm-base:r (r color)
	  rm-base:g (g color)
	  rm-base:b (b color)
	  rm-base:a (a color)))
  foreign-pointer)
(defmethod copy-color-to-foreign ((color c4d*) foreign-pointer &optional (index 0))
  (rm-base:with-c4d (col foreign-pointer nil)
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

(cffi:defcallback color-3d-proc :pointer
    ((data-fp :pointer))
  "Called when a color-3d is deleted"
  (log5:log-for (info) "DEFCALLBACK:COLOR-3D")
  (cffi:foreign-free data-fp)
  ;;(rm-cffi::rm-color-3d-delete data-fp)
  (cffi:null-pointer))

(cffi:defcallback color-4d-proc :pointer
    ((data-fp :pointer))
  "Called when a color-4d is deleted"
  (log5:log-for (info) "DEFCALLBACK:COLOR-4D")
  (cffi:foreign-free data-fp)
  ;;(rm-cffi::rm-color-4d-delete data-fp)
  (cffi:null-pointer))
