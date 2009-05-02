
(in-package #:rm)

(defclass sphere (node)()
  (:default-initargs
   :dims :rm-renderpass-3d
   :opacity :rm-renderpass-all
   :compute-bounding-box t
   :radius 1.0
   :tesselate 512
   :p-xy/z #(0.0 0.0 0.0)))

(defclass cone (node)()
  (:default-initargs
   :dims :rm-renderpass-3d
   :opacity :rm-renderpass-opaque
   :compute-bounding-box t
   :p-xy/z #(#(1.0 0.0 0.0)
             #(0.0 0.0 0.0))
   :radius #(1.0 1.0 1.0)
   :rgb/a #(1.0 1.0 1.0 1.0)))

(defclass cylinder (node)()
  (:default-initargs
   :dims :rm-renderpass-3d
   :opacity :rm-renderpass-opaque
   :compute-bounding-box t
   :p-xy/z #(#(0.0 0.0 0.0)
             #(1.0 0.0 0.0))
   :radius #(1.0 1.0 1.0)
   :rgb/a  #(1.0 1.0 1.0 1.0)))

(defclass box (node)()
  (:default-initargs
   :dims :rm-renderpass-3d
   :opacity :opaque
   :compute-bounding-box t
   :p-xy/z #(#(-1.0 -1.0 -1.0)
             #( 1.0  1.0  1.0))
   :type :solid))

(defclass app-list (node)()
  (:default-initargs
   :dims :rm-renderpass-3d
   :opacity :opaque
   :compute-bounding-box t))

(defclass sprite (node)()
  (:default-initargs
   :dims :rm-renderpass-all
   :opacity :opaque
   :compute-bounding-box t
   :p-xy/z #(0.0 0.0)))

(defun replace-opacity-keyword (opacity keywords)
  (let* ((keywords (if keywords keywords (list :opacity opacity)))
         (keywords (if (position :opacity keywords)
                     keywords
                     (append (list :opacity opacity) keywords))))
    (replace keywords (list :opacity (cond ((eq opacity :opaque)
                                            :rm-renderpass-opaque)
                                           ((eq opacity :transparent)
                                            :rm-renderpass-transparent)
                                           ((eq opacity :all)
                                            :rm-renderpass-all)
                                           (t (error "Cannot set the opacity to ~A." opacity))))
             :start1 (position :opacity keywords))))

(defmacro def-procedural-object (obj)
  `(defmethod initialize-instance :after ((self ,obj) &key
                                          ;; Primitive KEYwords
                                          rgb/a p-xy/z normals
                                          p-bounding-box p-compute-bounding-box
                                          display-list app-display-list
                                          ;; Procedural Primitive KEYwords
                                          tesselate radius)
     (add-to-node self (make-instance ',(intern (format nil "~A-PRIMITIVE" obj))
                                      :rgb/a rgb/a :xy/z p-xy/z :normals normals
                                      :bounding-box p-bounding-box
                                      :compute-bounding-box p-compute-bounding-box
                                      :tesselate tesselate :radius radius
                                      :display-list display-list
                                      :app-display-list app-display-list))))
(defmacro def-new-object (obj)
  `(defun ,(intern (format nil "NEW-~A" obj))
          (&rest rest &key (opacity :opaque) &allow-other-keys)
       (apply #'make-instance ',obj (replace-opacity-keyword opacity rest))))

(defmethod initialize-instance :after ((self box) &key
                                       type
                                       ;; Primitive KEYwords
                                       rgb/a p-xy/z normals
                                       p-bounding-box p-compute-bounding-box
                                       display-list app-display-list)
  (add-to-node self (cond ((eq type :solid)
                           (make-instance 'box-solid-primitive
                                          :rgb/a rgb/a :xy/z p-xy/z :normals normals
                                          :bounding-box p-bounding-box
                                          :compute-bounding-box p-compute-bounding-box
                                          :display-list display-list
                                          :app-display-list app-display-list))
                          ((eq type :wire)
                           (make-instance 'box-wire-primitive
                                          :rgb/a rgb/a :xy/z p-xy/z :normals normals
                                          :bounding-box p-bounding-box
                                          :compute-bounding-box p-compute-bounding-box
                                          :display-list display-list
                                          :app-display-list app-display-list))
                          (t (error "Cannot create BOX of type ~A" type)))))

(defmethod initialize-instance :after ((self app-list) &key
                                       ;; Primitive KEYwords
                                       rgb/a p-xy/z normals
                                       p-bounding-box p-compute-bounding-box
                                       display-list app-display-list)
  (add-to-node self (make-instance 'app-list-primitive
                                   :rgb/a rgb/a :xy/z p-xy/z :normals normals
                                   :bounding-box p-bounding-box
                                   :compute-bounding-box p-compute-bounding-box
                                   :display-list display-list
                                   :app-display-list app-display-list)))

(defmethod initialize-instance :after ((self sprite) &key
                                       images
                                       ;; Primitive KEYwords
                                       rgb/a p-xy/z normals
                                       p-bounding-box p-compute-bounding-box
                                       display-list app-display-list)
  (add-to-node self (make-instance 'sprite-primitive
                                   :images images
                                   :rgb/a rgb/a :xy/z p-xy/z :normals normals
                                   :bounding-box p-bounding-box
                                   :compute-bounding-box p-compute-bounding-box
                                   :display-list display-list
                                   :app-display-list app-display-list)))

(def-procedural-object sphere)
(def-procedural-object cylinder)
(def-procedural-object cone)

(def-new-object sphere)
(def-new-object cylinder)
(def-new-object cone)
(def-new-object box)
(def-new-object app-list)
(def-new-object sprite)


(defclass spotlight (node)
  ((target-node
    :reader target-node
    :initarg :target
    :initform (error "ERROR; SPOTLIGHT.INITIALIZE-INSTANCE; :TARGET must be specified."))
   (light-source
    :reader light-source
    :initarg :light-source)
   (light-direction
    :reader light-direction))
  (:default-initargs
   :dims :rm-renderpass-all
   :opacity :rm-renderpass-all
   :compute-bounding-box t
   :radius (coerce (rm::to-radian *default-spot-cutoff*) 'single-float)
   :target nil
   :light-source :rm-light-1
   :diffuse-color #(0.9 0.5 0.5 1.0)
   :specular-color #(0.9 0.5 0.5 1.0)
   :cutoff *default-spot-cutoff*
   :exponent 4.0
   :xy/z #(0.0 0.0 0.0)
   :direction #(0.0 -1.0 0.0)))

(def-new-object spotlight)

(defmethod initialize-instance :after ((self spotlight) &key
                                       ;; Node KEYwords
                                       xy/z
                                       name
                                       ;; spotlight KEYwords
                                       target
                                       ;; Primitive KEYwords
                                       rgb/a normals
                                       p-bounding-box p-compute-bounding-box
                                       tesselate radius
                                       display-list app-display-list
                                       ;; Light KEYwords
                                       light-source
                                       diffuse-color specular-color
                                       cutoff exponent
                                       direction)
  (unless target
    (setf (slot-value self 'target-node) (rm-root-node)))
  
  (setf (slot-value self 'light-direction) direction)
  
  (setf (light target) (make-instance 'spot-light
                                      :light-source light-source
                                      :diffuse-color diffuse-color
                                      :specular-color specular-color
                                      :cutoff cutoff
                                      :exponent exponent
                                      :xy/z xy/z
                                      :direction direction))
  (add-node (make-instance 'cone-primitive
                           :rgb/a rgb/a
                           :xy/z (vector direction #(0.0 0.0 0.0))
                           :normals normals
                           :bounding-box p-bounding-box
                           :compute-bounding-box p-compute-bounding-box
                           :tesselate tesselate :radius radius
                           :display-list display-list
                           :app-display-list app-display-list)
            self))

(defmethod rotate :after ((self spotlight) (node node) &key (reverse nil))
  (declare (ignore node reverse))
  ;; Retrieve the light at the Spotlight's 'target-node'.
  (let ((temp-light (get-light (target-node self) (light-source self))))
    ;; Point the 'target-node' in the direction that the cone is facing.
    (setf (direction temp-light)
          (rm::point-direction self (light-direction self)))
    ;; Add the light back to the 'target-node'
    (setf (light (target-node self)) temp-light)
    (free temp-light)))

