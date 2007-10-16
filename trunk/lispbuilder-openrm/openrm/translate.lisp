
;; OpenRM library using CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>
;; see COPYING for license

(in-package #:lispbuilder-openrm)

(defun create-list-if-not (var)
  (if (listp var)
      var
      (list var)))

(defun vertex-copy (vertices vertex-array)
  (let ((index 0))
    (mapcar #'(lambda (vertex)
		(copy-to-foreign-vertex vertex (cffi:mem-aref vertex-array 'rm::rmvertex3d index))
		(incf index))
	    vertices))
  vertex-array)

(defun color-copy (colors col-array)
  (let ((index 0))
    (mapcar #'(lambda (color)
		(copy-to-foreign-color color (cffi:mem-aref col-array 'rm::RMcolor4D index))
		(incf index))
	    colors))
  col-array)

(defctype rm-vertex-3d rm::rmvertex3d)
(defctype rm-color-3d rm::rmcolor3d)
(defctype rm-color-4d rm::rmcolor4d)
(defctype rm-enum rm::rmenum)

(defctype float-pointer :pointer)
(defctype float-array :pointer)
(defctype s-float :float)
(defctype int-array :pointer)

(defcstruct matrix
  (m s-float :count 16))

(defcfun ("rmNodeSetTranslateVector" set-node-position) :int
  (toModify :pointer)
  (newVector rm-vertex-3d))

(defcfun ("rmPrimitiveSetVertex3D" PrimitiveSetVertex3D) :int
  (toModify :pointer)
  (nVertices :int)
  (vertexData rm-vertex-3d)
  (copyEnum rmenum)
  (appFreeFunc :pointer))

(defcfun ("rmPrimitiveSetIndices" PrimitiveSetIndices) :int
  (toModify :pointer)
  (numIndices :int)
  (indicesArray int-array)
  (copyEnum rmenum)
  (appFreeFunc :pointer))


(defcfun ("rmNodeSetCenter" NodeSetCenter) :int
  (toModify :pointer)
  (newVertex rm-vertex-3d))

(defcfun ("rmLightSetXYZ" LightSetXYZ) :int
  (toModify :pointer)
  (newXYZ rm-vertex-3d))

(defcfun ("rmauxArcBall" auxArcBall) :void
  (x1 float-pointer)
  (y1 float-pointer)
  (x2 float-pointer)
  (y2 float-pointer)
  (result :pointer))

(defcfun ("rmauxDolly" auxDolly) :void
  (toModify :pointer)
  (x1 float-pointer)
  (y1 float-pointer)
  (x2 float-pointer)
  (y2 float-pointer))

(defcfun ("rmauxTranslate" auxTranslate) :void
  (toModify :pointer)
  (x1 float-pointer)
  (y1 float-pointer)
  (x2 float-pointer)
  (y2 float-pointer))

(defcfun ("rmLightSetColor" LightSetColor) :int
  (toModify :pointer)
  (newAmbientColor rm-color-4d)
  (newDiffuseColor rm-color-4d)
  (newSpecularColor rm-color-4d))

(defcfun ("rmNodeSetSceneBackgroundColor" NodeSetSceneBackgroundColor) :int
  (toModify :pointer)
  (newColor rm-color-4d))

(defcfun ("rmPrimitiveSetColor3D" PrimitiveSetColor3D) :int
  (toModify :pointer)
  (nColors :int)
  (colorData rm-color-3d)
  (copyEnum rmenum)
  (appFreeFunc :pointer))

(defcfun ("rmPrimitiveSetColor4D" PrimitiveSetColor4D) :int
  (toModify :pointer)
  (nColors :int)
  (colorData rm-color-4d)
  (copyEnum rmenum)
  (appFreeFunc :pointer))

(defcfun ("rmPrimitiveSetRadii" PrimitiveSetRadii) :int
  (toModify :pointer)
  (nRadii :int)
  (radii float-array)
  (copyEnum rmenum)
  (freeFunc :pointer))

(defcfun ("rmPrimitiveNew" PrimitiveNew) :pointer
  (primType rmenum))

(defcfun ("rmPrimitiveSetNormal3D" PrimitiveSetNormal3D) :int
  (toModify :pointer)
  (nNormals :int)
  (normalsData rm-vertex-3d)
  (copyEnum rmenum)
  (freeFunc :pointer))

(defcfun ("rmNodeSetDiffuseColor" NodeSetDiffuseColor) rm::rm-enum
  (toModify :pointer)
  (newColor rm-color-4d))

(defcfun ("rmNodeSetSpecularColor" NodeSetSpecularColor) rm::rm-enum
  (toModify :pointer)
  (newColor rm-color-4d))

(defcfun ("rmNodeSetAmbientColor" NodeSetAmbientColor) rm::rm-enum
  (toModify :pointer)
  (newColor rm-color-4d))

(defcfun ("rmNodeSetSpecularExponent" NodeSetSpecularExponent) rm::rm-enum
  (toModify :pointer)
  (newValue :float))



(defcfun ("rmLightSetSpotDirection" LightSetSpotDirection) :int
  (toModify :pointer)
  (newSpotDirection rm-vertex-3d))

(defcfun ("rmLightSetSpotCutoff" LightSetSpotCutoff) :int
  (toModify :pointer)
  (newValue s-float))

(defcfun ("rmLightSetSpotExponent" LightSetSpotExponent) :int
  (toModify :pointer)
  (newValue s-float))

(defcfun ("rmPipeSetRenderPassEnable" PipeSetRenderPassEnable) :int
  (t_arg0 :pointer)
  (opaque3DEnable rm-enum)
  (transparent3DEnable rm-enum)
  (opaque2DEnable rm-enum))

(defcfun ("rmPipeSetProcessingMode" PipeSetProcessingMode) :int
  (toModify :pointer)
  (newMode rm-enum))

(defcfun ("rmPipeNew" PipeNew) :pointer
  (targetPlatform rmenum))

(defcfun ("rmNodeGetRotateMatrix" NodeGetRotateMatrix) rm::rm-enum
  (toQuery :pointer)
  (matrixReturn :pointer))

(defcfun ("rmNodeGetSceneCamera3D" NodeGetSceneCamera3D) rm::rm-enum
  (toQuery :pointer)
  (returnCamera :pointer))

(defcfun ("rmNodeGetTranslateVector" NodeGetTranslateVector) rm::rm-enum
  (toQuery :pointer)
  (returnVector :pointer))



(defmethod translate-from-foreign (value (type (eql 'rm::rm-enum)))
  (case value
    (-1 :RM_WHACKED)
    (0 :RM_FALSE)
    (1 :RM_TRUE)
    (2 :RM_CHILL)
    (otherwise value)))

(defmethod translate-to-foreign (value (type (eql 'rm::s-float)))
  (coerce value 'single-float))

(defmethod translate-to-foreign (value (type (eql 'rm::float-pointer)))
  (let ((float-ptr (cffi:foreign-alloc :float)))
    (setf (cffi:mem-aref float-ptr :float) value)
    (values float-ptr t)))

(defmethod free-translated-object (ptr (name (eql 'rm::float-pointer)) free-p)
  (if free-p
      (cffi:foreign-free ptr)))

(defmethod free-translated-object (ptr (name (eql 'rm::rm-vertex-3d)) array-p)
  (if array-p
      (rm::rmVertex3DDelete ptr)
      (cffi:foreign-free ptr)))

(defmethod translate-to-foreign (value (type (eql 'rm::rm-vertex-3d)))
  (let* ((value (create-list-if-not value))
	 (vertex-array (rm::rmVertex3DNew (length value))))
    (vertex-copy value vertex-array)
    (values vertex-array t)))

(defmethod translate-to-foreign (value (type (eql 'rm::rm-enum)))
  (if (keywordp value)
      (cffi:foreign-enum-value 'rm::rmenum value)
      (if (null value)
	  (cffi:foreign-enum-value 'rm::rmenum :RM_FALSE)
	  (cffi:foreign-enum-value 'rm::rmenum :RM_TRUE))))


(defmethod translate-to-foreign (value (type (eql 'rm::rm-color-4d)))
  (if (null value)
      (values (cffi:null-pointer) nil)
      (let* ((value (create-list-if-not value))
	     (color-array (rm::rmColor4DNew (length value))))
	(color-copy value color-array)
	(values color-array t))))

(defmethod translate-to-foreign (value (type (eql 'rm::rm-color-3d)))
  (if (null value)
      (values (cffi:null-pointer) nil)
      (let* ((value (create-list-if-not value))
	     (color-array (rm::rmColor3DNew (length value))))
	(color-copy value color-array)
	(values color-array t))))

(defmethod free-translated-object (ptr (name (eql 'rm::rm-color-4d)) array-p)
  (if array-p
      (rm::rmColor4DDelete ptr)
      (cffi:foreign-free ptr)))

(defmethod free-translated-object (ptr (name (eql 'rm::rm-color-3d)) array-p)
  (if array-p
      (rm::rmColor3DDelete ptr)
      (cffi:foreign-free ptr)))

(defmethod translate-to-foreign (value (type (eql 'rm::float-array)))
  (values (cffi:foreign-alloc :float :count (length value) :initial-contents value) t))

(defmethod free-translated-object (ptr (name (eql 'rm::float-array)) free-p)
  (if free-p
      (cffi:foreign-free ptr)))


