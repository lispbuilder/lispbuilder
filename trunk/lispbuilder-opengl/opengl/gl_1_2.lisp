
;;;; gl.h Mesa v6.4.2 CFFI lisp wrapper
;;;; Part of the CL-Gardeners project
;;;; http://wiki.alu.org/Application_Builder
;;;; (C)2006 Luke J Crook
;;;; See COPYING for license

;;;; Note: (1) "gl.h" contains a LOT of typedef void (APIENTRYP blah blah blah) and
;;;;           none of these are yet defined.

(in-package #:lispbuilder-opengl)




;;;SWIG wrapper code starts here

(defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(progn ,@(loop for value in enums
                  for index = 0 then (1+ index)
                  when (listp value) do (setf index (second value)
                                              value (first value))
                  collect `(defconstant ,value ,index))))

;;;SWIG wrapper code ends here


(defconstant GL_RESCALE_NORMAL #x803A)

(defconstant GL_CLAMP_TO_EDGE #x812F)

(defconstant GL_MAX_ELEMENTS_VERTICES #x80E8)

(defconstant GL_MAX_ELEMENTS_INDICES #x80E9)

(defconstant GL_BGR #x80E0)

(defconstant GL_BGRA #x80E1)

(defconstant GL_UNSIGNED_BYTE_3_3_2 #x8032)

(defconstant GL_UNSIGNED_BYTE_2_3_3_REV #x8362)

(defconstant GL_UNSIGNED_SHORT_5_6_5 #x8363)

(defconstant GL_UNSIGNED_SHORT_5_6_5_REV #x8364)

(defconstant GL_UNSIGNED_SHORT_4_4_4_4 #x8033)

(defconstant GL_UNSIGNED_SHORT_4_4_4_4_REV #x8365)

(defconstant GL_UNSIGNED_SHORT_5_5_5_1 #x8034)

(defconstant GL_UNSIGNED_SHORT_1_5_5_5_REV #x8366)

(defconstant GL_UNSIGNED_INT_8_8_8_8 #x8035)

(defconstant GL_UNSIGNED_INT_8_8_8_8_REV #x8367)

(defconstant GL_UNSIGNED_INT_10_10_10_2 #x8036)

(defconstant GL_UNSIGNED_INT_2_10_10_10_REV #x8368)

(defconstant GL_LIGHT_MODEL_COLOR_CONTROL #x81F8)

(defconstant GL_SINGLE_COLOR #x81F9)

(defconstant GL_SEPARATE_SPECULAR_COLOR #x81FA)

(defconstant GL_TEXTURE_MIN_LOD #x813A)

(defconstant GL_TEXTURE_MAX_LOD #x813B)

(defconstant GL_TEXTURE_BASE_LEVEL #x813C)

(defconstant GL_TEXTURE_MAX_LEVEL #x813D)

(defconstant GL_SMOOTH_POINT_SIZE_RANGE #x0B12)

(defconstant GL_SMOOTH_POINT_SIZE_GRANULARITY #x0B13)

(defconstant GL_SMOOTH_LINE_WIDTH_RANGE #x0B22)

(defconstant GL_SMOOTH_LINE_WIDTH_GRANULARITY #x0B23)

(defconstant GL_ALIASED_POINT_SIZE_RANGE #x846D)

(defconstant GL_ALIASED_LINE_WIDTH_RANGE #x846E)

(defconstant GL_PACK_SKIP_IMAGES #x806B)

(defconstant GL_PACK_IMAGE_HEIGHT #x806C)

(defconstant GL_UNPACK_SKIP_IMAGES #x806D)

(defconstant GL_UNPACK_IMAGE_HEIGHT #x806E)

(defconstant GL_TEXTURE_3D #x806F)

(defconstant GL_PROXY_TEXTURE_3D #x8070)

(defconstant GL_TEXTURE_DEPTH #x8071)

(defconstant GL_TEXTURE_WRAP_R #x8072)

(defconstant GL_MAX_3D_TEXTURE_SIZE #x8073)

(defconstant GL_TEXTURE_BINDING_3D #x806A)

(defcfun ("glDrawRangeElements" glDrawRangeElements) :void
  (mode :unsigned-int)
  (start :unsigned-int)
  (end :unsigned-int)
  (count :int)
  (type :unsigned-int)
  (indices :pointer))

(defcfun ("glTexImage3D" glTexImage3D) :void
  (target :unsigned-int)
  (level :int)
  (internalFormat :int)
  (width :int)
  (height :int)
  (depth :int)
  (border :int)
  (format :unsigned-int)
  (type :unsigned-int)
  (pixels :pointer))

(defcfun ("glTexSubImage3D" glTexSubImage3D) :void
  (target :unsigned-int)
  (level :int)
  (xoffset :int)
  (yoffset :int)
  (zoffset :int)
  (width :int)
  (height :int)
  (depth :int)
  (format :unsigned-int)
  (type :unsigned-int)
  (pixels :pointer))

(defcfun ("glCopyTexSubImage3D" glCopyTexSubImage3D) :void
  (target :unsigned-int)
  (level :int)
  (xoffset :int)
  (yoffset :int)
  (zoffset :int)
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defconstant GL_CONSTANT_COLOR #x8001)

(defconstant GL_ONE_MINUS_CONSTANT_COLOR #x8002)

(defconstant GL_CONSTANT_ALPHA #x8003)

(defconstant GL_ONE_MINUS_CONSTANT_ALPHA #x8004)

(defconstant GL_COLOR_TABLE #x80D0)

(defconstant GL_POST_CONVOLUTION_COLOR_TABLE #x80D1)

(defconstant GL_POST_COLOR_MATRIX_COLOR_TABLE #x80D2)

(defconstant GL_PROXY_COLOR_TABLE #x80D3)

(defconstant GL_PROXY_POST_CONVOLUTION_COLOR_TABLE #x80D4)

(defconstant GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE #x80D5)

(defconstant GL_COLOR_TABLE_SCALE #x80D6)

(defconstant GL_COLOR_TABLE_BIAS #x80D7)

(defconstant GL_COLOR_TABLE_FORMAT #x80D8)

(defconstant GL_COLOR_TABLE_WIDTH #x80D9)

(defconstant GL_COLOR_TABLE_RED_SIZE #x80DA)

(defconstant GL_COLOR_TABLE_GREEN_SIZE #x80DB)

(defconstant GL_COLOR_TABLE_BLUE_SIZE #x80DC)

(defconstant GL_COLOR_TABLE_ALPHA_SIZE #x80DD)

(defconstant GL_COLOR_TABLE_LUMINANCE_SIZE #x80DE)

(defconstant GL_COLOR_TABLE_INTENSITY_SIZE #x80DF)

(defconstant GL_CONVOLUTION_1D #x8010)

(defconstant GL_CONVOLUTION_2D #x8011)

(defconstant GL_SEPARABLE_2D #x8012)

(defconstant GL_CONVOLUTION_BORDER_MODE #x8013)

(defconstant GL_CONVOLUTION_FILTER_SCALE #x8014)

(defconstant GL_CONVOLUTION_FILTER_BIAS #x8015)

(defconstant GL_REDUCE #x8016)

(defconstant GL_CONVOLUTION_FORMAT #x8017)

(defconstant GL_CONVOLUTION_WIDTH #x8018)

(defconstant GL_CONVOLUTION_HEIGHT #x8019)

(defconstant GL_MAX_CONVOLUTION_WIDTH #x801A)

(defconstant GL_MAX_CONVOLUTION_HEIGHT #x801B)

(defconstant GL_POST_CONVOLUTION_RED_SCALE #x801C)

(defconstant GL_POST_CONVOLUTION_GREEN_SCALE #x801D)

(defconstant GL_POST_CONVOLUTION_BLUE_SCALE #x801E)

(defconstant GL_POST_CONVOLUTION_ALPHA_SCALE #x801F)

(defconstant GL_POST_CONVOLUTION_RED_BIAS #x8020)

(defconstant GL_POST_CONVOLUTION_GREEN_BIAS #x8021)

(defconstant GL_POST_CONVOLUTION_BLUE_BIAS #x8022)

(defconstant GL_POST_CONVOLUTION_ALPHA_BIAS #x8023)

(defconstant GL_CONSTANT_BORDER #x8151)

(defconstant GL_REPLICATE_BORDER #x8153)

(defconstant GL_CONVOLUTION_BORDER_COLOR #x8154)

(defconstant GL_COLOR_MATRIX #x80B1)

(defconstant GL_COLOR_MATRIX_STACK_DEPTH #x80B2)

(defconstant GL_MAX_COLOR_MATRIX_STACK_DEPTH #x80B3)

(defconstant GL_POST_COLOR_MATRIX_RED_SCALE #x80B4)

(defconstant GL_POST_COLOR_MATRIX_GREEN_SCALE #x80B5)

(defconstant GL_POST_COLOR_MATRIX_BLUE_SCALE #x80B6)

(defconstant GL_POST_COLOR_MATRIX_ALPHA_SCALE #x80B7)

(defconstant GL_POST_COLOR_MATRIX_RED_BIAS #x80B8)

(defconstant GL_POST_COLOR_MATRIX_GREEN_BIAS #x80B9)

(defconstant GL_POST_COLOR_MATRIX_BLUE_BIAS #x80BA)

(defconstant GL_POST_COLOR_MATRIX_ALPHA_BIAS #x80BB)

(defconstant GL_HISTOGRAM #x8024)

(defconstant GL_PROXY_HISTOGRAM #x8025)

(defconstant GL_HISTOGRAM_WIDTH #x8026)

(defconstant GL_HISTOGRAM_FORMAT #x8027)

(defconstant GL_HISTOGRAM_RED_SIZE #x8028)

(defconstant GL_HISTOGRAM_GREEN_SIZE #x8029)

(defconstant GL_HISTOGRAM_BLUE_SIZE #x802A)

(defconstant GL_HISTOGRAM_ALPHA_SIZE #x802B)

(defconstant GL_HISTOGRAM_LUMINANCE_SIZE #x802C)

(defconstant GL_HISTOGRAM_SINK #x802D)

(defconstant GL_MINMAX #x802E)

(defconstant GL_MINMAX_FORMAT #x802F)

(defconstant GL_MINMAX_SINK #x8030)

(defconstant GL_TABLE_TOO_LARGE #x8031)

(defconstant GL_BLEND_EQUATION #x8009)

(defconstant GL_MIN #x8007)

(defconstant GL_MAX #x8008)

(defconstant GL_FUNC_ADD #x8006)

(defconstant GL_FUNC_SUBTRACT #x800A)

(defconstant GL_FUNC_REVERSE_SUBTRACT #x800B)

(defconstant GL_BLEND_COLOR #x8005)

(defcfun ("glColorTable" glColorTable) :void
  (target :unsigned-int)
  (internalformat :unsigned-int)
  (width :int)
  (format :unsigned-int)
  (type :unsigned-int)
  (table :pointer))

(defcfun ("glColorSubTable" glColorSubTable) :void
  (target :unsigned-int)
  (start :int)
  (count :int)
  (format :unsigned-int)
  (type :unsigned-int)
  (data :pointer))

(defcfun ("glColorTableParameteriv" glColorTableParameteriv) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glColorTableParameterfv" glColorTableParameterfv) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glCopyColorSubTable" glCopyColorSubTable) :void
  (target :unsigned-int)
  (start :int)
  (x :int)
  (y :int)
  (width :int))

(defcfun ("glCopyColorTable" glCopyColorTable) :void
  (target :unsigned-int)
  (internalformat :unsigned-int)
  (x :int)
  (y :int)
  (width :int))

(defcfun ("glGetColorTable" glGetColorTable) :void
  (target :unsigned-int)
  (format :unsigned-int)
  (type :unsigned-int)
  (table :pointer))

(defcfun ("glGetColorTableParameterfv" glGetColorTableParameterfv) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glGetColorTableParameteriv" glGetColorTableParameteriv) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glBlendEquation" glBlendEquation) :void
  (mode :unsigned-int))

(defcfun ("glBlendColor" glBlendColor) :void
  (red :float)
  (green :float)
  (blue :float)
  (alpha :float))

(defcfun ("glHistogram" glHistogram) :void
  (target :unsigned-int)
  (width :int)
  (internalformat :unsigned-int)
  (sink :unsigned-char))

(defcfun ("glResetHistogram" glResetHistogram) :void
  (target :unsigned-int))

(defcfun ("glGetHistogram" glGetHistogram) :void
  (target :unsigned-int)
  (reset :unsigned-char)
  (format :unsigned-int)
  (type :unsigned-int)
  (values :pointer))

(defcfun ("glGetHistogramParameterfv" glGetHistogramParameterfv) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glGetHistogramParameteriv" glGetHistogramParameteriv) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glMinmax" glMinmax) :void
  (target :unsigned-int)
  (internalformat :unsigned-int)
  (sink :unsigned-char))

(defcfun ("glResetMinmax" glResetMinmax) :void
  (target :unsigned-int))

(defcfun ("glGetMinmax" glGetMinmax) :void
  (target :unsigned-int)
  (reset :unsigned-char)
  (format :unsigned-int)
  (types :unsigned-int)
  (values :pointer))

(defcfun ("glGetMinmaxParameterfv" glGetMinmaxParameterfv) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glGetMinmaxParameteriv" glGetMinmaxParameteriv) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glConvolutionFilter1D" glConvolutionFilter1D) :void
  (target :unsigned-int)
  (internalformat :unsigned-int)
  (width :int)
  (format :unsigned-int)
  (type :unsigned-int)
  (image :pointer))

(defcfun ("glConvolutionFilter2D" glConvolutionFilter2D) :void
  (target :unsigned-int)
  (internalformat :unsigned-int)
  (width :int)
  (height :int)
  (format :unsigned-int)
  (type :unsigned-int)
  (image :pointer))

(defcfun ("glConvolutionParameterf" glConvolutionParameterf) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :float))

(defcfun ("glConvolutionParameterfv" glConvolutionParameterfv) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glConvolutionParameteri" glConvolutionParameteri) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :int))

(defcfun ("glConvolutionParameteriv" glConvolutionParameteriv) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glCopyConvolutionFilter1D" glCopyConvolutionFilter1D) :void
  (target :unsigned-int)
  (internalformat :unsigned-int)
  (x :int)
  (y :int)
  (width :int))

(defcfun ("glCopyConvolutionFilter2D" glCopyConvolutionFilter2D) :void
  (target :unsigned-int)
  (internalformat :unsigned-int)
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defcfun ("glGetConvolutionFilter" glGetConvolutionFilter) :void
  (target :unsigned-int)
  (format :unsigned-int)
  (type :unsigned-int)
  (image :pointer))

(defcfun ("glGetConvolutionParameterfv" glGetConvolutionParameterfv) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glGetConvolutionParameteriv" glGetConvolutionParameteriv) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glSeparableFilter2D" glSeparableFilter2D) :void
  (target :unsigned-int)
  (internalformat :unsigned-int)
  (width :int)
  (height :int)
  (format :unsigned-int)
  (type :unsigned-int)
  (row :pointer)
  (column :pointer))

(defcfun ("glGetSeparableFilter" glGetSeparableFilter) :void
  (target :unsigned-int)
  (format :unsigned-int)
  (type :unsigned-int)
  (row :pointer)
  (column :pointer)
  (span :pointer))


