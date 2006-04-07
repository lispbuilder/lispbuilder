
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


(defconstant GL_TEXTURE0 #x84C0)

(defconstant GL_TEXTURE1 #x84C1)

(defconstant GL_TEXTURE2 #x84C2)

(defconstant GL_TEXTURE3 #x84C3)

(defconstant GL_TEXTURE4 #x84C4)

(defconstant GL_TEXTURE5 #x84C5)

(defconstant GL_TEXTURE6 #x84C6)

(defconstant GL_TEXTURE7 #x84C7)

(defconstant GL_TEXTURE8 #x84C8)

(defconstant GL_TEXTURE9 #x84C9)

(defconstant GL_TEXTURE10 #x84CA)

(defconstant GL_TEXTURE11 #x84CB)

(defconstant GL_TEXTURE12 #x84CC)

(defconstant GL_TEXTURE13 #x84CD)

(defconstant GL_TEXTURE14 #x84CE)

(defconstant GL_TEXTURE15 #x84CF)

(defconstant GL_TEXTURE16 #x84D0)

(defconstant GL_TEXTURE17 #x84D1)

(defconstant GL_TEXTURE18 #x84D2)

(defconstant GL_TEXTURE19 #x84D3)

(defconstant GL_TEXTURE20 #x84D4)

(defconstant GL_TEXTURE21 #x84D5)

(defconstant GL_TEXTURE22 #x84D6)

(defconstant GL_TEXTURE23 #x84D7)

(defconstant GL_TEXTURE24 #x84D8)

(defconstant GL_TEXTURE25 #x84D9)

(defconstant GL_TEXTURE26 #x84DA)

(defconstant GL_TEXTURE27 #x84DB)

(defconstant GL_TEXTURE28 #x84DC)

(defconstant GL_TEXTURE29 #x84DD)

(defconstant GL_TEXTURE30 #x84DE)

(defconstant GL_TEXTURE31 #x84DF)

(defconstant GL_ACTIVE_TEXTURE #x84E0)

(defconstant GL_CLIENT_ACTIVE_TEXTURE #x84E1)

(defconstant GL_MAX_TEXTURE_UNITS #x84E2)

(defconstant GL_NORMAL_MAP #x8511)

(defconstant GL_REFLECTION_MAP #x8512)

(defconstant GL_TEXTURE_CUBE_MAP #x8513)

(defconstant GL_TEXTURE_BINDING_CUBE_MAP #x8514)

(defconstant GL_TEXTURE_CUBE_MAP_POSITIVE_X #x8515)

(defconstant GL_TEXTURE_CUBE_MAP_NEGATIVE_X #x8516)

(defconstant GL_TEXTURE_CUBE_MAP_POSITIVE_Y #x8517)

(defconstant GL_TEXTURE_CUBE_MAP_NEGATIVE_Y #x8518)

(defconstant GL_TEXTURE_CUBE_MAP_POSITIVE_Z #x8519)

(defconstant GL_TEXTURE_CUBE_MAP_NEGATIVE_Z #x851A)

(defconstant GL_PROXY_TEXTURE_CUBE_MAP #x851B)

(defconstant GL_MAX_CUBE_MAP_TEXTURE_SIZE #x851C)

(defconstant GL_COMPRESSED_ALPHA #x84E9)

(defconstant GL_COMPRESSED_LUMINANCE #x84EA)

(defconstant GL_COMPRESSED_LUMINANCE_ALPHA #x84EB)

(defconstant GL_COMPRESSED_INTENSITY #x84EC)

(defconstant GL_COMPRESSED_RGB #x84ED)

(defconstant GL_COMPRESSED_RGBA #x84EE)

(defconstant GL_TEXTURE_COMPRESSION_HINT #x84EF)

(defconstant GL_TEXTURE_COMPRESSED_IMAGE_SIZE #x86A0)

(defconstant GL_TEXTURE_COMPRESSED #x86A1)

(defconstant GL_NUM_COMPRESSED_TEXTURE_FORMATS #x86A2)

(defconstant GL_COMPRESSED_TEXTURE_FORMATS #x86A3)

(defconstant GL_MULTISAMPLE #x809D)

(defconstant GL_SAMPLE_ALPHA_TO_COVERAGE #x809E)

(defconstant GL_SAMPLE_ALPHA_TO_ONE #x809F)

(defconstant GL_SAMPLE_COVERAGE #x80A0)

(defconstant GL_SAMPLE_BUFFERS #x80A8)

(defconstant GL_SAMPLES #x80A9)

(defconstant GL_SAMPLE_COVERAGE_VALUE #x80AA)

(defconstant GL_SAMPLE_COVERAGE_INVERT #x80AB)

(defconstant GL_MULTISAMPLE_BIT #x20000000)

(defconstant GL_TRANSPOSE_MODELVIEW_MATRIX #x84E3)

(defconstant GL_TRANSPOSE_PROJECTION_MATRIX #x84E4)

(defconstant GL_TRANSPOSE_TEXTURE_MATRIX #x84E5)

(defconstant GL_TRANSPOSE_COLOR_MATRIX #x84E6)

(defconstant GL_COMBINE #x8570)

(defconstant GL_COMBINE_RGB #x8571)

(defconstant GL_COMBINE_ALPHA #x8572)

(defconstant GL_SOURCE0_RGB #x8580)

(defconstant GL_SOURCE1_RGB #x8581)

(defconstant GL_SOURCE2_RGB #x8582)

(defconstant GL_SOURCE0_ALPHA #x8588)

(defconstant GL_SOURCE1_ALPHA #x8589)

(defconstant GL_SOURCE2_ALPHA #x858A)

(defconstant GL_OPERAND0_RGB #x8590)

(defconstant GL_OPERAND1_RGB #x8591)

(defconstant GL_OPERAND2_RGB #x8592)

(defconstant GL_OPERAND0_ALPHA #x8598)

(defconstant GL_OPERAND1_ALPHA #x8599)

(defconstant GL_OPERAND2_ALPHA #x859A)

(defconstant GL_RGB_SCALE #x8573)

(defconstant GL_ADD_SIGNED #x8574)

(defconstant GL_INTERPOLATE #x8575)

(defconstant GL_SUBTRACT #x84E7)

(defconstant GL_CONSTANT #x8576)

(defconstant GL_PRIMARY_COLOR #x8577)

(defconstant GL_PREVIOUS #x8578)

(defconstant GL_DOT3_RGB #x86AE)

(defconstant GL_DOT3_RGBA #x86AF)

(defconstant GL_CLAMP_TO_BORDER #x812D)

(defcfun ("glActiveTexture" glActiveTexture) :void
  (texture :unsigned-int))

(defcfun ("glClientActiveTexture" glClientActiveTexture) :void
  (texture :unsigned-int))

(defcfun ("glCompressedTexImage1D" glCompressedTexImage1D) :void
  (target :unsigned-int)
  (level :int)
  (internalformat :unsigned-int)
  (width :int)
  (border :int)
  (imageSize :int)
  (data :pointer))

(defcfun ("glCompressedTexImage2D" glCompressedTexImage2D) :void
  (target :unsigned-int)
  (level :int)
  (internalformat :unsigned-int)
  (width :int)
  (height :int)
  (border :int)
  (imageSize :int)
  (data :pointer))

(defcfun ("glCompressedTexImage3D" glCompressedTexImage3D) :void
  (target :unsigned-int)
  (level :int)
  (internalformat :unsigned-int)
  (width :int)
  (height :int)
  (depth :int)
  (border :int)
  (imageSize :int)
  (data :pointer))

(defcfun ("glCompressedTexSubImage1D" glCompressedTexSubImage1D) :void
  (target :unsigned-int)
  (level :int)
  (xoffset :int)
  (width :int)
  (format :unsigned-int)
  (imageSize :int)
  (data :pointer))

(defcfun ("glCompressedTexSubImage2D" glCompressedTexSubImage2D) :void
  (target :unsigned-int)
  (level :int)
  (xoffset :int)
  (yoffset :int)
  (width :int)
  (height :int)
  (format :unsigned-int)
  (imageSize :int)
  (data :pointer))

(defcfun ("glCompressedTexSubImage3D" glCompressedTexSubImage3D) :void
  (target :unsigned-int)
  (level :int)
  (xoffset :int)
  (yoffset :int)
  (zoffset :int)
  (width :int)
  (height :int)
  (depth :int)
  (format :unsigned-int)
  (imageSize :int)
  (data :pointer))

(defcfun ("glGetCompressedTexImage" glGetCompressedTexImage) :void
  (target :unsigned-int)
  (lod :int)
  (img :pointer))

(defcfun ("glMultiTexCoord1d" glMultiTexCoord1d) :void
  (target :unsigned-int)
  (s :double))

(defcfun ("glMultiTexCoord1dv" glMultiTexCoord1dv) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord1f" glMultiTexCoord1f) :void
  (target :unsigned-int)
  (s :float))

(defcfun ("glMultiTexCoord1fv" glMultiTexCoord1fv) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord1i" glMultiTexCoord1i) :void
  (target :unsigned-int)
  (s :int))

(defcfun ("glMultiTexCoord1iv" glMultiTexCoord1iv) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord1s" glMultiTexCoord1s) :void
  (target :unsigned-int)
  (s :short))

(defcfun ("glMultiTexCoord1sv" glMultiTexCoord1sv) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord2d" glMultiTexCoord2d) :void
  (target :unsigned-int)
  (s :double)
  (t_arg2 :double))

(defcfun ("glMultiTexCoord2dv" glMultiTexCoord2dv) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord2f" glMultiTexCoord2f) :void
  (target :unsigned-int)
  (s :float)
  (t_arg2 :float))

(defcfun ("glMultiTexCoord2fv" glMultiTexCoord2fv) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord2i" glMultiTexCoord2i) :void
  (target :unsigned-int)
  (s :int)
  (t_arg2 :int))

(defcfun ("glMultiTexCoord2iv" glMultiTexCoord2iv) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord2s" glMultiTexCoord2s) :void
  (target :unsigned-int)
  (s :short)
  (t_arg2 :short))

(defcfun ("glMultiTexCoord2sv" glMultiTexCoord2sv) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord3d" glMultiTexCoord3d) :void
  (target :unsigned-int)
  (s :double)
  (t_arg2 :double)
  (r :double))

(defcfun ("glMultiTexCoord3dv" glMultiTexCoord3dv) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord3f" glMultiTexCoord3f) :void
  (target :unsigned-int)
  (s :float)
  (t_arg2 :float)
  (r :float))

(defcfun ("glMultiTexCoord3fv" glMultiTexCoord3fv) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord3i" glMultiTexCoord3i) :void
  (target :unsigned-int)
  (s :int)
  (t_arg2 :int)
  (r :int))

(defcfun ("glMultiTexCoord3iv" glMultiTexCoord3iv) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord3s" glMultiTexCoord3s) :void
  (target :unsigned-int)
  (s :short)
  (t_arg2 :short)
  (r :short))

(defcfun ("glMultiTexCoord3sv" glMultiTexCoord3sv) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord4d" glMultiTexCoord4d) :void
  (target :unsigned-int)
  (s :double)
  (t_arg2 :double)
  (r :double)
  (q :double))

(defcfun ("glMultiTexCoord4dv" glMultiTexCoord4dv) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord4f" glMultiTexCoord4f) :void
  (target :unsigned-int)
  (s :float)
  (t_arg2 :float)
  (r :float)
  (q :float))

(defcfun ("glMultiTexCoord4fv" glMultiTexCoord4fv) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord4i" glMultiTexCoord4i) :void
  (target :unsigned-int)
  (s :int)
  (t_arg2 :int)
  (r :int)
  (q :int))

(defcfun ("glMultiTexCoord4iv" glMultiTexCoord4iv) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord4s" glMultiTexCoord4s) :void
  (target :unsigned-int)
  (s :short)
  (t_arg2 :short)
  (r :short)
  (q :short))

(defcfun ("glMultiTexCoord4sv" glMultiTexCoord4sv) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glLoadTransposeMatrixd" glLoadTransposeMatrixd) :void
  (m :pointer))

(defcfun ("glLoadTransposeMatrixf" glLoadTransposeMatrixf) :void
  (m :pointer))

(defcfun ("glMultTransposeMatrixd" glMultTransposeMatrixd) :void
  (m :pointer))

(defcfun ("glMultTransposeMatrixf" glMultTransposeMatrixf) :void
  (m :pointer))

(defcfun ("glSampleCoverage" glSampleCoverage) :void
  (value :float)
  (invert :unsigned-char))

(defconstant GL_ARB_multitexture 1)

(defconstant GL_TEXTURE0_ARB #x84C0)

(defconstant GL_TEXTURE1_ARB #x84C1)

(defconstant GL_TEXTURE2_ARB #x84C2)

(defconstant GL_TEXTURE3_ARB #x84C3)

(defconstant GL_TEXTURE4_ARB #x84C4)

(defconstant GL_TEXTURE5_ARB #x84C5)

(defconstant GL_TEXTURE6_ARB #x84C6)

(defconstant GL_TEXTURE7_ARB #x84C7)

(defconstant GL_TEXTURE8_ARB #x84C8)

(defconstant GL_TEXTURE9_ARB #x84C9)

(defconstant GL_TEXTURE10_ARB #x84CA)

(defconstant GL_TEXTURE11_ARB #x84CB)

(defconstant GL_TEXTURE12_ARB #x84CC)

(defconstant GL_TEXTURE13_ARB #x84CD)

(defconstant GL_TEXTURE14_ARB #x84CE)

(defconstant GL_TEXTURE15_ARB #x84CF)

(defconstant GL_TEXTURE16_ARB #x84D0)

(defconstant GL_TEXTURE17_ARB #x84D1)

(defconstant GL_TEXTURE18_ARB #x84D2)

(defconstant GL_TEXTURE19_ARB #x84D3)

(defconstant GL_TEXTURE20_ARB #x84D4)

(defconstant GL_TEXTURE21_ARB #x84D5)

(defconstant GL_TEXTURE22_ARB #x84D6)

(defconstant GL_TEXTURE23_ARB #x84D7)

(defconstant GL_TEXTURE24_ARB #x84D8)

(defconstant GL_TEXTURE25_ARB #x84D9)

(defconstant GL_TEXTURE26_ARB #x84DA)

(defconstant GL_TEXTURE27_ARB #x84DB)

(defconstant GL_TEXTURE28_ARB #x84DC)

(defconstant GL_TEXTURE29_ARB #x84DD)

(defconstant GL_TEXTURE30_ARB #x84DE)

(defconstant GL_TEXTURE31_ARB #x84DF)

(defconstant GL_ACTIVE_TEXTURE_ARB #x84E0)

(defconstant GL_CLIENT_ACTIVE_TEXTURE_ARB #x84E1)

(defconstant GL_MAX_TEXTURE_UNITS_ARB #x84E2)

(defcfun ("glActiveTextureARB" glActiveTextureARB) :void
  (texture :unsigned-int))

(defcfun ("glClientActiveTextureARB" glClientActiveTextureARB) :void
  (texture :unsigned-int))

(defcfun ("glMultiTexCoord1dARB" glMultiTexCoord1dARB) :void
  (target :unsigned-int)
  (s :double))

(defcfun ("glMultiTexCoord1dvARB" glMultiTexCoord1dvARB) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord1fARB" glMultiTexCoord1fARB) :void
  (target :unsigned-int)
  (s :float))

(defcfun ("glMultiTexCoord1fvARB" glMultiTexCoord1fvARB) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord1iARB" glMultiTexCoord1iARB) :void
  (target :unsigned-int)
  (s :int))

(defcfun ("glMultiTexCoord1ivARB" glMultiTexCoord1ivARB) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord1sARB" glMultiTexCoord1sARB) :void
  (target :unsigned-int)
  (s :short))

(defcfun ("glMultiTexCoord1svARB" glMultiTexCoord1svARB) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord2dARB" glMultiTexCoord2dARB) :void
  (target :unsigned-int)
  (s :double)
  (t_arg2 :double))

(defcfun ("glMultiTexCoord2dvARB" glMultiTexCoord2dvARB) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord2fARB" glMultiTexCoord2fARB) :void
  (target :unsigned-int)
  (s :float)
  (t_arg2 :float))

(defcfun ("glMultiTexCoord2fvARB" glMultiTexCoord2fvARB) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord2iARB" glMultiTexCoord2iARB) :void
  (target :unsigned-int)
  (s :int)
  (t_arg2 :int))

(defcfun ("glMultiTexCoord2ivARB" glMultiTexCoord2ivARB) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord2sARB" glMultiTexCoord2sARB) :void
  (target :unsigned-int)
  (s :short)
  (t_arg2 :short))

(defcfun ("glMultiTexCoord2svARB" glMultiTexCoord2svARB) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord3dARB" glMultiTexCoord3dARB) :void
  (target :unsigned-int)
  (s :double)
  (t_arg2 :double)
  (r :double))

(defcfun ("glMultiTexCoord3dvARB" glMultiTexCoord3dvARB) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord3fARB" glMultiTexCoord3fARB) :void
  (target :unsigned-int)
  (s :float)
  (t_arg2 :float)
  (r :float))

(defcfun ("glMultiTexCoord3fvARB" glMultiTexCoord3fvARB) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord3iARB" glMultiTexCoord3iARB) :void
  (target :unsigned-int)
  (s :int)
  (t_arg2 :int)
  (r :int))

(defcfun ("glMultiTexCoord3ivARB" glMultiTexCoord3ivARB) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord3sARB" glMultiTexCoord3sARB) :void
  (target :unsigned-int)
  (s :short)
  (t_arg2 :short)
  (r :short))

(defcfun ("glMultiTexCoord3svARB" glMultiTexCoord3svARB) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord4dARB" glMultiTexCoord4dARB) :void
  (target :unsigned-int)
  (s :double)
  (t_arg2 :double)
  (r :double)
  (q :double))

(defcfun ("glMultiTexCoord4dvARB" glMultiTexCoord4dvARB) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord4fARB" glMultiTexCoord4fARB) :void
  (target :unsigned-int)
  (s :float)
  (t_arg2 :float)
  (r :float)
  (q :float))

(defcfun ("glMultiTexCoord4fvARB" glMultiTexCoord4fvARB) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord4iARB" glMultiTexCoord4iARB) :void
  (target :unsigned-int)
  (s :int)
  (t_arg2 :int)
  (r :int)
  (q :int))

(defcfun ("glMultiTexCoord4ivARB" glMultiTexCoord4ivARB) :void
  (target :unsigned-int)
  (v :pointer))

(defcfun ("glMultiTexCoord4sARB" glMultiTexCoord4sARB) :void
  (target :unsigned-int)
  (s :short)
  (t_arg2 :short)
  (r :short)
  (q :short))

(defcfun ("glMultiTexCoord4svARB" glMultiTexCoord4svARB) :void
  (target :unsigned-int)
  (v :pointer))

(defconstant GL_MESA_trace 1)

(defconstant GL_TRACE_ALL_BITS_MESA #xFFFF)

(defconstant GL_TRACE_OPERATIONS_BIT_MESA #x0001)

(defconstant GL_TRACE_PRIMITIVES_BIT_MESA #x0002)

(defconstant GL_TRACE_ARRAYS_BIT_MESA #x0004)

(defconstant GL_TRACE_TEXTURES_BIT_MESA #x0008)

(defconstant GL_TRACE_PIXELS_BIT_MESA #x0010)

(defconstant GL_TRACE_ERRORS_BIT_MESA #x0020)

(defconstant GL_TRACE_MASK_MESA #x8755)

(defconstant GL_TRACE_NAME_MESA #x8756)

(defcfun ("glEnableTraceMESA" glEnableTraceMESA) :void
  (mask :unsigned-int))

(defcfun ("glDisableTraceMESA" glDisableTraceMESA) :void
  (mask :unsigned-int))

(defcfun ("glNewTraceMESA" glNewTraceMESA) :void
  (mask :unsigned-int)
  (traceName :pointer))

(defcfun ("glEndTraceMESA" glEndTraceMESA) :void)

(defcfun ("glTraceAssertAttribMESA" glTraceAssertAttribMESA) :void
  (attribMask :unsigned-int))

(defcfun ("glTraceCommentMESA" glTraceCommentMESA) :void
  (comment :pointer))

(defcfun ("glTraceTextureMESA" glTraceTextureMESA) :void
  (name :unsigned-int)
  (comment :pointer))

(defcfun ("glTraceListMESA" glTraceListMESA) :void
  (name :unsigned-int)
  (comment :pointer))

(defcfun ("glTracePointerMESA" glTracePointerMESA) :void
  (pointer :pointer)
  (comment :pointer))

(defcfun ("glTracePointerRangeMESA" glTracePointerRangeMESA) :void
  (first :pointer)
  (last :pointer)
  (comment :pointer))

(defconstant GL_MESA_packed_depth_stencil 1)

(defconstant GL_DEPTH_STENCIL_MESA #x8750)

(defconstant GL_UNSIGNED_INT_24_8_MESA #x8751)

(defconstant GL_UNSIGNED_INT_8_24_REV_MESA #x8752)

(defconstant GL_UNSIGNED_SHORT_15_1_MESA #x8753)

(defconstant GL_UNSIGNED_SHORT_1_15_REV_MESA #x8754)

(defconstant GL_MESA_program_debug 1)

(defconstant GL_FRAGMENT_PROGRAM_POSITION_MESA #x8bb0)

(defconstant GL_FRAGMENT_PROGRAM_CALLBACK_MESA #x8bb1)

(defconstant GL_FRAGMENT_PROGRAM_CALLBACK_FUNC_MESA #x8bb2)

(defconstant GL_FRAGMENT_PROGRAM_CALLBACK_DATA_MESA #x8bb3)

(defconstant GL_VERTEX_PROGRAM_POSITION_MESA #x8bb4)

(defconstant GL_VERTEX_PROGRAM_CALLBACK_MESA #x8bb5)

(defconstant GL_VERTEX_PROGRAM_CALLBACK_FUNC_MESA #x8bb6)

(defconstant GL_VERTEX_PROGRAM_CALLBACK_DATA_MESA #x8bb7)

(defcfun ("glProgramCallbackMESA" glProgramCallbackMESA) :void
  (target :unsigned-int)
  (callback :pointer)
  (data :pointer))

(defcfun ("glGetProgramRegisterfvMESA" glGetProgramRegisterfvMESA) :void
  (target :unsigned-int)
  (len :int)
  (name :pointer)
  (v :pointer))

(defconstant GL_ATI_blend_equation_separate 1)

(defconstant GL_ALPHA_BLEND_EQUATION_ATI #x883D)

(defcfun ("glBlendEquationSeparateATI" glBlendEquationSeparateATI) :void
  (modeRGB :unsigned-int)
  (modeA :unsigned-int))


