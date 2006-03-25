
;;;; gl.h Mesa v6.4.2 CFFI lisp wrapper
;;;; Part of the CL-Gardeners project
;;;; http://wiki.alu.org/Application_Builder
;;;; (C)2006 Luke J Crook
;;;; See COPYING for license

;;;; Note: (1) "gl.h" contains a LOT of typedef void (APIENTRYP blah blah blah) and
;;;;           none of these are yet defined.

(in-package #:lispbuilder-opengl)

(defctype GLenum :unsigned-int)
(defctype GLboolean :unsigned-char)
(defctype GLbitfield :unsigned-char)
(defctype GLvoid :void)
(defctype GLbyte :uint8)
(defctype GLshort :short)
(defctype GLint :int)
(defctype GLubyte :unsigned-char)
(defctype GLushort :unsigned-short)
(defctype GLuint :unsigned-int)
(defctype GLsizei :int)
(defctype GLfloat :float)
(defctype GLclampf :float)
(defctype GLdouble :double)
(defctype GLclampd :double)




;;;SWIG wrapper code starts here

(defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(progn ,@(loop for value in enums
                  for index = 0 then (1+ index)
                  when (listp value) do (setf index (second value)
                                              value (first value))
                  collect `(defconstant ,value ,index))))

;;;SWIG wrapper code ends here


(defconstant GL_VERSION_1_1 1)

(defconstant GL_VERSION_1_2 1)

(defconstant GL_VERSION_1_3 1)

(defconstant GL_ARB_imaging 1)

(defconstant GL_FALSE #x0)

(defconstant GL_TRUE #x1)

(defconstant GL_BYTE #x1400)

(defconstant GL_UNSIGNED_BYTE #x1401)

(defconstant GL_SHORT #x1402)

(defconstant GL_UNSIGNED_SHORT #x1403)

(defconstant GL_INT #x1404)

(defconstant GL_UNSIGNED_INT #x1405)

(defconstant GL_FLOAT #x1406)

(defconstant GL_2_BYTES #x1407)

(defconstant GL_3_BYTES #x1408)

(defconstant GL_4_BYTES #x1409)

(defconstant GL_DOUBLE #x140A)

(defconstant GL_POINTS #x0000)

(defconstant GL_LINES #x0001)

(defconstant GL_LINE_LOOP #x0002)

(defconstant GL_LINE_STRIP #x0003)

(defconstant GL_TRIANGLES #x0004)

(defconstant GL_TRIANGLE_STRIP #x0005)

(defconstant GL_TRIANGLE_FAN #x0006)

(defconstant GL_QUADS #x0007)

(defconstant GL_QUAD_STRIP #x0008)

(defconstant GL_POLYGON #x0009)

(defconstant GL_VERTEX_ARRAY #x8074)

(defconstant GL_NORMAL_ARRAY #x8075)

(defconstant GL_COLOR_ARRAY #x8076)

(defconstant GL_INDEX_ARRAY #x8077)

(defconstant GL_TEXTURE_COORD_ARRAY #x8078)

(defconstant GL_EDGE_FLAG_ARRAY #x8079)

(defconstant GL_VERTEX_ARRAY_SIZE #x807A)

(defconstant GL_VERTEX_ARRAY_TYPE #x807B)

(defconstant GL_VERTEX_ARRAY_STRIDE #x807C)

(defconstant GL_NORMAL_ARRAY_TYPE #x807E)

(defconstant GL_NORMAL_ARRAY_STRIDE #x807F)

(defconstant GL_COLOR_ARRAY_SIZE #x8081)

(defconstant GL_COLOR_ARRAY_TYPE #x8082)

(defconstant GL_COLOR_ARRAY_STRIDE #x8083)

(defconstant GL_INDEX_ARRAY_TYPE #x8085)

(defconstant GL_INDEX_ARRAY_STRIDE #x8086)

(defconstant GL_TEXTURE_COORD_ARRAY_SIZE #x8088)

(defconstant GL_TEXTURE_COORD_ARRAY_TYPE #x8089)

(defconstant GL_TEXTURE_COORD_ARRAY_STRIDE #x808A)

(defconstant GL_EDGE_FLAG_ARRAY_STRIDE #x808C)

(defconstant GL_VERTEX_ARRAY_POINTER #x808E)

(defconstant GL_NORMAL_ARRAY_POINTER #x808F)

(defconstant GL_COLOR_ARRAY_POINTER #x8090)

(defconstant GL_INDEX_ARRAY_POINTER #x8091)

(defconstant GL_TEXTURE_COORD_ARRAY_POINTER #x8092)

(defconstant GL_EDGE_FLAG_ARRAY_POINTER #x8093)

(defconstant GL_V2F #x2A20)

(defconstant GL_V3F #x2A21)

(defconstant GL_C4UB_V2F #x2A22)

(defconstant GL_C4UB_V3F #x2A23)

(defconstant GL_C3F_V3F #x2A24)

(defconstant GL_N3F_V3F #x2A25)

(defconstant GL_C4F_N3F_V3F #x2A26)

(defconstant GL_T2F_V3F #x2A27)

(defconstant GL_T4F_V4F #x2A28)

(defconstant GL_T2F_C4UB_V3F #x2A29)

(defconstant GL_T2F_C3F_V3F #x2A2A)

(defconstant GL_T2F_N3F_V3F #x2A2B)

(defconstant GL_T2F_C4F_N3F_V3F #x2A2C)

(defconstant GL_T4F_C4F_N3F_V4F #x2A2D)

(defconstant GL_MATRIX_MODE #x0BA0)

(defconstant GL_MODELVIEW #x1700)

(defconstant GL_PROJECTION #x1701)

(defconstant GL_TEXTURE #x1702)

(defconstant GL_POINT_SMOOTH #x0B10)

(defconstant GL_POINT_SIZE #x0B11)

(defconstant GL_POINT_SIZE_GRANULARITY #x0B13)

(defconstant GL_POINT_SIZE_RANGE #x0B12)

(defconstant GL_LINE_SMOOTH #x0B20)

(defconstant GL_LINE_STIPPLE #x0B24)

(defconstant GL_LINE_STIPPLE_PATTERN #x0B25)

(defconstant GL_LINE_STIPPLE_REPEAT #x0B26)

(defconstant GL_LINE_WIDTH #x0B21)

(defconstant GL_LINE_WIDTH_GRANULARITY #x0B23)

(defconstant GL_LINE_WIDTH_RANGE #x0B22)

(defconstant GL_POINT #x1B00)

(defconstant GL_LINE #x1B01)

(defconstant GL_FILL #x1B02)

(defconstant GL_CW #x0900)

(defconstant GL_CCW #x0901)

(defconstant GL_FRONT #x0404)

(defconstant GL_BACK #x0405)

(defconstant GL_POLYGON_MODE #x0B40)

(defconstant GL_POLYGON_SMOOTH #x0B41)

(defconstant GL_POLYGON_STIPPLE #x0B42)

(defconstant GL_EDGE_FLAG #x0B43)

(defconstant GL_CULL_FACE #x0B44)

(defconstant GL_CULL_FACE_MODE #x0B45)

(defconstant GL_FRONT_FACE #x0B46)

(defconstant GL_POLYGON_OFFSET_FACTOR #x8038)

(defconstant GL_POLYGON_OFFSET_UNITS #x2A00)

(defconstant GL_POLYGON_OFFSET_POINT #x2A01)

(defconstant GL_POLYGON_OFFSET_LINE #x2A02)

(defconstant GL_POLYGON_OFFSET_FILL #x8037)

(defconstant GL_COMPILE #x1300)

(defconstant GL_COMPILE_AND_EXECUTE #x1301)

(defconstant GL_LIST_BASE #x0B32)

(defconstant GL_LIST_INDEX #x0B33)

(defconstant GL_LIST_MODE #x0B30)

(defconstant GL_NEVER #x0200)

(defconstant GL_LESS #x0201)

(defconstant GL_EQUAL #x0202)

(defconstant GL_LEQUAL #x0203)

(defconstant GL_GREATER #x0204)

(defconstant GL_NOTEQUAL #x0205)

(defconstant GL_GEQUAL #x0206)

(defconstant GL_ALWAYS #x0207)

(defconstant GL_DEPTH_TEST #x0B71)

(defconstant GL_DEPTH_BITS #x0D56)

(defconstant GL_DEPTH_CLEAR_VALUE #x0B73)

(defconstant GL_DEPTH_FUNC #x0B74)

(defconstant GL_DEPTH_RANGE #x0B70)

(defconstant GL_DEPTH_WRITEMASK #x0B72)

(defconstant GL_DEPTH_COMPONENT #x1902)

(defconstant GL_LIGHTING #x0B50)

(defconstant GL_LIGHT0 #x4000)

(defconstant GL_LIGHT1 #x4001)

(defconstant GL_LIGHT2 #x4002)

(defconstant GL_LIGHT3 #x4003)

(defconstant GL_LIGHT4 #x4004)

(defconstant GL_LIGHT5 #x4005)

(defconstant GL_LIGHT6 #x4006)

(defconstant GL_LIGHT7 #x4007)

(defconstant GL_SPOT_EXPONENT #x1205)

(defconstant GL_SPOT_CUTOFF #x1206)

(defconstant GL_CONSTANT_ATTENUATION #x1207)

(defconstant GL_LINEAR_ATTENUATION #x1208)

(defconstant GL_QUADRATIC_ATTENUATION #x1209)

(defconstant GL_AMBIENT #x1200)

(defconstant GL_DIFFUSE #x1201)

(defconstant GL_SPECULAR #x1202)

(defconstant GL_SHININESS #x1601)

(defconstant GL_EMISSION #x1600)

(defconstant GL_POSITION #x1203)

(defconstant GL_SPOT_DIRECTION #x1204)

(defconstant GL_AMBIENT_AND_DIFFUSE #x1602)

(defconstant GL_COLOR_INDEXES #x1603)

(defconstant GL_LIGHT_MODEL_TWO_SIDE #x0B52)

(defconstant GL_LIGHT_MODEL_LOCAL_VIEWER #x0B51)

(defconstant GL_LIGHT_MODEL_AMBIENT #x0B53)

(defconstant GL_FRONT_AND_BACK #x0408)

(defconstant GL_SHADE_MODEL #x0B54)

(defconstant GL_FLAT #x1D00)

(defconstant GL_SMOOTH #x1D01)

(defconstant GL_COLOR_MATERIAL #x0B57)

(defconstant GL_COLOR_MATERIAL_FACE #x0B55)

(defconstant GL_COLOR_MATERIAL_PARAMETER #x0B56)

(defconstant GL_NORMALIZE #x0BA1)

(defconstant GL_CLIP_PLANE0 #x3000)

(defconstant GL_CLIP_PLANE1 #x3001)

(defconstant GL_CLIP_PLANE2 #x3002)

(defconstant GL_CLIP_PLANE3 #x3003)

(defconstant GL_CLIP_PLANE4 #x3004)

(defconstant GL_CLIP_PLANE5 #x3005)

(defconstant GL_ACCUM_RED_BITS #x0D58)

(defconstant GL_ACCUM_GREEN_BITS #x0D59)

(defconstant GL_ACCUM_BLUE_BITS #x0D5A)

(defconstant GL_ACCUM_ALPHA_BITS #x0D5B)

(defconstant GL_ACCUM_CLEAR_VALUE #x0B80)

(defconstant GL_ACCUM #x0100)

(defconstant GL_ADD #x0104)

(defconstant GL_LOAD #x0101)

(defconstant GL_MULT #x0103)

(defconstant GL_RETURN #x0102)

(defconstant GL_ALPHA_TEST #x0BC0)

(defconstant GL_ALPHA_TEST_REF #x0BC2)

(defconstant GL_ALPHA_TEST_FUNC #x0BC1)

(defconstant GL_BLEND #x0BE2)

(defconstant GL_BLEND_SRC #x0BE1)

(defconstant GL_BLEND_DST #x0BE0)

(defconstant GL_ZERO #x0)

(defconstant GL_ONE #x1)

(defconstant GL_SRC_COLOR #x0300)

(defconstant GL_ONE_MINUS_SRC_COLOR #x0301)

(defconstant GL_SRC_ALPHA #x0302)

(defconstant GL_ONE_MINUS_SRC_ALPHA #x0303)

(defconstant GL_DST_ALPHA #x0304)

(defconstant GL_ONE_MINUS_DST_ALPHA #x0305)

(defconstant GL_DST_COLOR #x0306)

(defconstant GL_ONE_MINUS_DST_COLOR #x0307)

(defconstant GL_SRC_ALPHA_SATURATE #x0308)

(defconstant GL_FEEDBACK #x1C01)

(defconstant GL_RENDER #x1C00)

(defconstant GL_SELECT #x1C02)

(defconstant GL_2D #x0600)

(defconstant GL_3D #x0601)

(defconstant GL_3D_COLOR #x0602)

(defconstant GL_3D_COLOR_TEXTURE #x0603)

(defconstant GL_4D_COLOR_TEXTURE #x0604)

(defconstant GL_POINT_TOKEN #x0701)

(defconstant GL_LINE_TOKEN #x0702)

(defconstant GL_LINE_RESET_TOKEN #x0707)

(defconstant GL_POLYGON_TOKEN #x0703)

(defconstant GL_BITMAP_TOKEN #x0704)

(defconstant GL_DRAW_PIXEL_TOKEN #x0705)

(defconstant GL_COPY_PIXEL_TOKEN #x0706)

(defconstant GL_PASS_THROUGH_TOKEN #x0700)

(defconstant GL_FEEDBACK_BUFFER_POINTER #x0DF0)

(defconstant GL_FEEDBACK_BUFFER_SIZE #x0DF1)

(defconstant GL_FEEDBACK_BUFFER_TYPE #x0DF2)

(defconstant GL_SELECTION_BUFFER_POINTER #x0DF3)

(defconstant GL_SELECTION_BUFFER_SIZE #x0DF4)

(defconstant GL_FOG #x0B60)

(defconstant GL_FOG_MODE #x0B65)

(defconstant GL_FOG_DENSITY #x0B62)

(defconstant GL_FOG_COLOR #x0B66)

(defconstant GL_FOG_INDEX #x0B61)

(defconstant GL_FOG_START #x0B63)

(defconstant GL_FOG_END #x0B64)

(defconstant GL_LINEAR #x2601)

(defconstant GL_EXP #x0800)

(defconstant GL_EXP2 #x0801)

(defconstant GL_LOGIC_OP #x0BF1)

(defconstant GL_INDEX_LOGIC_OP #x0BF1)

(defconstant GL_COLOR_LOGIC_OP #x0BF2)

(defconstant GL_LOGIC_OP_MODE #x0BF0)

(defconstant GL_CLEAR #x1500)

(defconstant GL_SET #x150F)

(defconstant GL_COPY #x1503)

(defconstant GL_COPY_INVERTED #x150C)

(defconstant GL_NOOP #x1505)

(defconstant GL_INVERT #x150A)

(defconstant GL_AND #x1501)

(defconstant GL_NAND #x150E)

(defconstant GL_OR #x1507)

(defconstant GL_NOR #x1508)

(defconstant GL_XOR #x1506)

(defconstant GL_EQUIV #x1509)

(defconstant GL_AND_REVERSE #x1502)

(defconstant GL_AND_INVERTED #x1504)

(defconstant GL_OR_REVERSE #x150B)

(defconstant GL_OR_INVERTED #x150D)

(defconstant GL_STENCIL_TEST #x0B90)

(defconstant GL_STENCIL_WRITEMASK #x0B98)

(defconstant GL_STENCIL_BITS #x0D57)

(defconstant GL_STENCIL_FUNC #x0B92)

(defconstant GL_STENCIL_VALUE_MASK #x0B93)

(defconstant GL_STENCIL_REF #x0B97)

(defconstant GL_STENCIL_FAIL #x0B94)

(defconstant GL_STENCIL_PASS_DEPTH_PASS #x0B96)

(defconstant GL_STENCIL_PASS_DEPTH_FAIL #x0B95)

(defconstant GL_STENCIL_CLEAR_VALUE #x0B91)

(defconstant GL_STENCIL_INDEX #x1901)

(defconstant GL_KEEP #x1E00)

(defconstant GL_REPLACE #x1E01)

(defconstant GL_INCR #x1E02)

(defconstant GL_DECR #x1E03)

(defconstant GL_NONE #x0)

(defconstant GL_LEFT #x0406)

(defconstant GL_RIGHT #x0407)

(defconstant GL_FRONT_LEFT #x0400)

(defconstant GL_FRONT_RIGHT #x0401)

(defconstant GL_BACK_LEFT #x0402)

(defconstant GL_BACK_RIGHT #x0403)

(defconstant GL_AUX0 #x0409)

(defconstant GL_AUX1 #x040A)

(defconstant GL_AUX2 #x040B)

(defconstant GL_AUX3 #x040C)

(defconstant GL_COLOR_INDEX #x1900)

(defconstant GL_RED #x1903)

(defconstant GL_GREEN #x1904)

(defconstant GL_BLUE #x1905)

(defconstant GL_ALPHA #x1906)

(defconstant GL_LUMINANCE #x1909)

(defconstant GL_LUMINANCE_ALPHA #x190A)

(defconstant GL_ALPHA_BITS #x0D55)

(defconstant GL_RED_BITS #x0D52)

(defconstant GL_GREEN_BITS #x0D53)

(defconstant GL_BLUE_BITS #x0D54)

(defconstant GL_INDEX_BITS #x0D51)

(defconstant GL_SUBPIXEL_BITS #x0D50)

(defconstant GL_AUX_BUFFERS #x0C00)

(defconstant GL_READ_BUFFER #x0C02)

(defconstant GL_DRAW_BUFFER #x0C01)

(defconstant GL_DOUBLEBUFFER #x0C32)

(defconstant GL_STEREO #x0C33)

(defconstant GL_BITMAP #x1A00)

(defconstant GL_COLOR #x1800)

(defconstant GL_DEPTH #x1801)

(defconstant GL_STENCIL #x1802)

(defconstant GL_DITHER #x0BD0)

(defconstant GL_RGB #x1907)

(defconstant GL_RGBA #x1908)

(defconstant GL_MAX_LIST_NESTING #x0B31)

(defconstant GL_MAX_ATTRIB_STACK_DEPTH #x0D35)

(defconstant GL_MAX_MODELVIEW_STACK_DEPTH #x0D36)

(defconstant GL_MAX_NAME_STACK_DEPTH #x0D37)

(defconstant GL_MAX_PROJECTION_STACK_DEPTH #x0D38)

(defconstant GL_MAX_TEXTURE_STACK_DEPTH #x0D39)

(defconstant GL_MAX_EVAL_ORDER #x0D30)

(defconstant GL_MAX_LIGHTS #x0D31)

(defconstant GL_MAX_CLIP_PLANES #x0D32)

(defconstant GL_MAX_TEXTURE_SIZE #x0D33)

(defconstant GL_MAX_PIXEL_MAP_TABLE #x0D34)

(defconstant GL_MAX_VIEWPORT_DIMS #x0D3A)

(defconstant GL_MAX_CLIENT_ATTRIB_STACK_DEPTH #x0D3B)

(defconstant GL_ATTRIB_STACK_DEPTH #x0BB0)

(defconstant GL_CLIENT_ATTRIB_STACK_DEPTH #x0BB1)

(defconstant GL_COLOR_CLEAR_VALUE #x0C22)

(defconstant GL_COLOR_WRITEMASK #x0C23)

(defconstant GL_CURRENT_INDEX #x0B01)

(defconstant GL_CURRENT_COLOR #x0B00)

(defconstant GL_CURRENT_NORMAL #x0B02)

(defconstant GL_CURRENT_RASTER_COLOR #x0B04)

(defconstant GL_CURRENT_RASTER_DISTANCE #x0B09)

(defconstant GL_CURRENT_RASTER_INDEX #x0B05)

(defconstant GL_CURRENT_RASTER_POSITION #x0B07)

(defconstant GL_CURRENT_RASTER_TEXTURE_COORDS #x0B06)

(defconstant GL_CURRENT_RASTER_POSITION_VALID #x0B08)

(defconstant GL_CURRENT_TEXTURE_COORDS #x0B03)

(defconstant GL_INDEX_CLEAR_VALUE #x0C20)

(defconstant GL_INDEX_MODE #x0C30)

(defconstant GL_INDEX_WRITEMASK #x0C21)

(defconstant GL_MODELVIEW_MATRIX #x0BA6)

(defconstant GL_MODELVIEW_STACK_DEPTH #x0BA3)

(defconstant GL_NAME_STACK_DEPTH #x0D70)

(defconstant GL_PROJECTION_MATRIX #x0BA7)

(defconstant GL_PROJECTION_STACK_DEPTH #x0BA4)

(defconstant GL_RENDER_MODE #x0C40)

(defconstant GL_RGBA_MODE #x0C31)

(defconstant GL_TEXTURE_MATRIX #x0BA8)

(defconstant GL_TEXTURE_STACK_DEPTH #x0BA5)

(defconstant GL_VIEWPORT #x0BA2)

(defconstant GL_AUTO_NORMAL #x0D80)

(defconstant GL_MAP1_COLOR_4 #x0D90)

(defconstant GL_MAP1_INDEX #x0D91)

(defconstant GL_MAP1_NORMAL #x0D92)

(defconstant GL_MAP1_TEXTURE_COORD_1 #x0D93)

(defconstant GL_MAP1_TEXTURE_COORD_2 #x0D94)

(defconstant GL_MAP1_TEXTURE_COORD_3 #x0D95)

(defconstant GL_MAP1_TEXTURE_COORD_4 #x0D96)

(defconstant GL_MAP1_VERTEX_3 #x0D97)

(defconstant GL_MAP1_VERTEX_4 #x0D98)

(defconstant GL_MAP2_COLOR_4 #x0DB0)

(defconstant GL_MAP2_INDEX #x0DB1)

(defconstant GL_MAP2_NORMAL #x0DB2)

(defconstant GL_MAP2_TEXTURE_COORD_1 #x0DB3)

(defconstant GL_MAP2_TEXTURE_COORD_2 #x0DB4)

(defconstant GL_MAP2_TEXTURE_COORD_3 #x0DB5)

(defconstant GL_MAP2_TEXTURE_COORD_4 #x0DB6)

(defconstant GL_MAP2_VERTEX_3 #x0DB7)

(defconstant GL_MAP2_VERTEX_4 #x0DB8)

(defconstant GL_MAP1_GRID_DOMAIN #x0DD0)

(defconstant GL_MAP1_GRID_SEGMENTS #x0DD1)

(defconstant GL_MAP2_GRID_DOMAIN #x0DD2)

(defconstant GL_MAP2_GRID_SEGMENTS #x0DD3)

(defconstant GL_COEFF #x0A00)

(defconstant GL_DOMAIN #x0A02)

(defconstant GL_ORDER #x0A01)

(defconstant GL_FOG_HINT #x0C54)

(defconstant GL_LINE_SMOOTH_HINT #x0C52)

(defconstant GL_PERSPECTIVE_CORRECTION_HINT #x0C50)

(defconstant GL_POINT_SMOOTH_HINT #x0C51)

(defconstant GL_POLYGON_SMOOTH_HINT #x0C53)

(defconstant GL_DONT_CARE #x1100)

(defconstant GL_FASTEST #x1101)

(defconstant GL_NICEST #x1102)

(defconstant GL_SCISSOR_TEST #x0C11)

(defconstant GL_SCISSOR_BOX #x0C10)

(defconstant GL_MAP_COLOR #x0D10)

(defconstant GL_MAP_STENCIL #x0D11)

(defconstant GL_INDEX_SHIFT #x0D12)

(defconstant GL_INDEX_OFFSET #x0D13)

(defconstant GL_RED_SCALE #x0D14)

(defconstant GL_RED_BIAS #x0D15)

(defconstant GL_GREEN_SCALE #x0D18)

(defconstant GL_GREEN_BIAS #x0D19)

(defconstant GL_BLUE_SCALE #x0D1A)

(defconstant GL_BLUE_BIAS #x0D1B)

(defconstant GL_ALPHA_SCALE #x0D1C)

(defconstant GL_ALPHA_BIAS #x0D1D)

(defconstant GL_DEPTH_SCALE #x0D1E)

(defconstant GL_DEPTH_BIAS #x0D1F)

(defconstant GL_PIXEL_MAP_S_TO_S_SIZE #x0CB1)

(defconstant GL_PIXEL_MAP_I_TO_I_SIZE #x0CB0)

(defconstant GL_PIXEL_MAP_I_TO_R_SIZE #x0CB2)

(defconstant GL_PIXEL_MAP_I_TO_G_SIZE #x0CB3)

(defconstant GL_PIXEL_MAP_I_TO_B_SIZE #x0CB4)

(defconstant GL_PIXEL_MAP_I_TO_A_SIZE #x0CB5)

(defconstant GL_PIXEL_MAP_R_TO_R_SIZE #x0CB6)

(defconstant GL_PIXEL_MAP_G_TO_G_SIZE #x0CB7)

(defconstant GL_PIXEL_MAP_B_TO_B_SIZE #x0CB8)

(defconstant GL_PIXEL_MAP_A_TO_A_SIZE #x0CB9)

(defconstant GL_PIXEL_MAP_S_TO_S #x0C71)

(defconstant GL_PIXEL_MAP_I_TO_I #x0C70)

(defconstant GL_PIXEL_MAP_I_TO_R #x0C72)

(defconstant GL_PIXEL_MAP_I_TO_G #x0C73)

(defconstant GL_PIXEL_MAP_I_TO_B #x0C74)

(defconstant GL_PIXEL_MAP_I_TO_A #x0C75)

(defconstant GL_PIXEL_MAP_R_TO_R #x0C76)

(defconstant GL_PIXEL_MAP_G_TO_G #x0C77)

(defconstant GL_PIXEL_MAP_B_TO_B #x0C78)

(defconstant GL_PIXEL_MAP_A_TO_A #x0C79)

(defconstant GL_PACK_ALIGNMENT #x0D05)

(defconstant GL_PACK_LSB_FIRST #x0D01)

(defconstant GL_PACK_ROW_LENGTH #x0D02)

(defconstant GL_PACK_SKIP_PIXELS #x0D04)

(defconstant GL_PACK_SKIP_ROWS #x0D03)

(defconstant GL_PACK_SWAP_BYTES #x0D00)

(defconstant GL_UNPACK_ALIGNMENT #x0CF5)

(defconstant GL_UNPACK_LSB_FIRST #x0CF1)

(defconstant GL_UNPACK_ROW_LENGTH #x0CF2)

(defconstant GL_UNPACK_SKIP_PIXELS #x0CF4)

(defconstant GL_UNPACK_SKIP_ROWS #x0CF3)

(defconstant GL_UNPACK_SWAP_BYTES #x0CF0)

(defconstant GL_ZOOM_X #x0D16)

(defconstant GL_ZOOM_Y #x0D17)

(defconstant GL_TEXTURE_ENV #x2300)

(defconstant GL_TEXTURE_ENV_MODE #x2200)

(defconstant GL_TEXTURE_1D #x0DE0)

(defconstant GL_TEXTURE_2D #x0DE1)

(defconstant GL_TEXTURE_WRAP_S #x2802)

(defconstant GL_TEXTURE_WRAP_T #x2803)

(defconstant GL_TEXTURE_MAG_FILTER #x2800)

(defconstant GL_TEXTURE_MIN_FILTER #x2801)

(defconstant GL_TEXTURE_ENV_COLOR #x2201)

(defconstant GL_TEXTURE_GEN_S #x0C60)

(defconstant GL_TEXTURE_GEN_T #x0C61)

(defconstant GL_TEXTURE_GEN_MODE #x2500)

(defconstant GL_TEXTURE_BORDER_COLOR #x1004)

(defconstant GL_TEXTURE_WIDTH #x1000)

(defconstant GL_TEXTURE_HEIGHT #x1001)

(defconstant GL_TEXTURE_BORDER #x1005)

(defconstant GL_TEXTURE_COMPONENTS #x1003)

(defconstant GL_TEXTURE_RED_SIZE #x805C)

(defconstant GL_TEXTURE_GREEN_SIZE #x805D)

(defconstant GL_TEXTURE_BLUE_SIZE #x805E)

(defconstant GL_TEXTURE_ALPHA_SIZE #x805F)

(defconstant GL_TEXTURE_LUMINANCE_SIZE #x8060)

(defconstant GL_TEXTURE_INTENSITY_SIZE #x8061)

(defconstant GL_NEAREST_MIPMAP_NEAREST #x2700)

(defconstant GL_NEAREST_MIPMAP_LINEAR #x2702)

(defconstant GL_LINEAR_MIPMAP_NEAREST #x2701)

(defconstant GL_LINEAR_MIPMAP_LINEAR #x2703)

(defconstant GL_OBJECT_LINEAR #x2401)

(defconstant GL_OBJECT_PLANE #x2501)

(defconstant GL_EYE_LINEAR #x2400)

(defconstant GL_EYE_PLANE #x2502)

(defconstant GL_SPHERE_MAP #x2402)

(defconstant GL_DECAL #x2101)

(defconstant GL_MODULATE #x2100)

(defconstant GL_NEAREST #x2600)

(defconstant GL_REPEAT #x2901)

(defconstant GL_CLAMP #x2900)

(defconstant GL_S #x2000)

(defconstant GL_T #x2001)

(defconstant GL_R #x2002)

(defconstant GL_Q #x2003)

(defconstant GL_TEXTURE_GEN_R #x0C62)

(defconstant GL_TEXTURE_GEN_Q #x0C63)

(defconstant GL_VENDOR #x1F00)

(defconstant GL_RENDERER #x1F01)

(defconstant GL_VERSION #x1F02)

(defconstant GL_EXTENSIONS #x1F03)

(defconstant GL_NO_ERROR #x0)

(defconstant GL_INVALID_VALUE #x0501)

(defconstant GL_INVALID_ENUM #x0500)

(defconstant GL_INVALID_OPERATION #x0502)

(defconstant GL_STACK_OVERFLOW #x0503)

(defconstant GL_STACK_UNDERFLOW #x0504)

(defconstant GL_OUT_OF_MEMORY #x0505)

(defconstant GL_CURRENT_BIT #x00000001)

(defconstant GL_POINT_BIT #x00000002)

(defconstant GL_LINE_BIT #x00000004)

(defconstant GL_POLYGON_BIT #x00000008)

(defconstant GL_POLYGON_STIPPLE_BIT #x00000010)

(defconstant GL_PIXEL_MODE_BIT #x00000020)

(defconstant GL_LIGHTING_BIT #x00000040)

(defconstant GL_FOG_BIT #x00000080)

(defconstant GL_DEPTH_BUFFER_BIT #x00000100)

(defconstant GL_ACCUM_BUFFER_BIT #x00000200)

(defconstant GL_STENCIL_BUFFER_BIT #x00000400)

(defconstant GL_VIEWPORT_BIT #x00000800)

(defconstant GL_TRANSFORM_BIT #x00001000)

(defconstant GL_ENABLE_BIT #x00002000)

(defconstant GL_COLOR_BUFFER_BIT #x00004000)

(defconstant GL_HINT_BIT #x00008000)

(defconstant GL_EVAL_BIT #x00010000)

(defconstant GL_LIST_BIT #x00020000)

(defconstant GL_TEXTURE_BIT #x00040000)

(defconstant GL_SCISSOR_BIT #x00080000)

(defconstant GL_ALL_ATTRIB_BITS #x000FFFFF)

(defconstant GL_PROXY_TEXTURE_1D #x8063)

(defconstant GL_PROXY_TEXTURE_2D #x8064)

(defconstant GL_TEXTURE_PRIORITY #x8066)

(defconstant GL_TEXTURE_RESIDENT #x8067)

(defconstant GL_TEXTURE_BINDING_1D #x8068)

(defconstant GL_TEXTURE_BINDING_2D #x8069)

(defconstant GL_TEXTURE_INTERNAL_FORMAT #x1003)

(defconstant GL_ALPHA4 #x803B)

(defconstant GL_ALPHA8 #x803C)

(defconstant GL_ALPHA12 #x803D)

(defconstant GL_ALPHA16 #x803E)

(defconstant GL_LUMINANCE4 #x803F)

(defconstant GL_LUMINANCE8 #x8040)

(defconstant GL_LUMINANCE12 #x8041)

(defconstant GL_LUMINANCE16 #x8042)

(defconstant GL_LUMINANCE4_ALPHA4 #x8043)

(defconstant GL_LUMINANCE6_ALPHA2 #x8044)

(defconstant GL_LUMINANCE8_ALPHA8 #x8045)

(defconstant GL_LUMINANCE12_ALPHA4 #x8046)

(defconstant GL_LUMINANCE12_ALPHA12 #x8047)

(defconstant GL_LUMINANCE16_ALPHA16 #x8048)

(defconstant GL_INTENSITY #x8049)

(defconstant GL_INTENSITY4 #x804A)

(defconstant GL_INTENSITY8 #x804B)

(defconstant GL_INTENSITY12 #x804C)

(defconstant GL_INTENSITY16 #x804D)

(defconstant GL_R3_G3_B2 #x2A10)

(defconstant GL_RGB4 #x804F)

(defconstant GL_RGB5 #x8050)

(defconstant GL_RGB8 #x8051)

(defconstant GL_RGB10 #x8052)

(defconstant GL_RGB12 #x8053)

(defconstant GL_RGB16 #x8054)

(defconstant GL_RGBA2 #x8055)

(defconstant GL_RGBA4 #x8056)

(defconstant GL_RGB5_A1 #x8057)

(defconstant GL_RGBA8 #x8058)

(defconstant GL_RGB10_A2 #x8059)

(defconstant GL_RGBA12 #x805A)

(defconstant GL_RGBA16 #x805B)

(defconstant GL_CLIENT_PIXEL_STORE_BIT #x00000001)

(defconstant GL_CLIENT_VERTEX_ARRAY_BIT #x00000002)

(defconstant GL_ALL_CLIENT_ATTRIB_BITS #xFFFFFFFF)

(defconstant GL_CLIENT_ALL_ATTRIB_BITS #xFFFFFFFF)

(defcfun ("glClearIndex" glClearIndex) :void
  (c :float))

(defcfun ("glClearColor" glClearColor) :void
  (red :float)
  (green :float)
  (blue :float)
  (alpha :float))

(defcfun ("glClear" glClear) :void
  (mask :unsigned-int))

(defcfun ("glIndexMask" glIndexMask) :void
  (mask :unsigned-int))

(defcfun ("glColorMask" glColorMask) :void
  (red :unsigned-char)
  (green :unsigned-char)
  (blue :unsigned-char)
  (alpha :unsigned-char))

(defcfun ("glAlphaFunc" glAlphaFunc) :void
  (func :unsigned-int)
  (ref :float))

(defcfun ("glBlendFunc" glBlendFunc) :void
  (sfactor :unsigned-int)
  (dfactor :unsigned-int))

(defcfun ("glLogicOp" glLogicOp) :void
  (opcode :unsigned-int))

(defcfun ("glCullFace" glCullFace) :void
  (mode :unsigned-int))

(defcfun ("glFrontFace" glFrontFace) :void
  (mode :unsigned-int))

(defcfun ("glPointSize" glPointSize) :void
  (size :float))

(defcfun ("glLineWidth" glLineWidth) :void
  (width :float))

(defcfun ("glLineStipple" glLineStipple) :void
  (factor :int)
  (pattern :unsigned-short))

(defcfun ("glPolygonMode" glPolygonMode) :void
  (face :unsigned-int)
  (mode :unsigned-int))

(defcfun ("glPolygonOffset" glPolygonOffset) :void
  (factor :float)
  (units :float))

(defcfun ("glPolygonStipple" glPolygonStipple) :void
  (mask :pointer))

(defcfun ("glGetPolygonStipple" glGetPolygonStipple) :void
  (mask :pointer))

(defcfun ("glEdgeFlag" glEdgeFlag) :void
  (flag :unsigned-char))

(defcfun ("glEdgeFlagv" glEdgeFlagv) :void
  (flag :pointer))

(defcfun ("glScissor" glScissor) :void
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defcfun ("glClipPlane" glClipPlane) :void
  (plane :unsigned-int)
  (equation :pointer))

(defcfun ("glGetClipPlane" glGetClipPlane) :void
  (plane :unsigned-int)
  (equation :pointer))

(defcfun ("glDrawBuffer" glDrawBuffer) :void
  (mode :unsigned-int))

(defcfun ("glReadBuffer" glReadBuffer) :void
  (mode :unsigned-int))

(defcfun ("glEnable" glEnable) :void
  (cap :unsigned-int))

(defcfun ("glDisable" glDisable) :void
  (cap :unsigned-int))

(defcfun ("glIsEnabled" glIsEnabled) :unsigned-char
  (cap :unsigned-int))

(defcfun ("glEnableClientState" glEnableClientState) :void
  (cap :unsigned-int))

(defcfun ("glDisableClientState" glDisableClientState) :void
  (cap :unsigned-int))

(defcfun ("glGetBooleanv" glGetBooleanv) :void
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glGetDoublev" glGetDoublev) :void
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glGetFloatv" glGetFloatv) :void
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glGetIntegerv" glGetIntegerv) :void
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glPushAttrib" glPushAttrib) :void
  (mask :unsigned-int))

(defcfun ("glPopAttrib" glPopAttrib) :void)

(defcfun ("glPushClientAttrib" glPushClientAttrib) :void
  (mask :unsigned-int))

(defcfun ("glPopClientAttrib" glPopClientAttrib) :void)

(defcfun ("glRenderMode" glRenderMode) :int
  (mode :unsigned-int))

(defcfun ("glGetError" glGetError) :unsigned-int)

(defcfun ("glGetString" glGetString) :pointer
  (name :unsigned-int))

(defcfun ("glFinish" glFinish) :void)

(defcfun ("glFlush" glFlush) :void)

(defcfun ("glHint" glHint) :void
  (target :unsigned-int)
  (mode :unsigned-int))

(defcfun ("glClearDepth" glClearDepth) :void
  (depth :double))

(defcfun ("glDepthFunc" glDepthFunc) :void
  (func :unsigned-int))

(defcfun ("glDepthMask" glDepthMask) :void
  (flag :unsigned-char))

(defcfun ("glDepthRange" glDepthRange) :void
  (near_val :double)
  (far_val :double))

(defcfun ("glClearAccum" glClearAccum) :void
  (red :float)
  (green :float)
  (blue :float)
  (alpha :float))

(defcfun ("glAccum" glAccum) :void
  (op :unsigned-int)
  (value :float))

(defcfun ("glMatrixMode" glMatrixMode) :void
  (mode :unsigned-int))

(defcfun ("glOrtho" glOrtho) :void
  (left :double)
  (right :double)
  (bottom :double)
  (top :double)
  (near_val :double)
  (far_val :double))

(defcfun ("glFrustum" glFrustum) :void
  (left :double)
  (right :double)
  (bottom :double)
  (top :double)
  (near_val :double)
  (far_val :double))

(defcfun ("glViewport" glViewport) :void
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defcfun ("glPushMatrix" glPushMatrix) :void)

(defcfun ("glPopMatrix" glPopMatrix) :void)

(defcfun ("glLoadIdentity" glLoadIdentity) :void)

(defcfun ("glLoadMatrixd" glLoadMatrixd) :void
  (m :pointer))

(defcfun ("glLoadMatrixf" glLoadMatrixf) :void
  (m :pointer))

(defcfun ("glMultMatrixd" glMultMatrixd) :void
  (m :pointer))

(defcfun ("glMultMatrixf" glMultMatrixf) :void
  (m :pointer))

(defcfun ("glRotated" glRotated) :void
  (angle :double)
  (x :double)
  (y :double)
  (z :double))

(defcfun ("glRotatef" glRotatef) :void
  (angle :float)
  (x :float)
  (y :float)
  (z :float))

(defcfun ("glScaled" glScaled) :void
  (x :double)
  (y :double)
  (z :double))

(defcfun ("glScalef" glScalef) :void
  (x :float)
  (y :float)
  (z :float))

(defcfun ("glTranslated" glTranslated) :void
  (x :double)
  (y :double)
  (z :double))

(defcfun ("glTranslatef" glTranslatef) :void
  (x :float)
  (y :float)
  (z :float))

(defcfun ("glIsList" glIsList) :unsigned-char
  (list :unsigned-int))

(defcfun ("glDeleteLists" glDeleteLists) :void
  (list :unsigned-int)
  (range :int))

(defcfun ("glGenLists" glGenLists) :unsigned-int
  (range :int))

(defcfun ("glNewList" glNewList) :void
  (list :unsigned-int)
  (mode :unsigned-int))

(defcfun ("glEndList" glEndList) :void)

(defcfun ("glCallList" glCallList) :void
  (list :unsigned-int))

(defcfun ("glCallLists" glCallLists) :void
  (n :int)
  (type :unsigned-int)
  (lists :pointer))

(defcfun ("glListBase" glListBase) :void
  (base :unsigned-int))

(defcfun ("glBegin" glBegin) :void
  (mode :unsigned-int))

(defcfun ("glEnd" glEnd) :void)

(defcfun ("glVertex2d" glVertex2d) :void
  (x :double)
  (y :double))

(defcfun ("glVertex2f" glVertex2f) :void
  (x :float)
  (y :float))

(defcfun ("glVertex2i" glVertex2i) :void
  (x :int)
  (y :int))

(defcfun ("glVertex2s" glVertex2s) :void
  (x :short)
  (y :short))

(defcfun ("glVertex3d" glVertex3d) :void
  (x :double)
  (y :double)
  (z :double))

(defcfun ("glVertex3f" glVertex3f) :void
  (x :float)
  (y :float)
  (z :float))

(defcfun ("glVertex3i" glVertex3i) :void
  (x :int)
  (y :int)
  (z :int))

(defcfun ("glVertex3s" glVertex3s) :void
  (x :short)
  (y :short)
  (z :short))

(defcfun ("glVertex4d" glVertex4d) :void
  (x :double)
  (y :double)
  (z :double)
  (w :double))

(defcfun ("glVertex4f" glVertex4f) :void
  (x :float)
  (y :float)
  (z :float)
  (w :float))

(defcfun ("glVertex4i" glVertex4i) :void
  (x :int)
  (y :int)
  (z :int)
  (w :int))

(defcfun ("glVertex4s" glVertex4s) :void
  (x :short)
  (y :short)
  (z :short)
  (w :short))

(defcfun ("glVertex2dv" glVertex2dv) :void
  (v :pointer))

(defcfun ("glVertex2fv" glVertex2fv) :void
  (v :pointer))

(defcfun ("glVertex2iv" glVertex2iv) :void
  (v :pointer))

(defcfun ("glVertex2sv" glVertex2sv) :void
  (v :pointer))

(defcfun ("glVertex3dv" glVertex3dv) :void
  (v :pointer))

(defcfun ("glVertex3fv" glVertex3fv) :void
  (v :pointer))

(defcfun ("glVertex3iv" glVertex3iv) :void
  (v :pointer))

(defcfun ("glVertex3sv" glVertex3sv) :void
  (v :pointer))

(defcfun ("glVertex4dv" glVertex4dv) :void
  (v :pointer))

(defcfun ("glVertex4fv" glVertex4fv) :void
  (v :pointer))

(defcfun ("glVertex4iv" glVertex4iv) :void
  (v :pointer))

(defcfun ("glVertex4sv" glVertex4sv) :void
  (v :pointer))

(defcfun ("glNormal3b" glNormal3b) :void
  (nx :char)
  (ny :char)
  (nz :char))

(defcfun ("glNormal3d" glNormal3d) :void
  (nx :double)
  (ny :double)
  (nz :double))

(defcfun ("glNormal3f" glNormal3f) :void
  (nx :float)
  (ny :float)
  (nz :float))

(defcfun ("glNormal3i" glNormal3i) :void
  (nx :int)
  (ny :int)
  (nz :int))

(defcfun ("glNormal3s" glNormal3s) :void
  (nx :short)
  (ny :short)
  (nz :short))

(defcfun ("glNormal3bv" glNormal3bv) :void
  (v :pointer))

(defcfun ("glNormal3dv" glNormal3dv) :void
  (v :pointer))

(defcfun ("glNormal3fv" glNormal3fv) :void
  (v :pointer))

(defcfun ("glNormal3iv" glNormal3iv) :void
  (v :pointer))

(defcfun ("glNormal3sv" glNormal3sv) :void
  (v :pointer))

(defcfun ("glIndexd" glIndexd) :void
  (c :double))

(defcfun ("glIndexf" glIndexf) :void
  (c :float))

(defcfun ("glIndexi" glIndexi) :void
  (c :int))

(defcfun ("glIndexs" glIndexs) :void
  (c :short))

(defcfun ("glIndexub" glIndexub) :void
  (c :unsigned-char))

(defcfun ("glIndexdv" glIndexdv) :void
  (c :pointer))

(defcfun ("glIndexfv" glIndexfv) :void
  (c :pointer))

(defcfun ("glIndexiv" glIndexiv) :void
  (c :pointer))

(defcfun ("glIndexsv" glIndexsv) :void
  (c :pointer))

(defcfun ("glIndexubv" glIndexubv) :void
  (c :pointer))

(defcfun ("glColor3b" glColor3b) :void
  (red :char)
  (green :char)
  (blue :char))

(defcfun ("glColor3d" glColor3d) :void
  (red :double)
  (green :double)
  (blue :double))

(defcfun ("glColor3f" glColor3f) :void
  (red :float)
  (green :float)
  (blue :float))

(defcfun ("glColor3i" glColor3i) :void
  (red :int)
  (green :int)
  (blue :int))

(defcfun ("glColor3s" glColor3s) :void
  (red :short)
  (green :short)
  (blue :short))

(defcfun ("glColor3ub" glColor3ub) :void
  (red :unsigned-char)
  (green :unsigned-char)
  (blue :unsigned-char))

(defcfun ("glColor3ui" glColor3ui) :void
  (red :unsigned-int)
  (green :unsigned-int)
  (blue :unsigned-int))

(defcfun ("glColor3us" glColor3us) :void
  (red :unsigned-short)
  (green :unsigned-short)
  (blue :unsigned-short))

(defcfun ("glColor4b" glColor4b) :void
  (red :char)
  (green :char)
  (blue :char)
  (alpha :char))

(defcfun ("glColor4d" glColor4d) :void
  (red :double)
  (green :double)
  (blue :double)
  (alpha :double))

(defcfun ("glColor4f" glColor4f) :void
  (red :float)
  (green :float)
  (blue :float)
  (alpha :float))

(defcfun ("glColor4i" glColor4i) :void
  (red :int)
  (green :int)
  (blue :int)
  (alpha :int))

(defcfun ("glColor4s" glColor4s) :void
  (red :short)
  (green :short)
  (blue :short)
  (alpha :short))

(defcfun ("glColor4ub" glColor4ub) :void
  (red :unsigned-char)
  (green :unsigned-char)
  (blue :unsigned-char)
  (alpha :unsigned-char))

(defcfun ("glColor4ui" glColor4ui) :void
  (red :unsigned-int)
  (green :unsigned-int)
  (blue :unsigned-int)
  (alpha :unsigned-int))

(defcfun ("glColor4us" glColor4us) :void
  (red :unsigned-short)
  (green :unsigned-short)
  (blue :unsigned-short)
  (alpha :unsigned-short))

(defcfun ("glColor3bv" glColor3bv) :void
  (v :pointer))

(defcfun ("glColor3dv" glColor3dv) :void
  (v :pointer))

(defcfun ("glColor3fv" glColor3fv) :void
  (v :pointer))

(defcfun ("glColor3iv" glColor3iv) :void
  (v :pointer))

(defcfun ("glColor3sv" glColor3sv) :void
  (v :pointer))

(defcfun ("glColor3ubv" glColor3ubv) :void
  (v :pointer))

(defcfun ("glColor3uiv" glColor3uiv) :void
  (v :pointer))

(defcfun ("glColor3usv" glColor3usv) :void
  (v :pointer))

(defcfun ("glColor4bv" glColor4bv) :void
  (v :pointer))

(defcfun ("glColor4dv" glColor4dv) :void
  (v :pointer))

(defcfun ("glColor4fv" glColor4fv) :void
  (v :pointer))

(defcfun ("glColor4iv" glColor4iv) :void
  (v :pointer))

(defcfun ("glColor4sv" glColor4sv) :void
  (v :pointer))

(defcfun ("glColor4ubv" glColor4ubv) :void
  (v :pointer))

(defcfun ("glColor4uiv" glColor4uiv) :void
  (v :pointer))

(defcfun ("glColor4usv" glColor4usv) :void
  (v :pointer))

(defcfun ("glTexCoord1d" glTexCoord1d) :void
  (s :double))

(defcfun ("glTexCoord1f" glTexCoord1f) :void
  (s :float))

(defcfun ("glTexCoord1i" glTexCoord1i) :void
  (s :int))

(defcfun ("glTexCoord1s" glTexCoord1s) :void
  (s :short))

(defcfun ("glTexCoord2d" glTexCoord2d) :void
  (s :double)
  (t_arg1 :double))

(defcfun ("glTexCoord2f" glTexCoord2f) :void
  (s :float)
  (t_arg1 :float))

(defcfun ("glTexCoord2i" glTexCoord2i) :void
  (s :int)
  (t_arg1 :int))

(defcfun ("glTexCoord2s" glTexCoord2s) :void
  (s :short)
  (t_arg1 :short))

(defcfun ("glTexCoord3d" glTexCoord3d) :void
  (s :double)
  (t_arg1 :double)
  (r :double))

(defcfun ("glTexCoord3f" glTexCoord3f) :void
  (s :float)
  (t_arg1 :float)
  (r :float))

(defcfun ("glTexCoord3i" glTexCoord3i) :void
  (s :int)
  (t_arg1 :int)
  (r :int))

(defcfun ("glTexCoord3s" glTexCoord3s) :void
  (s :short)
  (t_arg1 :short)
  (r :short))

(defcfun ("glTexCoord4d" glTexCoord4d) :void
  (s :double)
  (t_arg1 :double)
  (r :double)
  (q :double))

(defcfun ("glTexCoord4f" glTexCoord4f) :void
  (s :float)
  (t_arg1 :float)
  (r :float)
  (q :float))

(defcfun ("glTexCoord4i" glTexCoord4i) :void
  (s :int)
  (t_arg1 :int)
  (r :int)
  (q :int))

(defcfun ("glTexCoord4s" glTexCoord4s) :void
  (s :short)
  (t_arg1 :short)
  (r :short)
  (q :short))

(defcfun ("glTexCoord1dv" glTexCoord1dv) :void
  (v :pointer))

(defcfun ("glTexCoord1fv" glTexCoord1fv) :void
  (v :pointer))

(defcfun ("glTexCoord1iv" glTexCoord1iv) :void
  (v :pointer))

(defcfun ("glTexCoord1sv" glTexCoord1sv) :void
  (v :pointer))

(defcfun ("glTexCoord2dv" glTexCoord2dv) :void
  (v :pointer))

(defcfun ("glTexCoord2fv" glTexCoord2fv) :void
  (v :pointer))

(defcfun ("glTexCoord2iv" glTexCoord2iv) :void
  (v :pointer))

(defcfun ("glTexCoord2sv" glTexCoord2sv) :void
  (v :pointer))

(defcfun ("glTexCoord3dv" glTexCoord3dv) :void
  (v :pointer))

(defcfun ("glTexCoord3fv" glTexCoord3fv) :void
  (v :pointer))

(defcfun ("glTexCoord3iv" glTexCoord3iv) :void
  (v :pointer))

(defcfun ("glTexCoord3sv" glTexCoord3sv) :void
  (v :pointer))

(defcfun ("glTexCoord4dv" glTexCoord4dv) :void
  (v :pointer))

(defcfun ("glTexCoord4fv" glTexCoord4fv) :void
  (v :pointer))

(defcfun ("glTexCoord4iv" glTexCoord4iv) :void
  (v :pointer))

(defcfun ("glTexCoord4sv" glTexCoord4sv) :void
  (v :pointer))

(defcfun ("glRasterPos2d" glRasterPos2d) :void
  (x :double)
  (y :double))

(defcfun ("glRasterPos2f" glRasterPos2f) :void
  (x :float)
  (y :float))

(defcfun ("glRasterPos2i" glRasterPos2i) :void
  (x :int)
  (y :int))

(defcfun ("glRasterPos2s" glRasterPos2s) :void
  (x :short)
  (y :short))

(defcfun ("glRasterPos3d" glRasterPos3d) :void
  (x :double)
  (y :double)
  (z :double))

(defcfun ("glRasterPos3f" glRasterPos3f) :void
  (x :float)
  (y :float)
  (z :float))

(defcfun ("glRasterPos3i" glRasterPos3i) :void
  (x :int)
  (y :int)
  (z :int))

(defcfun ("glRasterPos3s" glRasterPos3s) :void
  (x :short)
  (y :short)
  (z :short))

(defcfun ("glRasterPos4d" glRasterPos4d) :void
  (x :double)
  (y :double)
  (z :double)
  (w :double))

(defcfun ("glRasterPos4f" glRasterPos4f) :void
  (x :float)
  (y :float)
  (z :float)
  (w :float))

(defcfun ("glRasterPos4i" glRasterPos4i) :void
  (x :int)
  (y :int)
  (z :int)
  (w :int))

(defcfun ("glRasterPos4s" glRasterPos4s) :void
  (x :short)
  (y :short)
  (z :short)
  (w :short))

(defcfun ("glRasterPos2dv" glRasterPos2dv) :void
  (v :pointer))

(defcfun ("glRasterPos2fv" glRasterPos2fv) :void
  (v :pointer))

(defcfun ("glRasterPos2iv" glRasterPos2iv) :void
  (v :pointer))

(defcfun ("glRasterPos2sv" glRasterPos2sv) :void
  (v :pointer))

(defcfun ("glRasterPos3dv" glRasterPos3dv) :void
  (v :pointer))

(defcfun ("glRasterPos3fv" glRasterPos3fv) :void
  (v :pointer))

(defcfun ("glRasterPos3iv" glRasterPos3iv) :void
  (v :pointer))

(defcfun ("glRasterPos3sv" glRasterPos3sv) :void
  (v :pointer))

(defcfun ("glRasterPos4dv" glRasterPos4dv) :void
  (v :pointer))

(defcfun ("glRasterPos4fv" glRasterPos4fv) :void
  (v :pointer))

(defcfun ("glRasterPos4iv" glRasterPos4iv) :void
  (v :pointer))

(defcfun ("glRasterPos4sv" glRasterPos4sv) :void
  (v :pointer))

(defcfun ("glRectd" glRectd) :void
  (x1 :double)
  (y1 :double)
  (x2 :double)
  (y2 :double))

(defcfun ("glRectf" glRectf) :void
  (x1 :float)
  (y1 :float)
  (x2 :float)
  (y2 :float))

(defcfun ("glRecti" glRecti) :void
  (x1 :int)
  (y1 :int)
  (x2 :int)
  (y2 :int))

(defcfun ("glRects" glRects) :void
  (x1 :short)
  (y1 :short)
  (x2 :short)
  (y2 :short))

(defcfun ("glRectdv" glRectdv) :void
  (v1 :pointer)
  (v2 :pointer))

(defcfun ("glRectfv" glRectfv) :void
  (v1 :pointer)
  (v2 :pointer))

(defcfun ("glRectiv" glRectiv) :void
  (v1 :pointer)
  (v2 :pointer))

(defcfun ("glRectsv" glRectsv) :void
  (v1 :pointer)
  (v2 :pointer))

(defcfun ("glVertexPointer" glVertexPointer) :void
  (size :int)
  (type :unsigned-int)
  (stride :int)
  (ptr :pointer))

(defcfun ("glNormalPointer" glNormalPointer) :void
  (type :unsigned-int)
  (stride :int)
  (ptr :pointer))

(defcfun ("glColorPointer" glColorPointer) :void
  (size :int)
  (type :unsigned-int)
  (stride :int)
  (ptr :pointer))

(defcfun ("glIndexPointer" glIndexPointer) :void
  (type :unsigned-int)
  (stride :int)
  (ptr :pointer))

(defcfun ("glTexCoordPointer" glTexCoordPointer) :void
  (size :int)
  (type :unsigned-int)
  (stride :int)
  (ptr :pointer))

(defcfun ("glEdgeFlagPointer" glEdgeFlagPointer) :void
  (stride :int)
  (ptr :pointer))

(defcfun ("glGetPointerv" glGetPointerv) :void
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glArrayElement" glArrayElement) :void
  (i :int))

(defcfun ("glDrawArrays" glDrawArrays) :void
  (mode :unsigned-int)
  (first :int)
  (count :int))

(defcfun ("glDrawElements" glDrawElements) :void
  (mode :unsigned-int)
  (count :int)
  (type :unsigned-int)
  (indices :pointer))

(defcfun ("glInterleavedArrays" glInterleavedArrays) :void
  (format :unsigned-int)
  (stride :int)
  (pointer :pointer))

(defcfun ("glShadeModel" glShadeModel) :void
  (mode :unsigned-int))

(defcfun ("glLightf" glLightf) :void
  (light :unsigned-int)
  (pname :unsigned-int)
  (param :float))

(defcfun ("glLighti" glLighti) :void
  (light :unsigned-int)
  (pname :unsigned-int)
  (param :int))

(defcfun ("glLightfv" glLightfv) :void
  (light :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glLightiv" glLightiv) :void
  (light :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glGetLightfv" glGetLightfv) :void
  (light :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glGetLightiv" glGetLightiv) :void
  (light :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glLightModelf" glLightModelf) :void
  (pname :unsigned-int)
  (param :float))

(defcfun ("glLightModeli" glLightModeli) :void
  (pname :unsigned-int)
  (param :int))

(defcfun ("glLightModelfv" glLightModelfv) :void
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glLightModeliv" glLightModeliv) :void
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glMaterialf" glMaterialf) :void
  (face :unsigned-int)
  (pname :unsigned-int)
  (param :float))

(defcfun ("glMateriali" glMateriali) :void
  (face :unsigned-int)
  (pname :unsigned-int)
  (param :int))

(defcfun ("glMaterialfv" glMaterialfv) :void
  (face :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glMaterialiv" glMaterialiv) :void
  (face :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glGetMaterialfv" glGetMaterialfv) :void
  (face :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glGetMaterialiv" glGetMaterialiv) :void
  (face :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glColorMaterial" glColorMaterial) :void
  (face :unsigned-int)
  (mode :unsigned-int))

(defcfun ("glPixelZoom" glPixelZoom) :void
  (xfactor :float)
  (yfactor :float))

(defcfun ("glPixelStoref" glPixelStoref) :void
  (pname :unsigned-int)
  (param :float))

(defcfun ("glPixelStorei" glPixelStorei) :void
  (pname :unsigned-int)
  (param :int))

(defcfun ("glPixelTransferf" glPixelTransferf) :void
  (pname :unsigned-int)
  (param :float))

(defcfun ("glPixelTransferi" glPixelTransferi) :void
  (pname :unsigned-int)
  (param :int))

(defcfun ("glPixelMapfv" glPixelMapfv) :void
  (map :unsigned-int)
  (mapsize :int)
  (values :pointer))

(defcfun ("glPixelMapuiv" glPixelMapuiv) :void
  (map :unsigned-int)
  (mapsize :int)
  (values :pointer))

(defcfun ("glPixelMapusv" glPixelMapusv) :void
  (map :unsigned-int)
  (mapsize :int)
  (values :pointer))

(defcfun ("glGetPixelMapfv" glGetPixelMapfv) :void
  (map :unsigned-int)
  (values :pointer))

(defcfun ("glGetPixelMapuiv" glGetPixelMapuiv) :void
  (map :unsigned-int)
  (values :pointer))

(defcfun ("glGetPixelMapusv" glGetPixelMapusv) :void
  (map :unsigned-int)
  (values :pointer))

(defcfun ("glBitmap" glBitmap) :void
  (width :int)
  (height :int)
  (xorig :float)
  (yorig :float)
  (xmove :float)
  (ymove :float)
  (bitmap :pointer))

(defcfun ("glReadPixels" glReadPixels) :void
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (format :unsigned-int)
  (type :unsigned-int)
  (pixels :pointer))

(defcfun ("glDrawPixels" glDrawPixels) :void
  (width :int)
  (height :int)
  (format :unsigned-int)
  (type :unsigned-int)
  (pixels :pointer))

(defcfun ("glCopyPixels" glCopyPixels) :void
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (type :unsigned-int))

(defcfun ("glStencilFunc" glStencilFunc) :void
  (func :unsigned-int)
  (ref :int)
  (mask :unsigned-int))

(defcfun ("glStencilMask" glStencilMask) :void
  (mask :unsigned-int))

(defcfun ("glStencilOp" glStencilOp) :void
  (fail :unsigned-int)
  (zfail :unsigned-int)
  (zpass :unsigned-int))

(defcfun ("glClearStencil" glClearStencil) :void
  (s :int))

(defcfun ("glTexGend" glTexGend) :void
  (coord :unsigned-int)
  (pname :unsigned-int)
  (param :double))

(defcfun ("glTexGenf" glTexGenf) :void
  (coord :unsigned-int)
  (pname :unsigned-int)
  (param :float))

(defcfun ("glTexGeni" glTexGeni) :void
  (coord :unsigned-int)
  (pname :unsigned-int)
  (param :int))

(defcfun ("glTexGendv" glTexGendv) :void
  (coord :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glTexGenfv" glTexGenfv) :void
  (coord :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glTexGeniv" glTexGeniv) :void
  (coord :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glGetTexGendv" glGetTexGendv) :void
  (coord :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glGetTexGenfv" glGetTexGenfv) :void
  (coord :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glGetTexGeniv" glGetTexGeniv) :void
  (coord :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glTexEnvf" glTexEnvf) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (param :float))

(defcfun ("glTexEnvi" glTexEnvi) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (param :int))

(defcfun ("glTexEnvfv" glTexEnvfv) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glTexEnviv" glTexEnviv) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glGetTexEnvfv" glGetTexEnvfv) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glGetTexEnviv" glGetTexEnviv) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glTexParameterf" glTexParameterf) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (param :float))

(defcfun ("glTexParameteri" glTexParameteri) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (param :int))

(defcfun ("glTexParameterfv" glTexParameterfv) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glTexParameteriv" glTexParameteriv) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glGetTexParameterfv" glGetTexParameterfv) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glGetTexParameteriv" glGetTexParameteriv) :void
  (target :unsigned-int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glGetTexLevelParameterfv" glGetTexLevelParameterfv) :void
  (target :unsigned-int)
  (level :int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glGetTexLevelParameteriv" glGetTexLevelParameteriv) :void
  (target :unsigned-int)
  (level :int)
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glTexImage1D" glTexImage1D) :void
  (target :unsigned-int)
  (level :int)
  (internalFormat :int)
  (width :int)
  (border :int)
  (format :unsigned-int)
  (type :unsigned-int)
  (pixels :pointer))

(defcfun ("glTexImage2D" glTexImage2D) :void
  (target :unsigned-int)
  (level :int)
  (internalFormat :int)
  (width :int)
  (height :int)
  (border :int)
  (format :unsigned-int)
  (type :unsigned-int)
  (pixels :pointer))

(defcfun ("glGetTexImage" glGetTexImage) :void
  (target :unsigned-int)
  (level :int)
  (format :unsigned-int)
  (type :unsigned-int)
  (pixels :pointer))

(defcfun ("glGenTextures" glGenTextures) :void
  (n :int)
  (textures :pointer))

(defcfun ("glDeleteTextures" glDeleteTextures) :void
  (n :int)
  (textures :pointer))

(defcfun ("glBindTexture" glBindTexture) :void
  (target :unsigned-int)
  (texture :unsigned-int))

(defcfun ("glPrioritizeTextures" glPrioritizeTextures) :void
  (n :int)
  (textures :pointer)
  (priorities :pointer))

(defcfun ("glAreTexturesResident" glAreTexturesResident) :unsigned-char
  (n :int)
  (textures :pointer)
  (residences :pointer))

(defcfun ("glIsTexture" glIsTexture) :unsigned-char
  (texture :unsigned-int))

(defcfun ("glTexSubImage1D" glTexSubImage1D) :void
  (target :unsigned-int)
  (level :int)
  (xoffset :int)
  (width :int)
  (format :unsigned-int)
  (type :unsigned-int)
  (pixels :pointer))

(defcfun ("glTexSubImage2D" glTexSubImage2D) :void
  (target :unsigned-int)
  (level :int)
  (xoffset :int)
  (yoffset :int)
  (width :int)
  (height :int)
  (format :unsigned-int)
  (type :unsigned-int)
  (pixels :pointer))

(defcfun ("glCopyTexImage1D" glCopyTexImage1D) :void
  (target :unsigned-int)
  (level :int)
  (internalformat :unsigned-int)
  (x :int)
  (y :int)
  (width :int)
  (border :int))

(defcfun ("glCopyTexImage2D" glCopyTexImage2D) :void
  (target :unsigned-int)
  (level :int)
  (internalformat :unsigned-int)
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (border :int))

(defcfun ("glCopyTexSubImage1D" glCopyTexSubImage1D) :void
  (target :unsigned-int)
  (level :int)
  (xoffset :int)
  (x :int)
  (y :int)
  (width :int))

(defcfun ("glCopyTexSubImage2D" glCopyTexSubImage2D) :void
  (target :unsigned-int)
  (level :int)
  (xoffset :int)
  (yoffset :int)
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defcfun ("glMap1d" glMap1d) :void
  (target :unsigned-int)
  (u1 :double)
  (u2 :double)
  (stride :int)
  (order :int)
  (points :pointer))

(defcfun ("glMap1f" glMap1f) :void
  (target :unsigned-int)
  (u1 :float)
  (u2 :float)
  (stride :int)
  (order :int)
  (points :pointer))

(defcfun ("glMap2d" glMap2d) :void
  (target :unsigned-int)
  (u1 :double)
  (u2 :double)
  (ustride :int)
  (uorder :int)
  (v1 :double)
  (v2 :double)
  (vstride :int)
  (vorder :int)
  (points :pointer))

(defcfun ("glMap2f" glMap2f) :void
  (target :unsigned-int)
  (u1 :float)
  (u2 :float)
  (ustride :int)
  (uorder :int)
  (v1 :float)
  (v2 :float)
  (vstride :int)
  (vorder :int)
  (points :pointer))

(defcfun ("glGetMapdv" glGetMapdv) :void
  (target :unsigned-int)
  (query :unsigned-int)
  (v :pointer))

(defcfun ("glGetMapfv" glGetMapfv) :void
  (target :unsigned-int)
  (query :unsigned-int)
  (v :pointer))

(defcfun ("glGetMapiv" glGetMapiv) :void
  (target :unsigned-int)
  (query :unsigned-int)
  (v :pointer))

(defcfun ("glEvalCoord1d" glEvalCoord1d) :void
  (u :double))

(defcfun ("glEvalCoord1f" glEvalCoord1f) :void
  (u :float))

(defcfun ("glEvalCoord1dv" glEvalCoord1dv) :void
  (u :pointer))

(defcfun ("glEvalCoord1fv" glEvalCoord1fv) :void
  (u :pointer))

(defcfun ("glEvalCoord2d" glEvalCoord2d) :void
  (u :double)
  (v :double))

(defcfun ("glEvalCoord2f" glEvalCoord2f) :void
  (u :float)
  (v :float))

(defcfun ("glEvalCoord2dv" glEvalCoord2dv) :void
  (u :pointer))

(defcfun ("glEvalCoord2fv" glEvalCoord2fv) :void
  (u :pointer))

(defcfun ("glMapGrid1d" glMapGrid1d) :void
  (un :int)
  (u1 :double)
  (u2 :double))

(defcfun ("glMapGrid1f" glMapGrid1f) :void
  (un :int)
  (u1 :float)
  (u2 :float))

(defcfun ("glMapGrid2d" glMapGrid2d) :void
  (un :int)
  (u1 :double)
  (u2 :double)
  (vn :int)
  (v1 :double)
  (v2 :double))

(defcfun ("glMapGrid2f" glMapGrid2f) :void
  (un :int)
  (u1 :float)
  (u2 :float)
  (vn :int)
  (v1 :float)
  (v2 :float))

(defcfun ("glEvalPoint1" glEvalPoint1) :void
  (i :int))

(defcfun ("glEvalPoint2" glEvalPoint2) :void
  (i :int)
  (j :int))

(defcfun ("glEvalMesh1" glEvalMesh1) :void
  (mode :unsigned-int)
  (i1 :int)
  (i2 :int))

(defcfun ("glEvalMesh2" glEvalMesh2) :void
  (mode :unsigned-int)
  (i1 :int)
  (i2 :int)
  (j1 :int)
  (j2 :int))

(defcfun ("glFogf" glFogf) :void
  (pname :unsigned-int)
  (param :float))

(defcfun ("glFogi" glFogi) :void
  (pname :unsigned-int)
  (param :int))

(defcfun ("glFogfv" glFogfv) :void
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glFogiv" glFogiv) :void
  (pname :unsigned-int)
  (params :pointer))

(defcfun ("glFeedbackBuffer" glFeedbackBuffer) :void
  (size :int)
  (type :unsigned-int)
  (buffer :pointer))

(defcfun ("glPassThrough" glPassThrough) :void
  (token :float))

(defcfun ("glSelectBuffer" glSelectBuffer) :void
  (size :int)
  (buffer :pointer))

(defcfun ("glInitNames" glInitNames) :void)

(defcfun ("glLoadName" glLoadName) :void
  (name :unsigned-int))

(defcfun ("glPushName" glPushName) :void
  (name :unsigned-int))

(defcfun ("glPopName" glPopName) :void)

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

(defconstant GL_GLEXT_VERSION 29)

(defconstant GL_BLEND_DST_RGB #x80C8)

(defconstant GL_BLEND_SRC_RGB #x80C9)

(defconstant GL_BLEND_DST_ALPHA #x80CA)

(defconstant GL_BLEND_SRC_ALPHA #x80CB)

(defconstant GL_POINT_SIZE_MIN #x8126)

(defconstant GL_POINT_SIZE_MAX #x8127)

(defconstant GL_POINT_FADE_THRESHOLD_SIZE #x8128)

(defconstant GL_POINT_DISTANCE_ATTENUATION #x8129)

(defconstant GL_GENERATE_MIPMAP #x8191)

(defconstant GL_GENERATE_MIPMAP_HINT #x8192)

(defconstant GL_DEPTH_COMPONENT16 #x81A5)

(defconstant GL_DEPTH_COMPONENT24 #x81A6)

(defconstant GL_DEPTH_COMPONENT32 #x81A7)

(defconstant GL_MIRRORED_REPEAT #x8370)

(defconstant GL_FOG_COORDINATE_SOURCE #x8450)

(defconstant GL_FOG_COORDINATE #x8451)

(defconstant GL_FRAGMENT_DEPTH #x8452)

(defconstant GL_CURRENT_FOG_COORDINATE #x8453)

(defconstant GL_FOG_COORDINATE_ARRAY_TYPE #x8454)

(defconstant GL_FOG_COORDINATE_ARRAY_STRIDE #x8455)

(defconstant GL_FOG_COORDINATE_ARRAY_POINTER #x8456)

(defconstant GL_FOG_COORDINATE_ARRAY #x8457)

(defconstant GL_COLOR_SUM #x8458)

(defconstant GL_CURRENT_SECONDARY_COLOR #x8459)

(defconstant GL_SECONDARY_COLOR_ARRAY_SIZE #x845A)

(defconstant GL_SECONDARY_COLOR_ARRAY_TYPE #x845B)

(defconstant GL_SECONDARY_COLOR_ARRAY_STRIDE #x845C)

(defconstant GL_SECONDARY_COLOR_ARRAY_POINTER #x845D)

(defconstant GL_SECONDARY_COLOR_ARRAY #x845E)

(defconstant GL_MAX_TEXTURE_LOD_BIAS #x84FD)

(defconstant GL_TEXTURE_FILTER_CONTROL #x8500)

(defconstant GL_TEXTURE_LOD_BIAS #x8501)

(defconstant GL_INCR_WRAP #x8507)

(defconstant GL_DECR_WRAP #x8508)

(defconstant GL_TEXTURE_DEPTH_SIZE #x884A)

(defconstant GL_DEPTH_TEXTURE_MODE #x884B)

(defconstant GL_TEXTURE_COMPARE_MODE #x884C)

(defconstant GL_TEXTURE_COMPARE_FUNC #x884D)

(defconstant GL_COMPARE_R_TO_TEXTURE #x884E)

(defconstant GL_BUFFER_SIZE #x8764)

(defconstant GL_BUFFER_USAGE #x8765)

(defconstant GL_QUERY_COUNTER_BITS #x8864)

(defconstant GL_CURRENT_QUERY #x8865)

(defconstant GL_QUERY_RESULT #x8866)

(defconstant GL_QUERY_RESULT_AVAILABLE #x8867)

(defconstant GL_ARRAY_BUFFER #x8892)

(defconstant GL_ELEMENT_ARRAY_BUFFER #x8893)

(defconstant GL_ARRAY_BUFFER_BINDING #x8894)

(defconstant GL_ELEMENT_ARRAY_BUFFER_BINDING #x8895)

(defconstant GL_VERTEX_ARRAY_BUFFER_BINDING #x8896)

(defconstant GL_NORMAL_ARRAY_BUFFER_BINDING #x8897)

(defconstant GL_COLOR_ARRAY_BUFFER_BINDING #x8898)

(defconstant GL_INDEX_ARRAY_BUFFER_BINDING #x8899)

(defconstant GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING #x889A)

(defconstant GL_EDGE_FLAG_ARRAY_BUFFER_BINDING #x889B)

(defconstant GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING #x889C)

(defconstant GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING #x889D)

(defconstant GL_WEIGHT_ARRAY_BUFFER_BINDING #x889E)

(defconstant GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING #x889F)

(defconstant GL_READ_ONLY #x88B8)

(defconstant GL_WRITE_ONLY #x88B9)

(defconstant GL_READ_WRITE #x88BA)

(defconstant GL_BUFFER_ACCESS #x88BB)

(defconstant GL_BUFFER_MAPPED #x88BC)

(defconstant GL_BUFFER_MAP_POINTER #x88BD)

(defconstant GL_STREAM_DRAW #x88E0)

(defconstant GL_STREAM_READ #x88E1)

(defconstant GL_STREAM_COPY #x88E2)

(defconstant GL_STATIC_DRAW #x88E4)

(defconstant GL_STATIC_READ #x88E5)

(defconstant GL_STATIC_COPY #x88E6)

(defconstant GL_DYNAMIC_DRAW #x88E8)

(defconstant GL_DYNAMIC_READ #x88E9)

(defconstant GL_DYNAMIC_COPY #x88EA)

(defconstant GL_SAMPLES_PASSED #x8914)

(defconstant GL_FOG_COORD_SRC #x8450)

(defconstant GL_FOG_COORD #x8451)

(defconstant GL_CURRENT_FOG_COORD #x8453)

(defconstant GL_FOG_COORD_ARRAY_TYPE #x8454)

(defconstant GL_FOG_COORD_ARRAY_STRIDE #x8455)

(defconstant GL_FOG_COORD_ARRAY_POINTER #x8456)

(defconstant GL_FOG_COORD_ARRAY #x8457)

(defconstant GL_FOG_COORD_ARRAY_BUFFER_BINDING #x889D)

(defconstant GL_SRC0_RGB #x8580)

(defconstant GL_SRC1_RGB #x8581)

(defconstant GL_SRC2_RGB #x8582)

(defconstant GL_SRC0_ALPHA #x8588)

(defconstant GL_SRC1_ALPHA #x8589)

(defconstant GL_SRC2_ALPHA #x858A)

(defconstant GL_BLEND_EQUATION_RGB #x8009)

(defconstant GL_VERTEX_ATTRIB_ARRAY_ENABLED #x8622)

(defconstant GL_VERTEX_ATTRIB_ARRAY_SIZE #x8623)

(defconstant GL_VERTEX_ATTRIB_ARRAY_STRIDE #x8624)

(defconstant GL_VERTEX_ATTRIB_ARRAY_TYPE #x8625)

(defconstant GL_CURRENT_VERTEX_ATTRIB #x8626)

(defconstant GL_VERTEX_PROGRAM_POINT_SIZE #x8642)

(defconstant GL_VERTEX_PROGRAM_TWO_SIDE #x8643)

(defconstant GL_VERTEX_ATTRIB_ARRAY_POINTER #x8645)

(defconstant GL_STENCIL_BACK_FUNC #x8800)

(defconstant GL_STENCIL_BACK_FAIL #x8801)

(defconstant GL_STENCIL_BACK_PASS_DEPTH_FAIL #x8802)

(defconstant GL_STENCIL_BACK_PASS_DEPTH_PASS #x8803)

(defconstant GL_MAX_DRAW_BUFFERS #x8824)

(defconstant GL_DRAW_BUFFER0 #x8825)

(defconstant GL_DRAW_BUFFER1 #x8826)

(defconstant GL_DRAW_BUFFER2 #x8827)

(defconstant GL_DRAW_BUFFER3 #x8828)

(defconstant GL_DRAW_BUFFER4 #x8829)

(defconstant GL_DRAW_BUFFER5 #x882A)

(defconstant GL_DRAW_BUFFER6 #x882B)

(defconstant GL_DRAW_BUFFER7 #x882C)

(defconstant GL_DRAW_BUFFER8 #x882D)

(defconstant GL_DRAW_BUFFER9 #x882E)

(defconstant GL_DRAW_BUFFER10 #x882F)

(defconstant GL_DRAW_BUFFER11 #x8830)

(defconstant GL_DRAW_BUFFER12 #x8831)

(defconstant GL_DRAW_BUFFER13 #x8832)

(defconstant GL_DRAW_BUFFER14 #x8833)

(defconstant GL_DRAW_BUFFER15 #x8834)

(defconstant GL_BLEND_EQUATION_ALPHA #x883D)

(defconstant GL_POINT_SPRITE #x8861)

(defconstant GL_COORD_REPLACE #x8862)

(defconstant GL_MAX_VERTEX_ATTRIBS #x8869)

(defconstant GL_VERTEX_ATTRIB_ARRAY_NORMALIZED #x886A)

(defconstant GL_MAX_TEXTURE_COORDS #x8871)

(defconstant GL_MAX_TEXTURE_IMAGE_UNITS #x8872)

(defconstant GL_FRAGMENT_SHADER #x8B30)

(defconstant GL_VERTEX_SHADER #x8B31)

(defconstant GL_MAX_FRAGMENT_UNIFORM_COMPONENTS #x8B49)

(defconstant GL_MAX_VERTEX_UNIFORM_COMPONENTS #x8B4A)

(defconstant GL_MAX_VARYING_FLOATS #x8B4B)

(defconstant GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS #x8B4C)

(defconstant GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS #x8B4D)

(defconstant GL_SHADER_TYPE #x8B4F)

(defconstant GL_FLOAT_VEC2 #x8B50)

(defconstant GL_FLOAT_VEC3 #x8B51)

(defconstant GL_FLOAT_VEC4 #x8B52)

(defconstant GL_INT_VEC2 #x8B53)

(defconstant GL_INT_VEC3 #x8B54)

(defconstant GL_INT_VEC4 #x8B55)

(defconstant GL_BOOL #x8B56)

(defconstant GL_BOOL_VEC2 #x8B57)

(defconstant GL_BOOL_VEC3 #x8B58)

(defconstant GL_BOOL_VEC4 #x8B59)

(defconstant GL_FLOAT_MAT2 #x8B5A)

(defconstant GL_FLOAT_MAT3 #x8B5B)

(defconstant GL_FLOAT_MAT4 #x8B5C)

(defconstant GL_SAMPLER_1D #x8B5D)

(defconstant GL_SAMPLER_2D #x8B5E)

(defconstant GL_SAMPLER_3D #x8B5F)

(defconstant GL_SAMPLER_CUBE #x8B60)

(defconstant GL_SAMPLER_1D_SHADOW #x8B61)

(defconstant GL_SAMPLER_2D_SHADOW #x8B62)

(defconstant GL_DELETE_STATUS #x8B80)

(defconstant GL_COMPILE_STATUS #x8B81)

(defconstant GL_LINK_STATUS #x8B82)

(defconstant GL_VALIDATE_STATUS #x8B83)

(defconstant GL_INFO_LOG_LENGTH #x8B84)

(defconstant GL_ATTACHED_SHADERS #x8B85)

(defconstant GL_ACTIVE_UNIFORMS #x8B86)

(defconstant GL_ACTIVE_UNIFORM_MAX_LENGTH #x8B87)

(defconstant GL_SHADER_SOURCE_LENGTH #x8B88)

(defconstant GL_ACTIVE_ATTRIBUTES #x8B89)

(defconstant GL_ACTIVE_ATTRIBUTE_MAX_LENGTH #x8B8A)

(defconstant GL_FRAGMENT_SHADER_DERIVATIVE_HINT #x8B8B)

(defconstant GL_SHADING_LANGUAGE_VERSION #x8B8C)

(defconstant GL_CURRENT_PROGRAM #x8B8D)

(defconstant GL_POINT_SPRITE_COORD_ORIGIN #x8CA0)

(defconstant GL_LOWER_LEFT #x8CA1)

(defconstant GL_UPPER_LEFT #x8CA2)

(defconstant GL_STENCIL_BACK_REF #x8CA3)

(defconstant GL_STENCIL_BACK_VALUE_MASK #x8CA4)

(defconstant GL_STENCIL_BACK_WRITEMASK #x8CA5)

(defconstant GL_TRANSPOSE_MODELVIEW_MATRIX_ARB #x84E3)

(defconstant GL_TRANSPOSE_PROJECTION_MATRIX_ARB #x84E4)

(defconstant GL_TRANSPOSE_TEXTURE_MATRIX_ARB #x84E5)

(defconstant GL_TRANSPOSE_COLOR_MATRIX_ARB #x84E6)

(defconstant GL_MULTISAMPLE_ARB #x809D)

(defconstant GL_SAMPLE_ALPHA_TO_COVERAGE_ARB #x809E)

(defconstant GL_SAMPLE_ALPHA_TO_ONE_ARB #x809F)

(defconstant GL_SAMPLE_COVERAGE_ARB #x80A0)

(defconstant GL_SAMPLE_BUFFERS_ARB #x80A8)

(defconstant GL_SAMPLES_ARB #x80A9)

(defconstant GL_SAMPLE_COVERAGE_VALUE_ARB #x80AA)

(defconstant GL_SAMPLE_COVERAGE_INVERT_ARB #x80AB)

(defconstant GL_MULTISAMPLE_BIT_ARB #x20000000)

(defconstant GL_NORMAL_MAP_ARB #x8511)

(defconstant GL_REFLECTION_MAP_ARB #x8512)

(defconstant GL_TEXTURE_CUBE_MAP_ARB #x8513)

(defconstant GL_TEXTURE_BINDING_CUBE_MAP_ARB #x8514)

(defconstant GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB #x8515)

(defconstant GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB #x8516)

(defconstant GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB #x8517)

(defconstant GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB #x8518)

(defconstant GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB #x8519)

(defconstant GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB #x851A)

(defconstant GL_PROXY_TEXTURE_CUBE_MAP_ARB #x851B)

(defconstant GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB #x851C)

(defconstant GL_COMPRESSED_ALPHA_ARB #x84E9)

(defconstant GL_COMPRESSED_LUMINANCE_ARB #x84EA)

(defconstant GL_COMPRESSED_LUMINANCE_ALPHA_ARB #x84EB)

(defconstant GL_COMPRESSED_INTENSITY_ARB #x84EC)

(defconstant GL_COMPRESSED_RGB_ARB #x84ED)

(defconstant GL_COMPRESSED_RGBA_ARB #x84EE)

(defconstant GL_TEXTURE_COMPRESSION_HINT_ARB #x84EF)

(defconstant GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB #x86A0)

(defconstant GL_TEXTURE_COMPRESSED_ARB #x86A1)

(defconstant GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB #x86A2)

(defconstant GL_COMPRESSED_TEXTURE_FORMATS_ARB #x86A3)

(defconstant GL_CLAMP_TO_BORDER_ARB #x812D)

(defconstant GL_POINT_SIZE_MIN_ARB #x8126)

(defconstant GL_POINT_SIZE_MAX_ARB #x8127)

(defconstant GL_POINT_FADE_THRESHOLD_SIZE_ARB #x8128)

(defconstant GL_POINT_DISTANCE_ATTENUATION_ARB #x8129)

(defconstant GL_MAX_VERTEX_UNITS_ARB #x86A4)

(defconstant GL_ACTIVE_VERTEX_UNITS_ARB #x86A5)

(defconstant GL_WEIGHT_SUM_UNITY_ARB #x86A6)

(defconstant GL_VERTEX_BLEND_ARB #x86A7)

(defconstant GL_CURRENT_WEIGHT_ARB #x86A8)

(defconstant GL_WEIGHT_ARRAY_TYPE_ARB #x86A9)

(defconstant GL_WEIGHT_ARRAY_STRIDE_ARB #x86AA)

(defconstant GL_WEIGHT_ARRAY_SIZE_ARB #x86AB)

(defconstant GL_WEIGHT_ARRAY_POINTER_ARB #x86AC)

(defconstant GL_WEIGHT_ARRAY_ARB #x86AD)

(defconstant GL_MODELVIEW0_ARB #x1700)

(defconstant GL_MODELVIEW1_ARB #x850A)

(defconstant GL_MODELVIEW2_ARB #x8722)

(defconstant GL_MODELVIEW3_ARB #x8723)

(defconstant GL_MODELVIEW4_ARB #x8724)

(defconstant GL_MODELVIEW5_ARB #x8725)

(defconstant GL_MODELVIEW6_ARB #x8726)

(defconstant GL_MODELVIEW7_ARB #x8727)

(defconstant GL_MODELVIEW8_ARB #x8728)

(defconstant GL_MODELVIEW9_ARB #x8729)

(defconstant GL_MODELVIEW10_ARB #x872A)

(defconstant GL_MODELVIEW11_ARB #x872B)

(defconstant GL_MODELVIEW12_ARB #x872C)

(defconstant GL_MODELVIEW13_ARB #x872D)

(defconstant GL_MODELVIEW14_ARB #x872E)

(defconstant GL_MODELVIEW15_ARB #x872F)

(defconstant GL_MODELVIEW16_ARB #x8730)

(defconstant GL_MODELVIEW17_ARB #x8731)

(defconstant GL_MODELVIEW18_ARB #x8732)

(defconstant GL_MODELVIEW19_ARB #x8733)

(defconstant GL_MODELVIEW20_ARB #x8734)

(defconstant GL_MODELVIEW21_ARB #x8735)

(defconstant GL_MODELVIEW22_ARB #x8736)

(defconstant GL_MODELVIEW23_ARB #x8737)

(defconstant GL_MODELVIEW24_ARB #x8738)

(defconstant GL_MODELVIEW25_ARB #x8739)

(defconstant GL_MODELVIEW26_ARB #x873A)

(defconstant GL_MODELVIEW27_ARB #x873B)

(defconstant GL_MODELVIEW28_ARB #x873C)

(defconstant GL_MODELVIEW29_ARB #x873D)

(defconstant GL_MODELVIEW30_ARB #x873E)

(defconstant GL_MODELVIEW31_ARB #x873F)

(defconstant GL_MATRIX_PALETTE_ARB #x8840)

(defconstant GL_MAX_MATRIX_PALETTE_STACK_DEPTH_ARB #x8841)

(defconstant GL_MAX_PALETTE_MATRICES_ARB #x8842)

(defconstant GL_CURRENT_PALETTE_MATRIX_ARB #x8843)

(defconstant GL_MATRIX_INDEX_ARRAY_ARB #x8844)

(defconstant GL_CURRENT_MATRIX_INDEX_ARB #x8845)

(defconstant GL_MATRIX_INDEX_ARRAY_SIZE_ARB #x8846)

(defconstant GL_MATRIX_INDEX_ARRAY_TYPE_ARB #x8847)

(defconstant GL_MATRIX_INDEX_ARRAY_STRIDE_ARB #x8848)

(defconstant GL_MATRIX_INDEX_ARRAY_POINTER_ARB #x8849)

(defconstant GL_COMBINE_ARB #x8570)

(defconstant GL_COMBINE_RGB_ARB #x8571)

(defconstant GL_COMBINE_ALPHA_ARB #x8572)

(defconstant GL_SOURCE0_RGB_ARB #x8580)

(defconstant GL_SOURCE1_RGB_ARB #x8581)

(defconstant GL_SOURCE2_RGB_ARB #x8582)

(defconstant GL_SOURCE0_ALPHA_ARB #x8588)

(defconstant GL_SOURCE1_ALPHA_ARB #x8589)

(defconstant GL_SOURCE2_ALPHA_ARB #x858A)

(defconstant GL_OPERAND0_RGB_ARB #x8590)

(defconstant GL_OPERAND1_RGB_ARB #x8591)

(defconstant GL_OPERAND2_RGB_ARB #x8592)

(defconstant GL_OPERAND0_ALPHA_ARB #x8598)

(defconstant GL_OPERAND1_ALPHA_ARB #x8599)

(defconstant GL_OPERAND2_ALPHA_ARB #x859A)

(defconstant GL_RGB_SCALE_ARB #x8573)

(defconstant GL_ADD_SIGNED_ARB #x8574)

(defconstant GL_INTERPOLATE_ARB #x8575)

(defconstant GL_SUBTRACT_ARB #x84E7)

(defconstant GL_CONSTANT_ARB #x8576)

(defconstant GL_PRIMARY_COLOR_ARB #x8577)

(defconstant GL_PREVIOUS_ARB #x8578)

(defconstant GL_DOT3_RGB_ARB #x86AE)

(defconstant GL_DOT3_RGBA_ARB #x86AF)

(defconstant GL_MIRRORED_REPEAT_ARB #x8370)

(defconstant GL_DEPTH_COMPONENT16_ARB #x81A5)

(defconstant GL_DEPTH_COMPONENT24_ARB #x81A6)

(defconstant GL_DEPTH_COMPONENT32_ARB #x81A7)

(defconstant GL_TEXTURE_DEPTH_SIZE_ARB #x884A)

(defconstant GL_DEPTH_TEXTURE_MODE_ARB #x884B)

(defconstant GL_TEXTURE_COMPARE_MODE_ARB #x884C)

(defconstant GL_TEXTURE_COMPARE_FUNC_ARB #x884D)

(defconstant GL_COMPARE_R_TO_TEXTURE_ARB #x884E)

(defconstant GL_TEXTURE_COMPARE_FAIL_VALUE_ARB #x80BF)

(defconstant GL_COLOR_SUM_ARB #x8458)

(defconstant GL_VERTEX_PROGRAM_ARB #x8620)

(defconstant GL_VERTEX_ATTRIB_ARRAY_ENABLED_ARB #x8622)

(defconstant GL_VERTEX_ATTRIB_ARRAY_SIZE_ARB #x8623)

(defconstant GL_VERTEX_ATTRIB_ARRAY_STRIDE_ARB #x8624)

(defconstant GL_VERTEX_ATTRIB_ARRAY_TYPE_ARB #x8625)

(defconstant GL_CURRENT_VERTEX_ATTRIB_ARB #x8626)

(defconstant GL_PROGRAM_LENGTH_ARB #x8627)

(defconstant GL_PROGRAM_STRING_ARB #x8628)

(defconstant GL_MAX_PROGRAM_MATRIX_STACK_DEPTH_ARB #x862E)

(defconstant GL_MAX_PROGRAM_MATRICES_ARB #x862F)

(defconstant GL_CURRENT_MATRIX_STACK_DEPTH_ARB #x8640)

(defconstant GL_CURRENT_MATRIX_ARB #x8641)

(defconstant GL_VERTEX_PROGRAM_POINT_SIZE_ARB #x8642)

(defconstant GL_VERTEX_PROGRAM_TWO_SIDE_ARB #x8643)

(defconstant GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB #x8645)

(defconstant GL_PROGRAM_ERROR_POSITION_ARB #x864B)

(defconstant GL_PROGRAM_BINDING_ARB #x8677)

(defconstant GL_MAX_VERTEX_ATTRIBS_ARB #x8869)

(defconstant GL_VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB #x886A)

(defconstant GL_PROGRAM_ERROR_STRING_ARB #x8874)

(defconstant GL_PROGRAM_FORMAT_ASCII_ARB #x8875)

(defconstant GL_PROGRAM_FORMAT_ARB #x8876)

(defconstant GL_PROGRAM_INSTRUCTIONS_ARB #x88A0)

(defconstant GL_MAX_PROGRAM_INSTRUCTIONS_ARB #x88A1)

(defconstant GL_PROGRAM_NATIVE_INSTRUCTIONS_ARB #x88A2)

(defconstant GL_MAX_PROGRAM_NATIVE_INSTRUCTIONS_ARB #x88A3)

(defconstant GL_PROGRAM_TEMPORARIES_ARB #x88A4)

(defconstant GL_MAX_PROGRAM_TEMPORARIES_ARB #x88A5)

(defconstant GL_PROGRAM_NATIVE_TEMPORARIES_ARB #x88A6)

(defconstant GL_MAX_PROGRAM_NATIVE_TEMPORARIES_ARB #x88A7)

(defconstant GL_PROGRAM_PARAMETERS_ARB #x88A8)

(defconstant GL_MAX_PROGRAM_PARAMETERS_ARB #x88A9)

(defconstant GL_PROGRAM_NATIVE_PARAMETERS_ARB #x88AA)

(defconstant GL_MAX_PROGRAM_NATIVE_PARAMETERS_ARB #x88AB)

(defconstant GL_PROGRAM_ATTRIBS_ARB #x88AC)

(defconstant GL_MAX_PROGRAM_ATTRIBS_ARB #x88AD)

(defconstant GL_PROGRAM_NATIVE_ATTRIBS_ARB #x88AE)

(defconstant GL_MAX_PROGRAM_NATIVE_ATTRIBS_ARB #x88AF)

(defconstant GL_PROGRAM_ADDRESS_REGISTERS_ARB #x88B0)

(defconstant GL_MAX_PROGRAM_ADDRESS_REGISTERS_ARB #x88B1)

(defconstant GL_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB #x88B2)

(defconstant GL_MAX_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB #x88B3)

(defconstant GL_MAX_PROGRAM_LOCAL_PARAMETERS_ARB #x88B4)

(defconstant GL_MAX_PROGRAM_ENV_PARAMETERS_ARB #x88B5)

(defconstant GL_PROGRAM_UNDER_NATIVE_LIMITS_ARB #x88B6)

(defconstant GL_TRANSPOSE_CURRENT_MATRIX_ARB #x88B7)

(defconstant GL_MATRIX0_ARB #x88C0)

(defconstant GL_MATRIX1_ARB #x88C1)

(defconstant GL_MATRIX2_ARB #x88C2)

(defconstant GL_MATRIX3_ARB #x88C3)

(defconstant GL_MATRIX4_ARB #x88C4)

(defconstant GL_MATRIX5_ARB #x88C5)

(defconstant GL_MATRIX6_ARB #x88C6)

(defconstant GL_MATRIX7_ARB #x88C7)

(defconstant GL_MATRIX8_ARB #x88C8)

(defconstant GL_MATRIX9_ARB #x88C9)

(defconstant GL_MATRIX10_ARB #x88CA)

(defconstant GL_MATRIX11_ARB #x88CB)

(defconstant GL_MATRIX12_ARB #x88CC)

(defconstant GL_MATRIX13_ARB #x88CD)

(defconstant GL_MATRIX14_ARB #x88CE)

(defconstant GL_MATRIX15_ARB #x88CF)

(defconstant GL_MATRIX16_ARB #x88D0)

(defconstant GL_MATRIX17_ARB #x88D1)

(defconstant GL_MATRIX18_ARB #x88D2)

(defconstant GL_MATRIX19_ARB #x88D3)

(defconstant GL_MATRIX20_ARB #x88D4)

(defconstant GL_MATRIX21_ARB #x88D5)

(defconstant GL_MATRIX22_ARB #x88D6)

(defconstant GL_MATRIX23_ARB #x88D7)

(defconstant GL_MATRIX24_ARB #x88D8)

(defconstant GL_MATRIX25_ARB #x88D9)

(defconstant GL_MATRIX26_ARB #x88DA)

(defconstant GL_MATRIX27_ARB #x88DB)

(defconstant GL_MATRIX28_ARB #x88DC)

(defconstant GL_MATRIX29_ARB #x88DD)

(defconstant GL_MATRIX30_ARB #x88DE)

(defconstant GL_MATRIX31_ARB #x88DF)

(defconstant GL_FRAGMENT_PROGRAM_ARB #x8804)

(defconstant GL_PROGRAM_ALU_INSTRUCTIONS_ARB #x8805)

(defconstant GL_PROGRAM_TEX_INSTRUCTIONS_ARB #x8806)

(defconstant GL_PROGRAM_TEX_INDIRECTIONS_ARB #x8807)

(defconstant GL_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB #x8808)

(defconstant GL_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB #x8809)

(defconstant GL_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB #x880A)

(defconstant GL_MAX_PROGRAM_ALU_INSTRUCTIONS_ARB #x880B)

(defconstant GL_MAX_PROGRAM_TEX_INSTRUCTIONS_ARB #x880C)

(defconstant GL_MAX_PROGRAM_TEX_INDIRECTIONS_ARB #x880D)

(defconstant GL_MAX_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB #x880E)

(defconstant GL_MAX_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB #x880F)

(defconstant GL_MAX_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB #x8810)

(defconstant GL_MAX_TEXTURE_COORDS_ARB #x8871)

(defconstant GL_MAX_TEXTURE_IMAGE_UNITS_ARB #x8872)

(defconstant GL_BUFFER_SIZE_ARB #x8764)

(defconstant GL_BUFFER_USAGE_ARB #x8765)

(defconstant GL_ARRAY_BUFFER_ARB #x8892)

(defconstant GL_ELEMENT_ARRAY_BUFFER_ARB #x8893)

(defconstant GL_ARRAY_BUFFER_BINDING_ARB #x8894)

(defconstant GL_ELEMENT_ARRAY_BUFFER_BINDING_ARB #x8895)

(defconstant GL_VERTEX_ARRAY_BUFFER_BINDING_ARB #x8896)

(defconstant GL_NORMAL_ARRAY_BUFFER_BINDING_ARB #x8897)

(defconstant GL_COLOR_ARRAY_BUFFER_BINDING_ARB #x8898)

(defconstant GL_INDEX_ARRAY_BUFFER_BINDING_ARB #x8899)

(defconstant GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB #x889A)

(defconstant GL_EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB #x889B)

(defconstant GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB #x889C)

(defconstant GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB #x889D)

(defconstant GL_WEIGHT_ARRAY_BUFFER_BINDING_ARB #x889E)

(defconstant GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB #x889F)

(defconstant GL_READ_ONLY_ARB #x88B8)

(defconstant GL_WRITE_ONLY_ARB #x88B9)

(defconstant GL_READ_WRITE_ARB #x88BA)

(defconstant GL_BUFFER_ACCESS_ARB #x88BB)

(defconstant GL_BUFFER_MAPPED_ARB #x88BC)

(defconstant GL_BUFFER_MAP_POINTER_ARB #x88BD)

(defconstant GL_STREAM_DRAW_ARB #x88E0)

(defconstant GL_STREAM_READ_ARB #x88E1)

(defconstant GL_STREAM_COPY_ARB #x88E2)

(defconstant GL_STATIC_DRAW_ARB #x88E4)

(defconstant GL_STATIC_READ_ARB #x88E5)

(defconstant GL_STATIC_COPY_ARB #x88E6)

(defconstant GL_DYNAMIC_DRAW_ARB #x88E8)

(defconstant GL_DYNAMIC_READ_ARB #x88E9)

(defconstant GL_DYNAMIC_COPY_ARB #x88EA)

(defconstant GL_QUERY_COUNTER_BITS_ARB #x8864)

(defconstant GL_CURRENT_QUERY_ARB #x8865)

(defconstant GL_QUERY_RESULT_ARB #x8866)

(defconstant GL_QUERY_RESULT_AVAILABLE_ARB #x8867)

(defconstant GL_SAMPLES_PASSED_ARB #x8914)

(defconstant GL_PROGRAM_OBJECT_ARB #x8B40)

(defconstant GL_SHADER_OBJECT_ARB #x8B48)

(defconstant GL_OBJECT_TYPE_ARB #x8B4E)

(defconstant GL_OBJECT_SUBTYPE_ARB #x8B4F)

(defconstant GL_FLOAT_VEC2_ARB #x8B50)

(defconstant GL_FLOAT_VEC3_ARB #x8B51)

(defconstant GL_FLOAT_VEC4_ARB #x8B52)

(defconstant GL_INT_VEC2_ARB #x8B53)

(defconstant GL_INT_VEC3_ARB #x8B54)

(defconstant GL_INT_VEC4_ARB #x8B55)

(defconstant GL_BOOL_ARB #x8B56)

(defconstant GL_BOOL_VEC2_ARB #x8B57)

(defconstant GL_BOOL_VEC3_ARB #x8B58)

(defconstant GL_BOOL_VEC4_ARB #x8B59)

(defconstant GL_FLOAT_MAT2_ARB #x8B5A)

(defconstant GL_FLOAT_MAT3_ARB #x8B5B)

(defconstant GL_FLOAT_MAT4_ARB #x8B5C)

(defconstant GL_SAMPLER_1D_ARB #x8B5D)

(defconstant GL_SAMPLER_2D_ARB #x8B5E)

(defconstant GL_SAMPLER_3D_ARB #x8B5F)

(defconstant GL_SAMPLER_CUBE_ARB #x8B60)

(defconstant GL_SAMPLER_1D_SHADOW_ARB #x8B61)

(defconstant GL_SAMPLER_2D_SHADOW_ARB #x8B62)

(defconstant GL_SAMPLER_2D_RECT_ARB #x8B63)

(defconstant GL_SAMPLER_2D_RECT_SHADOW_ARB #x8B64)

(defconstant GL_OBJECT_DELETE_STATUS_ARB #x8B80)

(defconstant GL_OBJECT_COMPILE_STATUS_ARB #x8B81)

(defconstant GL_OBJECT_LINK_STATUS_ARB #x8B82)

(defconstant GL_OBJECT_VALIDATE_STATUS_ARB #x8B83)

(defconstant GL_OBJECT_INFO_LOG_LENGTH_ARB #x8B84)

(defconstant GL_OBJECT_ATTACHED_OBJECTS_ARB #x8B85)

(defconstant GL_OBJECT_ACTIVE_UNIFORMS_ARB #x8B86)

(defconstant GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB #x8B87)

(defconstant GL_OBJECT_SHADER_SOURCE_LENGTH_ARB #x8B88)

(defconstant GL_VERTEX_SHADER_ARB #x8B31)

(defconstant GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB #x8B4A)

(defconstant GL_MAX_VARYING_FLOATS_ARB #x8B4B)

(defconstant GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB #x8B4C)

(defconstant GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB #x8B4D)

(defconstant GL_OBJECT_ACTIVE_ATTRIBUTES_ARB #x8B89)

(defconstant GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB #x8B8A)

(defconstant GL_FRAGMENT_SHADER_ARB #x8B30)

(defconstant GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB #x8B49)

(defconstant GL_FRAGMENT_SHADER_DERIVATIVE_HINT_ARB #x8B8B)

(defconstant GL_SHADING_LANGUAGE_VERSION_ARB #x8B8C)

(defconstant GL_POINT_SPRITE_ARB #x8861)

(defconstant GL_COORD_REPLACE_ARB #x8862)

(defconstant GL_MAX_DRAW_BUFFERS_ARB #x8824)

(defconstant GL_DRAW_BUFFER0_ARB #x8825)

(defconstant GL_DRAW_BUFFER1_ARB #x8826)

(defconstant GL_DRAW_BUFFER2_ARB #x8827)

(defconstant GL_DRAW_BUFFER3_ARB #x8828)

(defconstant GL_DRAW_BUFFER4_ARB #x8829)

(defconstant GL_DRAW_BUFFER5_ARB #x882A)

(defconstant GL_DRAW_BUFFER6_ARB #x882B)

(defconstant GL_DRAW_BUFFER7_ARB #x882C)

(defconstant GL_DRAW_BUFFER8_ARB #x882D)

(defconstant GL_DRAW_BUFFER9_ARB #x882E)

(defconstant GL_DRAW_BUFFER10_ARB #x882F)

(defconstant GL_DRAW_BUFFER11_ARB #x8830)

(defconstant GL_DRAW_BUFFER12_ARB #x8831)

(defconstant GL_DRAW_BUFFER13_ARB #x8832)

(defconstant GL_DRAW_BUFFER14_ARB #x8833)

(defconstant GL_DRAW_BUFFER15_ARB #x8834)

(defconstant GL_TEXTURE_RECTANGLE_ARB #x84F5)

(defconstant GL_TEXTURE_BINDING_RECTANGLE_ARB #x84F6)

(defconstant GL_PROXY_TEXTURE_RECTANGLE_ARB #x84F7)

(defconstant GL_MAX_RECTANGLE_TEXTURE_SIZE_ARB #x84F8)

(defconstant GL_RGBA_FLOAT_MODE_ARB #x8820)

(defconstant GL_CLAMP_VERTEX_COLOR_ARB #x891A)

(defconstant GL_CLAMP_FRAGMENT_COLOR_ARB #x891B)

(defconstant GL_CLAMP_READ_COLOR_ARB #x891C)

(defconstant GL_FIXED_ONLY_ARB #x891D)

(defconstant GL_HALF_FLOAT_ARB #x140B)

(defconstant GL_TEXTURE_RED_TYPE_ARB #x8C10)

(defconstant GL_TEXTURE_GREEN_TYPE_ARB #x8C11)

(defconstant GL_TEXTURE_BLUE_TYPE_ARB #x8C12)

(defconstant GL_TEXTURE_ALPHA_TYPE_ARB #x8C13)

(defconstant GL_TEXTURE_LUMINANCE_TYPE_ARB #x8C14)

(defconstant GL_TEXTURE_INTENSITY_TYPE_ARB #x8C15)

(defconstant GL_TEXTURE_DEPTH_TYPE_ARB #x8C16)

(defconstant GL_UNSIGNED_NORMALIZED_ARB #x8C17)

(defconstant GL_RGBA32F_ARB #x8814)

(defconstant GL_RGB32F_ARB #x8815)

(defconstant GL_ALPHA32F_ARB #x8816)

(defconstant GL_INTENSITY32F_ARB #x8817)

(defconstant GL_LUMINANCE32F_ARB #x8818)

(defconstant GL_LUMINANCE_ALPHA32F_ARB #x8819)

(defconstant GL_RGBA16F_ARB #x881A)

(defconstant GL_RGB16F_ARB #x881B)

(defconstant GL_ALPHA16F_ARB #x881C)

(defconstant GL_INTENSITY16F_ARB #x881D)

(defconstant GL_LUMINANCE16F_ARB #x881E)

(defconstant GL_LUMINANCE_ALPHA16F_ARB #x881F)

(defconstant GL_PIXEL_PACK_BUFFER_ARB #x88EB)

(defconstant GL_PIXEL_UNPACK_BUFFER_ARB #x88EC)

(defconstant GL_PIXEL_PACK_BUFFER_BINDING_ARB #x88ED)

(defconstant GL_PIXEL_UNPACK_BUFFER_BINDING_ARB #x88EF)

(defconstant GL_ABGR_EXT #x8000)

(defconstant GL_CONSTANT_COLOR_EXT #x8001)

(defconstant GL_ONE_MINUS_CONSTANT_COLOR_EXT #x8002)

(defconstant GL_CONSTANT_ALPHA_EXT #x8003)

(defconstant GL_ONE_MINUS_CONSTANT_ALPHA_EXT #x8004)

(defconstant GL_BLEND_COLOR_EXT #x8005)

(defconstant GL_POLYGON_OFFSET_EXT #x8037)

(defconstant GL_POLYGON_OFFSET_FACTOR_EXT #x8038)

(defconstant GL_POLYGON_OFFSET_BIAS_EXT #x8039)

(defconstant GL_ALPHA4_EXT #x803B)

(defconstant GL_ALPHA8_EXT #x803C)

(defconstant GL_ALPHA12_EXT #x803D)

(defconstant GL_ALPHA16_EXT #x803E)

(defconstant GL_LUMINANCE4_EXT #x803F)

(defconstant GL_LUMINANCE8_EXT #x8040)

(defconstant GL_LUMINANCE12_EXT #x8041)

(defconstant GL_LUMINANCE16_EXT #x8042)

(defconstant GL_LUMINANCE4_ALPHA4_EXT #x8043)

(defconstant GL_LUMINANCE6_ALPHA2_EXT #x8044)

(defconstant GL_LUMINANCE8_ALPHA8_EXT #x8045)

(defconstant GL_LUMINANCE12_ALPHA4_EXT #x8046)

(defconstant GL_LUMINANCE12_ALPHA12_EXT #x8047)

(defconstant GL_LUMINANCE16_ALPHA16_EXT #x8048)

(defconstant GL_INTENSITY_EXT #x8049)

(defconstant GL_INTENSITY4_EXT #x804A)

(defconstant GL_INTENSITY8_EXT #x804B)

(defconstant GL_INTENSITY12_EXT #x804C)

(defconstant GL_INTENSITY16_EXT #x804D)

(defconstant GL_RGB2_EXT #x804E)

(defconstant GL_RGB4_EXT #x804F)

(defconstant GL_RGB5_EXT #x8050)

(defconstant GL_RGB8_EXT #x8051)

(defconstant GL_RGB10_EXT #x8052)

(defconstant GL_RGB12_EXT #x8053)

(defconstant GL_RGB16_EXT #x8054)

(defconstant GL_RGBA2_EXT #x8055)

(defconstant GL_RGBA4_EXT #x8056)

(defconstant GL_RGB5_A1_EXT #x8057)

(defconstant GL_RGBA8_EXT #x8058)

(defconstant GL_RGB10_A2_EXT #x8059)

(defconstant GL_RGBA12_EXT #x805A)

(defconstant GL_RGBA16_EXT #x805B)

(defconstant GL_TEXTURE_RED_SIZE_EXT #x805C)

(defconstant GL_TEXTURE_GREEN_SIZE_EXT #x805D)

(defconstant GL_TEXTURE_BLUE_SIZE_EXT #x805E)

(defconstant GL_TEXTURE_ALPHA_SIZE_EXT #x805F)

(defconstant GL_TEXTURE_LUMINANCE_SIZE_EXT #x8060)

(defconstant GL_TEXTURE_INTENSITY_SIZE_EXT #x8061)

(defconstant GL_REPLACE_EXT #x8062)

(defconstant GL_PROXY_TEXTURE_1D_EXT #x8063)

(defconstant GL_PROXY_TEXTURE_2D_EXT #x8064)

(defconstant GL_TEXTURE_TOO_LARGE_EXT #x8065)

(defconstant GL_PACK_SKIP_IMAGES_EXT #x806B)

(defconstant GL_PACK_IMAGE_HEIGHT_EXT #x806C)

(defconstant GL_UNPACK_SKIP_IMAGES_EXT #x806D)

(defconstant GL_UNPACK_IMAGE_HEIGHT_EXT #x806E)

(defconstant GL_TEXTURE_3D_EXT #x806F)

(defconstant GL_PROXY_TEXTURE_3D_EXT #x8070)

(defconstant GL_TEXTURE_DEPTH_EXT #x8071)

(defconstant GL_TEXTURE_WRAP_R_EXT #x8072)

(defconstant GL_MAX_3D_TEXTURE_SIZE_EXT #x8073)

(defconstant GL_FILTER4_SGIS #x8146)

(defconstant GL_TEXTURE_FILTER4_SIZE_SGIS #x8147)

(defconstant GL_HISTOGRAM_EXT #x8024)

(defconstant GL_PROXY_HISTOGRAM_EXT #x8025)

(defconstant GL_HISTOGRAM_WIDTH_EXT #x8026)

(defconstant GL_HISTOGRAM_FORMAT_EXT #x8027)

(defconstant GL_HISTOGRAM_RED_SIZE_EXT #x8028)

(defconstant GL_HISTOGRAM_GREEN_SIZE_EXT #x8029)

(defconstant GL_HISTOGRAM_BLUE_SIZE_EXT #x802A)

(defconstant GL_HISTOGRAM_ALPHA_SIZE_EXT #x802B)

(defconstant GL_HISTOGRAM_LUMINANCE_SIZE_EXT #x802C)

(defconstant GL_HISTOGRAM_SINK_EXT #x802D)

(defconstant GL_MINMAX_EXT #x802E)

(defconstant GL_MINMAX_FORMAT_EXT #x802F)

(defconstant GL_MINMAX_SINK_EXT #x8030)

(defconstant GL_TABLE_TOO_LARGE_EXT #x8031)

(defconstant GL_CONVOLUTION_1D_EXT #x8010)

(defconstant GL_CONVOLUTION_2D_EXT #x8011)

(defconstant GL_SEPARABLE_2D_EXT #x8012)

(defconstant GL_CONVOLUTION_BORDER_MODE_EXT #x8013)

(defconstant GL_CONVOLUTION_FILTER_SCALE_EXT #x8014)

(defconstant GL_CONVOLUTION_FILTER_BIAS_EXT #x8015)

(defconstant GL_REDUCE_EXT #x8016)

(defconstant GL_CONVOLUTION_FORMAT_EXT #x8017)

(defconstant GL_CONVOLUTION_WIDTH_EXT #x8018)

(defconstant GL_CONVOLUTION_HEIGHT_EXT #x8019)

(defconstant GL_MAX_CONVOLUTION_WIDTH_EXT #x801A)

(defconstant GL_MAX_CONVOLUTION_HEIGHT_EXT #x801B)

(defconstant GL_POST_CONVOLUTION_RED_SCALE_EXT #x801C)

(defconstant GL_POST_CONVOLUTION_GREEN_SCALE_EXT #x801D)

(defconstant GL_POST_CONVOLUTION_BLUE_SCALE_EXT #x801E)

(defconstant GL_POST_CONVOLUTION_ALPHA_SCALE_EXT #x801F)

(defconstant GL_POST_CONVOLUTION_RED_BIAS_EXT #x8020)

(defconstant GL_POST_CONVOLUTION_GREEN_BIAS_EXT #x8021)

(defconstant GL_POST_CONVOLUTION_BLUE_BIAS_EXT #x8022)

(defconstant GL_POST_CONVOLUTION_ALPHA_BIAS_EXT #x8023)

(defconstant GL_COLOR_MATRIX_SGI #x80B1)

(defconstant GL_COLOR_MATRIX_STACK_DEPTH_SGI #x80B2)

(defconstant GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI #x80B3)

(defconstant GL_POST_COLOR_MATRIX_RED_SCALE_SGI #x80B4)

(defconstant GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI #x80B5)

(defconstant GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI #x80B6)

(defconstant GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI #x80B7)

(defconstant GL_POST_COLOR_MATRIX_RED_BIAS_SGI #x80B8)

(defconstant GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI #x80B9)

(defconstant GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI #x80BA)

(defconstant GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI #x80BB)

(defconstant GL_COLOR_TABLE_SGI #x80D0)

(defconstant GL_POST_CONVOLUTION_COLOR_TABLE_SGI #x80D1)

(defconstant GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI #x80D2)

(defconstant GL_PROXY_COLOR_TABLE_SGI #x80D3)

(defconstant GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI #x80D4)

(defconstant GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI #x80D5)

(defconstant GL_COLOR_TABLE_SCALE_SGI #x80D6)

(defconstant GL_COLOR_TABLE_BIAS_SGI #x80D7)

(defconstant GL_COLOR_TABLE_FORMAT_SGI #x80D8)

(defconstant GL_COLOR_TABLE_WIDTH_SGI #x80D9)

(defconstant GL_COLOR_TABLE_RED_SIZE_SGI #x80DA)

(defconstant GL_COLOR_TABLE_GREEN_SIZE_SGI #x80DB)

(defconstant GL_COLOR_TABLE_BLUE_SIZE_SGI #x80DC)

(defconstant GL_COLOR_TABLE_ALPHA_SIZE_SGI #x80DD)

(defconstant GL_COLOR_TABLE_LUMINANCE_SIZE_SGI #x80DE)

(defconstant GL_COLOR_TABLE_INTENSITY_SIZE_SGI #x80DF)

(defconstant GL_PIXEL_TEXTURE_SGIS #x8353)

(defconstant GL_PIXEL_FRAGMENT_RGB_SOURCE_SGIS #x8354)

(defconstant GL_PIXEL_FRAGMENT_ALPHA_SOURCE_SGIS #x8355)

(defconstant GL_PIXEL_GROUP_COLOR_SGIS #x8356)

(defconstant GL_PIXEL_TEX_GEN_SGIX #x8139)

(defconstant GL_PIXEL_TEX_GEN_MODE_SGIX #x832B)

(defconstant GL_PACK_SKIP_VOLUMES_SGIS #x8130)

(defconstant GL_PACK_IMAGE_DEPTH_SGIS #x8131)

(defconstant GL_UNPACK_SKIP_VOLUMES_SGIS #x8132)

(defconstant GL_UNPACK_IMAGE_DEPTH_SGIS #x8133)

(defconstant GL_TEXTURE_4D_SGIS #x8134)

(defconstant GL_PROXY_TEXTURE_4D_SGIS #x8135)

(defconstant GL_TEXTURE_4DSIZE_SGIS #x8136)

(defconstant GL_TEXTURE_WRAP_Q_SGIS #x8137)

(defconstant GL_MAX_4D_TEXTURE_SIZE_SGIS #x8138)

(defconstant GL_TEXTURE_4D_BINDING_SGIS #x814F)

(defconstant GL_TEXTURE_COLOR_TABLE_SGI #x80BC)

(defconstant GL_PROXY_TEXTURE_COLOR_TABLE_SGI #x80BD)

(defconstant GL_CMYK_EXT #x800C)

(defconstant GL_CMYKA_EXT #x800D)

(defconstant GL_PACK_CMYK_HINT_EXT #x800E)

(defconstant GL_UNPACK_CMYK_HINT_EXT #x800F)

(defconstant GL_TEXTURE_PRIORITY_EXT #x8066)

(defconstant GL_TEXTURE_RESIDENT_EXT #x8067)

(defconstant GL_TEXTURE_1D_BINDING_EXT #x8068)

(defconstant GL_TEXTURE_2D_BINDING_EXT #x8069)

(defconstant GL_TEXTURE_3D_BINDING_EXT #x806A)

(defconstant GL_DETAIL_TEXTURE_2D_SGIS #x8095)

(defconstant GL_DETAIL_TEXTURE_2D_BINDING_SGIS #x8096)

(defconstant GL_LINEAR_DETAIL_SGIS #x8097)

(defconstant GL_LINEAR_DETAIL_ALPHA_SGIS #x8098)

(defconstant GL_LINEAR_DETAIL_COLOR_SGIS #x8099)

(defconstant GL_DETAIL_TEXTURE_LEVEL_SGIS #x809A)

(defconstant GL_DETAIL_TEXTURE_MODE_SGIS #x809B)

(defconstant GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS #x809C)

(defconstant GL_LINEAR_SHARPEN_SGIS #x80AD)

(defconstant GL_LINEAR_SHARPEN_ALPHA_SGIS #x80AE)

(defconstant GL_LINEAR_SHARPEN_COLOR_SGIS #x80AF)

(defconstant GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS #x80B0)

(defconstant GL_UNSIGNED_BYTE_3_3_2_EXT #x8032)

(defconstant GL_UNSIGNED_SHORT_4_4_4_4_EXT #x8033)

(defconstant GL_UNSIGNED_SHORT_5_5_5_1_EXT #x8034)

(defconstant GL_UNSIGNED_INT_8_8_8_8_EXT #x8035)

(defconstant GL_UNSIGNED_INT_10_10_10_2_EXT #x8036)

(defconstant GL_TEXTURE_MIN_LOD_SGIS #x813A)

(defconstant GL_TEXTURE_MAX_LOD_SGIS #x813B)

(defconstant GL_TEXTURE_BASE_LEVEL_SGIS #x813C)

(defconstant GL_TEXTURE_MAX_LEVEL_SGIS #x813D)

(defconstant GL_MULTISAMPLE_SGIS #x809D)

(defconstant GL_SAMPLE_ALPHA_TO_MASK_SGIS #x809E)

(defconstant GL_SAMPLE_ALPHA_TO_ONE_SGIS #x809F)

(defconstant GL_SAMPLE_MASK_SGIS #x80A0)

(defconstant GL_1PASS_SGIS #x80A1)

(defconstant GL_2PASS_0_SGIS #x80A2)

(defconstant GL_2PASS_1_SGIS #x80A3)

(defconstant GL_4PASS_0_SGIS #x80A4)

(defconstant GL_4PASS_1_SGIS #x80A5)

(defconstant GL_4PASS_2_SGIS #x80A6)

(defconstant GL_4PASS_3_SGIS #x80A7)

(defconstant GL_SAMPLE_BUFFERS_SGIS #x80A8)

(defconstant GL_SAMPLES_SGIS #x80A9)

(defconstant GL_SAMPLE_MASK_VALUE_SGIS #x80AA)

(defconstant GL_SAMPLE_MASK_INVERT_SGIS #x80AB)

(defconstant GL_SAMPLE_PATTERN_SGIS #x80AC)

(defconstant GL_RESCALE_NORMAL_EXT #x803A)

(defconstant GL_VERTEX_ARRAY_EXT #x8074)

(defconstant GL_NORMAL_ARRAY_EXT #x8075)

(defconstant GL_COLOR_ARRAY_EXT #x8076)

(defconstant GL_INDEX_ARRAY_EXT #x8077)

(defconstant GL_TEXTURE_COORD_ARRAY_EXT #x8078)

(defconstant GL_EDGE_FLAG_ARRAY_EXT #x8079)

(defconstant GL_VERTEX_ARRAY_SIZE_EXT #x807A)

(defconstant GL_VERTEX_ARRAY_TYPE_EXT #x807B)

(defconstant GL_VERTEX_ARRAY_STRIDE_EXT #x807C)

(defconstant GL_VERTEX_ARRAY_COUNT_EXT #x807D)

(defconstant GL_NORMAL_ARRAY_TYPE_EXT #x807E)

(defconstant GL_NORMAL_ARRAY_STRIDE_EXT #x807F)

(defconstant GL_NORMAL_ARRAY_COUNT_EXT #x8080)

(defconstant GL_COLOR_ARRAY_SIZE_EXT #x8081)

(defconstant GL_COLOR_ARRAY_TYPE_EXT #x8082)

(defconstant GL_COLOR_ARRAY_STRIDE_EXT #x8083)

(defconstant GL_COLOR_ARRAY_COUNT_EXT #x8084)

(defconstant GL_INDEX_ARRAY_TYPE_EXT #x8085)

(defconstant GL_INDEX_ARRAY_STRIDE_EXT #x8086)

(defconstant GL_INDEX_ARRAY_COUNT_EXT #x8087)

(defconstant GL_TEXTURE_COORD_ARRAY_SIZE_EXT #x8088)

(defconstant GL_TEXTURE_COORD_ARRAY_TYPE_EXT #x8089)

(defconstant GL_TEXTURE_COORD_ARRAY_STRIDE_EXT #x808A)

(defconstant GL_TEXTURE_COORD_ARRAY_COUNT_EXT #x808B)

(defconstant GL_EDGE_FLAG_ARRAY_STRIDE_EXT #x808C)

(defconstant GL_EDGE_FLAG_ARRAY_COUNT_EXT #x808D)

(defconstant GL_VERTEX_ARRAY_POINTER_EXT #x808E)

(defconstant GL_NORMAL_ARRAY_POINTER_EXT #x808F)

(defconstant GL_COLOR_ARRAY_POINTER_EXT #x8090)

(defconstant GL_INDEX_ARRAY_POINTER_EXT #x8091)

(defconstant GL_TEXTURE_COORD_ARRAY_POINTER_EXT #x8092)

(defconstant GL_EDGE_FLAG_ARRAY_POINTER_EXT #x8093)

(defconstant GL_GENERATE_MIPMAP_SGIS #x8191)

(defconstant GL_GENERATE_MIPMAP_HINT_SGIS #x8192)

(defconstant GL_LINEAR_CLIPMAP_LINEAR_SGIX #x8170)

(defconstant GL_TEXTURE_CLIPMAP_CENTER_SGIX #x8171)

(defconstant GL_TEXTURE_CLIPMAP_FRAME_SGIX #x8172)

(defconstant GL_TEXTURE_CLIPMAP_OFFSET_SGIX #x8173)

(defconstant GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX #x8174)

(defconstant GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX #x8175)

(defconstant GL_TEXTURE_CLIPMAP_DEPTH_SGIX #x8176)

(defconstant GL_MAX_CLIPMAP_DEPTH_SGIX #x8177)

(defconstant GL_MAX_CLIPMAP_VIRTUAL_DEPTH_SGIX #x8178)

(defconstant GL_NEAREST_CLIPMAP_NEAREST_SGIX #x844D)

(defconstant GL_NEAREST_CLIPMAP_LINEAR_SGIX #x844E)

(defconstant GL_LINEAR_CLIPMAP_NEAREST_SGIX #x844F)

(defconstant GL_TEXTURE_COMPARE_SGIX #x819A)

(defconstant GL_TEXTURE_COMPARE_OPERATOR_SGIX #x819B)

(defconstant GL_TEXTURE_LEQUAL_R_SGIX #x819C)

(defconstant GL_TEXTURE_GEQUAL_R_SGIX #x819D)

(defconstant GL_CLAMP_TO_EDGE_SGIS #x812F)

(defconstant GL_CLAMP_TO_BORDER_SGIS #x812D)

(defconstant GL_FUNC_ADD_EXT #x8006)

(defconstant GL_MIN_EXT #x8007)

(defconstant GL_MAX_EXT #x8008)

(defconstant GL_BLEND_EQUATION_EXT #x8009)

(defconstant GL_FUNC_SUBTRACT_EXT #x800A)

(defconstant GL_FUNC_REVERSE_SUBTRACT_EXT #x800B)

(defconstant GL_INTERLACE_SGIX #x8094)

(defconstant GL_PIXEL_TILE_BEST_ALIGNMENT_SGIX #x813E)

(defconstant GL_PIXEL_TILE_CACHE_INCREMENT_SGIX #x813F)

(defconstant GL_PIXEL_TILE_WIDTH_SGIX #x8140)

(defconstant GL_PIXEL_TILE_HEIGHT_SGIX #x8141)

(defconstant GL_PIXEL_TILE_GRID_WIDTH_SGIX #x8142)

(defconstant GL_PIXEL_TILE_GRID_HEIGHT_SGIX #x8143)

(defconstant GL_PIXEL_TILE_GRID_DEPTH_SGIX #x8144)

(defconstant GL_PIXEL_TILE_CACHE_SIZE_SGIX #x8145)

(defconstant GL_DUAL_ALPHA4_SGIS #x8110)

(defconstant GL_DUAL_ALPHA8_SGIS #x8111)

(defconstant GL_DUAL_ALPHA12_SGIS #x8112)

(defconstant GL_DUAL_ALPHA16_SGIS #x8113)

(defconstant GL_DUAL_LUMINANCE4_SGIS #x8114)

(defconstant GL_DUAL_LUMINANCE8_SGIS #x8115)

(defconstant GL_DUAL_LUMINANCE12_SGIS #x8116)

(defconstant GL_DUAL_LUMINANCE16_SGIS #x8117)

(defconstant GL_DUAL_INTENSITY4_SGIS #x8118)

(defconstant GL_DUAL_INTENSITY8_SGIS #x8119)

(defconstant GL_DUAL_INTENSITY12_SGIS #x811A)

(defconstant GL_DUAL_INTENSITY16_SGIS #x811B)

(defconstant GL_DUAL_LUMINANCE_ALPHA4_SGIS #x811C)

(defconstant GL_DUAL_LUMINANCE_ALPHA8_SGIS #x811D)

(defconstant GL_QUAD_ALPHA4_SGIS #x811E)

(defconstant GL_QUAD_ALPHA8_SGIS #x811F)

(defconstant GL_QUAD_LUMINANCE4_SGIS #x8120)

(defconstant GL_QUAD_LUMINANCE8_SGIS #x8121)

(defconstant GL_QUAD_INTENSITY4_SGIS #x8122)

(defconstant GL_QUAD_INTENSITY8_SGIS #x8123)

(defconstant GL_DUAL_TEXTURE_SELECT_SGIS #x8124)

(defconstant GL_QUAD_TEXTURE_SELECT_SGIS #x8125)

(defconstant GL_SPRITE_SGIX #x8148)

(defconstant GL_SPRITE_MODE_SGIX #x8149)

(defconstant GL_SPRITE_AXIS_SGIX #x814A)

(defconstant GL_SPRITE_TRANSLATION_SGIX #x814B)

(defconstant GL_SPRITE_AXIAL_SGIX #x814C)

(defconstant GL_SPRITE_OBJECT_ALIGNED_SGIX #x814D)

(defconstant GL_SPRITE_EYE_ALIGNED_SGIX #x814E)

(defconstant GL_TEXTURE_MULTI_BUFFER_HINT_SGIX #x812E)

(defconstant GL_POINT_SIZE_MIN_EXT #x8126)

(defconstant GL_POINT_SIZE_MAX_EXT #x8127)

(defconstant GL_POINT_FADE_THRESHOLD_SIZE_EXT #x8128)

(defconstant GL_DISTANCE_ATTENUATION_EXT #x8129)

(defconstant GL_POINT_SIZE_MIN_SGIS #x8126)

(defconstant GL_POINT_SIZE_MAX_SGIS #x8127)

(defconstant GL_POINT_FADE_THRESHOLD_SIZE_SGIS #x8128)

(defconstant GL_DISTANCE_ATTENUATION_SGIS #x8129)

(defconstant GL_INSTRUMENT_BUFFER_POINTER_SGIX #x8180)

(defconstant GL_INSTRUMENT_MEASUREMENTS_SGIX #x8181)

(defconstant GL_POST_TEXTURE_FILTER_BIAS_SGIX #x8179)

(defconstant GL_POST_TEXTURE_FILTER_SCALE_SGIX #x817A)

(defconstant GL_POST_TEXTURE_FILTER_BIAS_RANGE_SGIX #x817B)

(defconstant GL_POST_TEXTURE_FILTER_SCALE_RANGE_SGIX #x817C)

(defconstant GL_FRAMEZOOM_SGIX #x818B)

(defconstant GL_FRAMEZOOM_FACTOR_SGIX #x818C)

(defconstant GL_MAX_FRAMEZOOM_FACTOR_SGIX #x818D)

(defconstant GL_TEXTURE_DEFORMATION_BIT_SGIX #x00000001)

(defconstant GL_GEOMETRY_DEFORMATION_BIT_SGIX #x00000002)

(defconstant GL_GEOMETRY_DEFORMATION_SGIX #x8194)

(defconstant GL_TEXTURE_DEFORMATION_SGIX #x8195)

(defconstant GL_DEFORMATIONS_MASK_SGIX #x8196)

(defconstant GL_MAX_DEFORMATION_ORDER_SGIX #x8197)

(defconstant GL_REFERENCE_PLANE_SGIX #x817D)

(defconstant GL_REFERENCE_PLANE_EQUATION_SGIX #x817E)

(defconstant GL_DEPTH_COMPONENT16_SGIX #x81A5)

(defconstant GL_DEPTH_COMPONENT24_SGIX #x81A6)

(defconstant GL_DEPTH_COMPONENT32_SGIX #x81A7)

(defconstant GL_FOG_FUNC_SGIS #x812A)

(defconstant GL_FOG_FUNC_POINTS_SGIS #x812B)

(defconstant GL_MAX_FOG_FUNC_POINTS_SGIS #x812C)

(defconstant GL_FOG_OFFSET_SGIX #x8198)

(defconstant GL_FOG_OFFSET_VALUE_SGIX #x8199)

(defconstant GL_IMAGE_SCALE_X_HP #x8155)

(defconstant GL_IMAGE_SCALE_Y_HP #x8156)

(defconstant GL_IMAGE_TRANSLATE_X_HP #x8157)

(defconstant GL_IMAGE_TRANSLATE_Y_HP #x8158)

(defconstant GL_IMAGE_ROTATE_ANGLE_HP #x8159)

(defconstant GL_IMAGE_ROTATE_ORIGIN_X_HP #x815A)

(defconstant GL_IMAGE_ROTATE_ORIGIN_Y_HP #x815B)

(defconstant GL_IMAGE_MAG_FILTER_HP #x815C)

(defconstant GL_IMAGE_MIN_FILTER_HP #x815D)

(defconstant GL_IMAGE_CUBIC_WEIGHT_HP #x815E)

(defconstant GL_CUBIC_HP #x815F)

(defconstant GL_AVERAGE_HP #x8160)

(defconstant GL_IMAGE_TRANSFORM_2D_HP #x8161)

(defconstant GL_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP #x8162)

(defconstant GL_PROXY_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP #x8163)

(defconstant GL_IGNORE_BORDER_HP #x8150)

(defconstant GL_CONSTANT_BORDER_HP #x8151)

(defconstant GL_REPLICATE_BORDER_HP #x8153)

(defconstant GL_CONVOLUTION_BORDER_COLOR_HP #x8154)

(defconstant GL_TEXTURE_ENV_BIAS_SGIX #x80BE)

(defconstant GL_VERTEX_DATA_HINT_PGI #x1A22A)

(defconstant GL_VERTEX_CONSISTENT_HINT_PGI #x1A22B)

(defconstant GL_MATERIAL_SIDE_HINT_PGI #x1A22C)

(defconstant GL_MAX_VERTEX_HINT_PGI #x1A22D)

(defconstant GL_COLOR3_BIT_PGI #x00010000)

(defconstant GL_COLOR4_BIT_PGI #x00020000)

(defconstant GL_EDGEFLAG_BIT_PGI #x00040000)

(defconstant GL_INDEX_BIT_PGI #x00080000)

(defconstant GL_MAT_AMBIENT_BIT_PGI #x00100000)

(defconstant GL_MAT_AMBIENT_AND_DIFFUSE_BIT_PGI #x00200000)

(defconstant GL_MAT_DIFFUSE_BIT_PGI #x00400000)

(defconstant GL_MAT_EMISSION_BIT_PGI #x00800000)

(defconstant GL_MAT_COLOR_INDEXES_BIT_PGI #x01000000)

(defconstant GL_MAT_SHININESS_BIT_PGI #x02000000)

(defconstant GL_MAT_SPECULAR_BIT_PGI #x04000000)

(defconstant GL_NORMAL_BIT_PGI #x08000000)

(defconstant GL_TEXCOORD1_BIT_PGI #x10000000)

(defconstant GL_TEXCOORD2_BIT_PGI #x20000000)

(defconstant GL_TEXCOORD3_BIT_PGI #x40000000)

(defconstant GL_TEXCOORD4_BIT_PGI #x80000000)

(defconstant GL_VERTEX23_BIT_PGI #x00000004)

(defconstant GL_VERTEX4_BIT_PGI #x00000008)

(defconstant GL_PREFER_DOUBLEBUFFER_HINT_PGI #x1A1F8)

(defconstant GL_CONSERVE_MEMORY_HINT_PGI #x1A1FD)

(defconstant GL_RECLAIM_MEMORY_HINT_PGI #x1A1FE)

(defconstant GL_NATIVE_GRAPHICS_HANDLE_PGI #x1A202)

(defconstant GL_NATIVE_GRAPHICS_BEGIN_HINT_PGI #x1A203)

(defconstant GL_NATIVE_GRAPHICS_END_HINT_PGI #x1A204)

(defconstant GL_ALWAYS_FAST_HINT_PGI #x1A20C)

(defconstant GL_ALWAYS_SOFT_HINT_PGI #x1A20D)

(defconstant GL_ALLOW_DRAW_OBJ_HINT_PGI #x1A20E)

(defconstant GL_ALLOW_DRAW_WIN_HINT_PGI #x1A20F)

(defconstant GL_ALLOW_DRAW_FRG_HINT_PGI #x1A210)

(defconstant GL_ALLOW_DRAW_MEM_HINT_PGI #x1A211)

(defconstant GL_STRICT_DEPTHFUNC_HINT_PGI #x1A216)

(defconstant GL_STRICT_LIGHTING_HINT_PGI #x1A217)

(defconstant GL_STRICT_SCISSOR_HINT_PGI #x1A218)

(defconstant GL_FULL_STIPPLE_HINT_PGI #x1A219)

(defconstant GL_CLIP_NEAR_HINT_PGI #x1A220)

(defconstant GL_CLIP_FAR_HINT_PGI #x1A221)

(defconstant GL_WIDE_LINE_HINT_PGI #x1A222)

(defconstant GL_BACK_NORMALS_HINT_PGI #x1A223)

(defconstant GL_COLOR_INDEX1_EXT #x80E2)

(defconstant GL_COLOR_INDEX2_EXT #x80E3)

(defconstant GL_COLOR_INDEX4_EXT #x80E4)

(defconstant GL_COLOR_INDEX8_EXT #x80E5)

(defconstant GL_COLOR_INDEX12_EXT #x80E6)

(defconstant GL_COLOR_INDEX16_EXT #x80E7)

(defconstant GL_TEXTURE_INDEX_SIZE_EXT #x80ED)

(defconstant GL_CLIP_VOLUME_CLIPPING_HINT_EXT #x80F0)

(defconstant GL_LIST_PRIORITY_SGIX #x8182)

(defconstant GL_IR_INSTRUMENT1_SGIX #x817F)

(defconstant GL_CALLIGRAPHIC_FRAGMENT_SGIX #x8183)

(defconstant GL_TEXTURE_LOD_BIAS_S_SGIX #x818E)

(defconstant GL_TEXTURE_LOD_BIAS_T_SGIX #x818F)

(defconstant GL_TEXTURE_LOD_BIAS_R_SGIX #x8190)

(defconstant GL_SHADOW_AMBIENT_SGIX #x80BF)

(defconstant GL_INDEX_MATERIAL_EXT #x81B8)

(defconstant GL_INDEX_MATERIAL_PARAMETER_EXT #x81B9)

(defconstant GL_INDEX_MATERIAL_FACE_EXT #x81BA)

(defconstant GL_INDEX_TEST_EXT #x81B5)

(defconstant GL_INDEX_TEST_FUNC_EXT #x81B6)

(defconstant GL_INDEX_TEST_REF_EXT #x81B7)

(defconstant GL_IUI_V2F_EXT #x81AD)

(defconstant GL_IUI_V3F_EXT #x81AE)

(defconstant GL_IUI_N3F_V2F_EXT #x81AF)

(defconstant GL_IUI_N3F_V3F_EXT #x81B0)

(defconstant GL_T2F_IUI_V2F_EXT #x81B1)

(defconstant GL_T2F_IUI_V3F_EXT #x81B2)

(defconstant GL_T2F_IUI_N3F_V2F_EXT #x81B3)

(defconstant GL_T2F_IUI_N3F_V3F_EXT #x81B4)

(defconstant GL_ARRAY_ELEMENT_LOCK_FIRST_EXT #x81A8)

(defconstant GL_ARRAY_ELEMENT_LOCK_COUNT_EXT #x81A9)

(defconstant GL_CULL_VERTEX_EXT #x81AA)

(defconstant GL_CULL_VERTEX_EYE_POSITION_EXT #x81AB)

(defconstant GL_CULL_VERTEX_OBJECT_POSITION_EXT #x81AC)

(defconstant GL_YCRCB_422_SGIX #x81BB)

(defconstant GL_YCRCB_444_SGIX #x81BC)

(defconstant GL_FRAGMENT_LIGHTING_SGIX #x8400)

(defconstant GL_FRAGMENT_COLOR_MATERIAL_SGIX #x8401)

(defconstant GL_FRAGMENT_COLOR_MATERIAL_FACE_SGIX #x8402)

(defconstant GL_FRAGMENT_COLOR_MATERIAL_PARAMETER_SGIX #x8403)

(defconstant GL_MAX_FRAGMENT_LIGHTS_SGIX #x8404)

(defconstant GL_MAX_ACTIVE_LIGHTS_SGIX #x8405)

(defconstant GL_CURRENT_RASTER_NORMAL_SGIX #x8406)

(defconstant GL_LIGHT_ENV_MODE_SGIX #x8407)

(defconstant GL_FRAGMENT_LIGHT_MODEL_LOCAL_VIEWER_SGIX #x8408)

(defconstant GL_FRAGMENT_LIGHT_MODEL_TWO_SIDE_SGIX #x8409)

(defconstant GL_FRAGMENT_LIGHT_MODEL_AMBIENT_SGIX #x840A)

(defconstant GL_FRAGMENT_LIGHT_MODEL_NORMAL_INTERPOLATION_SGIX #x840B)

(defconstant GL_FRAGMENT_LIGHT0_SGIX #x840C)

(defconstant GL_FRAGMENT_LIGHT1_SGIX #x840D)

(defconstant GL_FRAGMENT_LIGHT2_SGIX #x840E)

(defconstant GL_FRAGMENT_LIGHT3_SGIX #x840F)

(defconstant GL_FRAGMENT_LIGHT4_SGIX #x8410)

(defconstant GL_FRAGMENT_LIGHT5_SGIX #x8411)

(defconstant GL_FRAGMENT_LIGHT6_SGIX #x8412)

(defconstant GL_FRAGMENT_LIGHT7_SGIX #x8413)

(defconstant GL_RASTER_POSITION_UNCLIPPED_IBM #x19262)

(defconstant GL_TEXTURE_LIGHTING_MODE_HP #x8167)

(defconstant GL_TEXTURE_POST_SPECULAR_HP #x8168)

(defconstant GL_TEXTURE_PRE_SPECULAR_HP #x8169)

(defconstant GL_MAX_ELEMENTS_VERTICES_EXT #x80E8)

(defconstant GL_MAX_ELEMENTS_INDICES_EXT #x80E9)

(defconstant GL_PHONG_WIN #x80EA)

(defconstant GL_PHONG_HINT_WIN #x80EB)

(defconstant GL_FOG_SPECULAR_TEXTURE_WIN #x80EC)

(defconstant GL_FRAGMENT_MATERIAL_EXT #x8349)

(defconstant GL_FRAGMENT_NORMAL_EXT #x834A)

(defconstant GL_FRAGMENT_COLOR_EXT #x834C)

(defconstant GL_ATTENUATION_EXT #x834D)

(defconstant GL_SHADOW_ATTENUATION_EXT #x834E)

(defconstant GL_TEXTURE_APPLICATION_MODE_EXT #x834F)

(defconstant GL_TEXTURE_LIGHT_EXT #x8350)

(defconstant GL_TEXTURE_MATERIAL_FACE_EXT #x8351)

(defconstant GL_TEXTURE_MATERIAL_PARAMETER_EXT #x8352)

(defconstant GL_ALPHA_MIN_SGIX #x8320)

(defconstant GL_ALPHA_MAX_SGIX #x8321)

(defconstant GL_PIXEL_TEX_GEN_Q_CEILING_SGIX #x8184)

(defconstant GL_PIXEL_TEX_GEN_Q_ROUND_SGIX #x8185)

(defconstant GL_PIXEL_TEX_GEN_Q_FLOOR_SGIX #x8186)

(defconstant GL_PIXEL_TEX_GEN_ALPHA_REPLACE_SGIX #x8187)

(defconstant GL_PIXEL_TEX_GEN_ALPHA_NO_REPLACE_SGIX #x8188)

(defconstant GL_PIXEL_TEX_GEN_ALPHA_LS_SGIX #x8189)

(defconstant GL_PIXEL_TEX_GEN_ALPHA_MS_SGIX #x818A)

(defconstant GL_BGR_EXT #x80E0)

(defconstant GL_BGRA_EXT #x80E1)

(defconstant GL_ASYNC_MARKER_SGIX #x8329)

(defconstant GL_ASYNC_TEX_IMAGE_SGIX #x835C)

(defconstant GL_ASYNC_DRAW_PIXELS_SGIX #x835D)

(defconstant GL_ASYNC_READ_PIXELS_SGIX #x835E)

(defconstant GL_MAX_ASYNC_TEX_IMAGE_SGIX #x835F)

(defconstant GL_MAX_ASYNC_DRAW_PIXELS_SGIX #x8360)

(defconstant GL_MAX_ASYNC_READ_PIXELS_SGIX #x8361)

(defconstant GL_ASYNC_HISTOGRAM_SGIX #x832C)

(defconstant GL_MAX_ASYNC_HISTOGRAM_SGIX #x832D)

(defconstant GL_PARALLEL_ARRAYS_INTEL #x83F4)

(defconstant GL_VERTEX_ARRAY_PARALLEL_POINTERS_INTEL #x83F5)

(defconstant GL_NORMAL_ARRAY_PARALLEL_POINTERS_INTEL #x83F6)

(defconstant GL_COLOR_ARRAY_PARALLEL_POINTERS_INTEL #x83F7)

(defconstant GL_TEXTURE_COORD_ARRAY_PARALLEL_POINTERS_INTEL #x83F8)

(defconstant GL_OCCLUSION_TEST_HP #x8165)

(defconstant GL_OCCLUSION_TEST_RESULT_HP #x8166)

(defconstant GL_PIXEL_TRANSFORM_2D_EXT #x8330)

(defconstant GL_PIXEL_MAG_FILTER_EXT #x8331)

(defconstant GL_PIXEL_MIN_FILTER_EXT #x8332)

(defconstant GL_PIXEL_CUBIC_WEIGHT_EXT #x8333)

(defconstant GL_CUBIC_EXT #x8334)

(defconstant GL_AVERAGE_EXT #x8335)

(defconstant GL_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT #x8336)

(defconstant GL_MAX_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT #x8337)

(defconstant GL_PIXEL_TRANSFORM_2D_MATRIX_EXT #x8338)

(defconstant GL_SHARED_TEXTURE_PALETTE_EXT #x81FB)

(defconstant GL_LIGHT_MODEL_COLOR_CONTROL_EXT #x81F8)

(defconstant GL_SINGLE_COLOR_EXT #x81F9)

(defconstant GL_SEPARATE_SPECULAR_COLOR_EXT #x81FA)

(defconstant GL_COLOR_SUM_EXT #x8458)

(defconstant GL_CURRENT_SECONDARY_COLOR_EXT #x8459)

(defconstant GL_SECONDARY_COLOR_ARRAY_SIZE_EXT #x845A)

(defconstant GL_SECONDARY_COLOR_ARRAY_TYPE_EXT #x845B)

(defconstant GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT #x845C)

(defconstant GL_SECONDARY_COLOR_ARRAY_POINTER_EXT #x845D)

(defconstant GL_SECONDARY_COLOR_ARRAY_EXT #x845E)

(defconstant GL_PERTURB_EXT #x85AE)

(defconstant GL_TEXTURE_NORMAL_EXT #x85AF)

(defconstant GL_FOG_COORDINATE_SOURCE_EXT #x8450)

(defconstant GL_FOG_COORDINATE_EXT #x8451)

(defconstant GL_FRAGMENT_DEPTH_EXT #x8452)

(defconstant GL_CURRENT_FOG_COORDINATE_EXT #x8453)

(defconstant GL_FOG_COORDINATE_ARRAY_TYPE_EXT #x8454)

(defconstant GL_FOG_COORDINATE_ARRAY_STRIDE_EXT #x8455)

(defconstant GL_FOG_COORDINATE_ARRAY_POINTER_EXT #x8456)

(defconstant GL_FOG_COORDINATE_ARRAY_EXT #x8457)

(defconstant GL_SCREEN_COORDINATES_REND #x8490)

(defconstant GL_INVERTED_SCREEN_W_REND #x8491)

(defconstant GL_TANGENT_ARRAY_EXT #x8439)

(defconstant GL_BINORMAL_ARRAY_EXT #x843A)

(defconstant GL_CURRENT_TANGENT_EXT #x843B)

(defconstant GL_CURRENT_BINORMAL_EXT #x843C)

(defconstant GL_TANGENT_ARRAY_TYPE_EXT #x843E)

(defconstant GL_TANGENT_ARRAY_STRIDE_EXT #x843F)

(defconstant GL_BINORMAL_ARRAY_TYPE_EXT #x8440)

(defconstant GL_BINORMAL_ARRAY_STRIDE_EXT #x8441)

(defconstant GL_TANGENT_ARRAY_POINTER_EXT #x8442)

(defconstant GL_BINORMAL_ARRAY_POINTER_EXT #x8443)

(defconstant GL_MAP1_TANGENT_EXT #x8444)

(defconstant GL_MAP2_TANGENT_EXT #x8445)

(defconstant GL_MAP1_BINORMAL_EXT #x8446)

(defconstant GL_MAP2_BINORMAL_EXT #x8447)

(defconstant GL_COMBINE_EXT #x8570)

(defconstant GL_COMBINE_RGB_EXT #x8571)

(defconstant GL_COMBINE_ALPHA_EXT #x8572)

(defconstant GL_RGB_SCALE_EXT #x8573)

(defconstant GL_ADD_SIGNED_EXT #x8574)

(defconstant GL_INTERPOLATE_EXT #x8575)

(defconstant GL_CONSTANT_EXT #x8576)

(defconstant GL_PRIMARY_COLOR_EXT #x8577)

(defconstant GL_PREVIOUS_EXT #x8578)

(defconstant GL_SOURCE0_RGB_EXT #x8580)

(defconstant GL_SOURCE1_RGB_EXT #x8581)

(defconstant GL_SOURCE2_RGB_EXT #x8582)

(defconstant GL_SOURCE0_ALPHA_EXT #x8588)

(defconstant GL_SOURCE1_ALPHA_EXT #x8589)

(defconstant GL_SOURCE2_ALPHA_EXT #x858A)

(defconstant GL_OPERAND0_RGB_EXT #x8590)

(defconstant GL_OPERAND1_RGB_EXT #x8591)

(defconstant GL_OPERAND2_RGB_EXT #x8592)

(defconstant GL_OPERAND0_ALPHA_EXT #x8598)

(defconstant GL_OPERAND1_ALPHA_EXT #x8599)

(defconstant GL_OPERAND2_ALPHA_EXT #x859A)

(defconstant GL_LIGHT_MODEL_SPECULAR_VECTOR_APPLE #x85B0)

(defconstant GL_TRANSFORM_HINT_APPLE #x85B1)

(defconstant GL_FOG_SCALE_SGIX #x81FC)

(defconstant GL_FOG_SCALE_VALUE_SGIX #x81FD)

(defconstant GL_UNPACK_CONSTANT_DATA_SUNX #x81D5)

(defconstant GL_TEXTURE_CONSTANT_DATA_SUNX #x81D6)

(defconstant GL_GLOBAL_ALPHA_SUN #x81D9)

(defconstant GL_GLOBAL_ALPHA_FACTOR_SUN #x81DA)

(defconstant GL_RESTART_SUN #x0001)

(defconstant GL_REPLACE_MIDDLE_SUN #x0002)

(defconstant GL_REPLACE_OLDEST_SUN #x0003)

(defconstant GL_TRIANGLE_LIST_SUN #x81D7)

(defconstant GL_REPLACEMENT_CODE_SUN #x81D8)

(defconstant GL_REPLACEMENT_CODE_ARRAY_SUN #x85C0)

(defconstant GL_REPLACEMENT_CODE_ARRAY_TYPE_SUN #x85C1)

(defconstant GL_REPLACEMENT_CODE_ARRAY_STRIDE_SUN #x85C2)

(defconstant GL_REPLACEMENT_CODE_ARRAY_POINTER_SUN #x85C3)

(defconstant GL_R1UI_V3F_SUN #x85C4)

(defconstant GL_R1UI_C4UB_V3F_SUN #x85C5)

(defconstant GL_R1UI_C3F_V3F_SUN #x85C6)

(defconstant GL_R1UI_N3F_V3F_SUN #x85C7)

(defconstant GL_R1UI_C4F_N3F_V3F_SUN #x85C8)

(defconstant GL_R1UI_T2F_V3F_SUN #x85C9)

(defconstant GL_R1UI_T2F_N3F_V3F_SUN #x85CA)

(defconstant GL_R1UI_T2F_C4F_N3F_V3F_SUN #x85CB)

(defconstant GL_BLEND_DST_RGB_EXT #x80C8)

(defconstant GL_BLEND_SRC_RGB_EXT #x80C9)

(defconstant GL_BLEND_DST_ALPHA_EXT #x80CA)

(defconstant GL_BLEND_SRC_ALPHA_EXT #x80CB)

(defconstant GL_RED_MIN_CLAMP_INGR #x8560)

(defconstant GL_GREEN_MIN_CLAMP_INGR #x8561)

(defconstant GL_BLUE_MIN_CLAMP_INGR #x8562)

(defconstant GL_ALPHA_MIN_CLAMP_INGR #x8563)

(defconstant GL_RED_MAX_CLAMP_INGR #x8564)

(defconstant GL_GREEN_MAX_CLAMP_INGR #x8565)

(defconstant GL_BLUE_MAX_CLAMP_INGR #x8566)

(defconstant GL_ALPHA_MAX_CLAMP_INGR #x8567)

(defconstant GL_INTERLACE_READ_INGR #x8568)

(defconstant GL_INCR_WRAP_EXT #x8507)

(defconstant GL_DECR_WRAP_EXT #x8508)

(defconstant GL_422_EXT #x80CC)

(defconstant GL_422_REV_EXT #x80CD)

(defconstant GL_422_AVERAGE_EXT #x80CE)

(defconstant GL_422_REV_AVERAGE_EXT #x80CF)

(defconstant GL_NORMAL_MAP_NV #x8511)

(defconstant GL_REFLECTION_MAP_NV #x8512)

(defconstant GL_NORMAL_MAP_EXT #x8511)

(defconstant GL_REFLECTION_MAP_EXT #x8512)

(defconstant GL_TEXTURE_CUBE_MAP_EXT #x8513)

(defconstant GL_TEXTURE_BINDING_CUBE_MAP_EXT #x8514)

(defconstant GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT #x8515)

(defconstant GL_TEXTURE_CUBE_MAP_NEGATIVE_X_EXT #x8516)

(defconstant GL_TEXTURE_CUBE_MAP_POSITIVE_Y_EXT #x8517)

(defconstant GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_EXT #x8518)

(defconstant GL_TEXTURE_CUBE_MAP_POSITIVE_Z_EXT #x8519)

(defconstant GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_EXT #x851A)

(defconstant GL_PROXY_TEXTURE_CUBE_MAP_EXT #x851B)

(defconstant GL_MAX_CUBE_MAP_TEXTURE_SIZE_EXT #x851C)

(defconstant GL_WRAP_BORDER_SUN #x81D4)

(defconstant GL_MAX_TEXTURE_LOD_BIAS_EXT #x84FD)

(defconstant GL_TEXTURE_FILTER_CONTROL_EXT #x8500)

(defconstant GL_TEXTURE_LOD_BIAS_EXT #x8501)

(defconstant GL_TEXTURE_MAX_ANISOTROPY_EXT #x84FE)

(defconstant GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT #x84FF)

(defconstant GL_MODELVIEW0_STACK_DEPTH_EXT #x0BA3)

(defconstant GL_MODELVIEW1_STACK_DEPTH_EXT #x8502)

(defconstant GL_MODELVIEW0_MATRIX_EXT #x0BA6)

(defconstant GL_MODELVIEW1_MATRIX_EXT #x8506)

(defconstant GL_VERTEX_WEIGHTING_EXT #x8509)

(defconstant GL_MODELVIEW0_EXT #x1700)

(defconstant GL_MODELVIEW1_EXT #x850A)

(defconstant GL_CURRENT_VERTEX_WEIGHT_EXT #x850B)

(defconstant GL_VERTEX_WEIGHT_ARRAY_EXT #x850C)

(defconstant GL_VERTEX_WEIGHT_ARRAY_SIZE_EXT #x850D)

(defconstant GL_VERTEX_WEIGHT_ARRAY_TYPE_EXT #x850E)

(defconstant GL_VERTEX_WEIGHT_ARRAY_STRIDE_EXT #x850F)

(defconstant GL_VERTEX_WEIGHT_ARRAY_POINTER_EXT #x8510)

(defconstant GL_MAX_SHININESS_NV #x8504)

(defconstant GL_MAX_SPOT_EXPONENT_NV #x8505)

(defconstant GL_VERTEX_ARRAY_RANGE_NV #x851D)

(defconstant GL_VERTEX_ARRAY_RANGE_LENGTH_NV #x851E)

(defconstant GL_VERTEX_ARRAY_RANGE_VALID_NV #x851F)

(defconstant GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV #x8520)

(defconstant GL_VERTEX_ARRAY_RANGE_POINTER_NV #x8521)

(defconstant GL_REGISTER_COMBINERS_NV #x8522)

(defconstant GL_VARIABLE_A_NV #x8523)

(defconstant GL_VARIABLE_B_NV #x8524)

(defconstant GL_VARIABLE_C_NV #x8525)

(defconstant GL_VARIABLE_D_NV #x8526)

(defconstant GL_VARIABLE_E_NV #x8527)

(defconstant GL_VARIABLE_F_NV #x8528)

(defconstant GL_VARIABLE_G_NV #x8529)

(defconstant GL_CONSTANT_COLOR0_NV #x852A)

(defconstant GL_CONSTANT_COLOR1_NV #x852B)

(defconstant GL_PRIMARY_COLOR_NV #x852C)

(defconstant GL_SECONDARY_COLOR_NV #x852D)

(defconstant GL_SPARE0_NV #x852E)

(defconstant GL_SPARE1_NV #x852F)

(defconstant GL_DISCARD_NV #x8530)

(defconstant GL_E_TIMES_F_NV #x8531)

(defconstant GL_SPARE0_PLUS_SECONDARY_COLOR_NV #x8532)

(defconstant GL_UNSIGNED_IDENTITY_NV #x8536)

(defconstant GL_UNSIGNED_INVERT_NV #x8537)

(defconstant GL_EXPAND_NORMAL_NV #x8538)

(defconstant GL_EXPAND_NEGATE_NV #x8539)

(defconstant GL_HALF_BIAS_NORMAL_NV #x853A)

(defconstant GL_HALF_BIAS_NEGATE_NV #x853B)

(defconstant GL_SIGNED_IDENTITY_NV #x853C)

(defconstant GL_SIGNED_NEGATE_NV #x853D)

(defconstant GL_SCALE_BY_TWO_NV #x853E)

(defconstant GL_SCALE_BY_FOUR_NV #x853F)

(defconstant GL_SCALE_BY_ONE_HALF_NV #x8540)

(defconstant GL_BIAS_BY_NEGATIVE_ONE_HALF_NV #x8541)

(defconstant GL_COMBINER_INPUT_NV #x8542)

(defconstant GL_COMBINER_MAPPING_NV #x8543)

(defconstant GL_COMBINER_COMPONENT_USAGE_NV #x8544)

(defconstant GL_COMBINER_AB_DOT_PRODUCT_NV #x8545)

(defconstant GL_COMBINER_CD_DOT_PRODUCT_NV #x8546)

(defconstant GL_COMBINER_MUX_SUM_NV #x8547)

(defconstant GL_COMBINER_SCALE_NV #x8548)

(defconstant GL_COMBINER_BIAS_NV #x8549)

(defconstant GL_COMBINER_AB_OUTPUT_NV #x854A)

(defconstant GL_COMBINER_CD_OUTPUT_NV #x854B)

(defconstant GL_COMBINER_SUM_OUTPUT_NV #x854C)

(defconstant GL_MAX_GENERAL_COMBINERS_NV #x854D)

(defconstant GL_NUM_GENERAL_COMBINERS_NV #x854E)

(defconstant GL_COLOR_SUM_CLAMP_NV #x854F)

(defconstant GL_COMBINER0_NV #x8550)

(defconstant GL_COMBINER1_NV #x8551)

(defconstant GL_COMBINER2_NV #x8552)

(defconstant GL_COMBINER3_NV #x8553)

(defconstant GL_COMBINER4_NV #x8554)

(defconstant GL_COMBINER5_NV #x8555)

(defconstant GL_COMBINER6_NV #x8556)

(defconstant GL_COMBINER7_NV #x8557)

(defconstant GL_FOG_DISTANCE_MODE_NV #x855A)

(defconstant GL_EYE_RADIAL_NV #x855B)

(defconstant GL_EYE_PLANE_ABSOLUTE_NV #x855C)

(defconstant GL_EMBOSS_LIGHT_NV #x855D)

(defconstant GL_EMBOSS_CONSTANT_NV #x855E)

(defconstant GL_EMBOSS_MAP_NV #x855F)

(defconstant GL_COMBINE4_NV #x8503)

(defconstant GL_SOURCE3_RGB_NV #x8583)

(defconstant GL_SOURCE3_ALPHA_NV #x858B)

(defconstant GL_OPERAND3_RGB_NV #x8593)

(defconstant GL_OPERAND3_ALPHA_NV #x859B)

(defconstant GL_COMPRESSED_RGB_S3TC_DXT1_EXT #x83F0)

(defconstant GL_COMPRESSED_RGBA_S3TC_DXT1_EXT #x83F1)

(defconstant GL_COMPRESSED_RGBA_S3TC_DXT3_EXT #x83F2)

(defconstant GL_COMPRESSED_RGBA_S3TC_DXT5_EXT #x83F3)

(defconstant GL_CULL_VERTEX_IBM 103050)

(defconstant GL_VERTEX_ARRAY_LIST_IBM 103070)

(defconstant GL_NORMAL_ARRAY_LIST_IBM 103071)

(defconstant GL_COLOR_ARRAY_LIST_IBM 103072)

(defconstant GL_INDEX_ARRAY_LIST_IBM 103073)

(defconstant GL_TEXTURE_COORD_ARRAY_LIST_IBM 103074)

(defconstant GL_EDGE_FLAG_ARRAY_LIST_IBM 103075)

(defconstant GL_FOG_COORDINATE_ARRAY_LIST_IBM 103076)

(defconstant GL_SECONDARY_COLOR_ARRAY_LIST_IBM 103077)

(defconstant GL_VERTEX_ARRAY_LIST_STRIDE_IBM 103080)

(defconstant GL_NORMAL_ARRAY_LIST_STRIDE_IBM 103081)

(defconstant GL_COLOR_ARRAY_LIST_STRIDE_IBM 103082)

(defconstant GL_INDEX_ARRAY_LIST_STRIDE_IBM 103083)

(defconstant GL_TEXTURE_COORD_ARRAY_LIST_STRIDE_IBM 103084)

(defconstant GL_EDGE_FLAG_ARRAY_LIST_STRIDE_IBM 103085)

(defconstant GL_FOG_COORDINATE_ARRAY_LIST_STRIDE_IBM 103086)

(defconstant GL_SECONDARY_COLOR_ARRAY_LIST_STRIDE_IBM 103087)

(defconstant GL_PACK_SUBSAMPLE_RATE_SGIX #x85A0)

(defconstant GL_UNPACK_SUBSAMPLE_RATE_SGIX #x85A1)

(defconstant GL_PIXEL_SUBSAMPLE_4444_SGIX #x85A2)

(defconstant GL_PIXEL_SUBSAMPLE_2424_SGIX #x85A3)

(defconstant GL_PIXEL_SUBSAMPLE_4242_SGIX #x85A4)

(defconstant GL_YCRCB_SGIX #x8318)

(defconstant GL_YCRCBA_SGIX #x8319)

(defconstant GL_DEPTH_PASS_INSTRUMENT_SGIX #x8310)

(defconstant GL_DEPTH_PASS_INSTRUMENT_COUNTERS_SGIX #x8311)

(defconstant GL_DEPTH_PASS_INSTRUMENT_MAX_SGIX #x8312)

(defconstant GL_COMPRESSED_RGB_FXT1_3DFX #x86B0)

(defconstant GL_COMPRESSED_RGBA_FXT1_3DFX #x86B1)

(defconstant GL_MULTISAMPLE_3DFX #x86B2)

(defconstant GL_SAMPLE_BUFFERS_3DFX #x86B3)

(defconstant GL_SAMPLES_3DFX #x86B4)

(defconstant GL_MULTISAMPLE_BIT_3DFX #x20000000)

(defconstant GL_MULTISAMPLE_EXT #x809D)

(defconstant GL_SAMPLE_ALPHA_TO_MASK_EXT #x809E)

(defconstant GL_SAMPLE_ALPHA_TO_ONE_EXT #x809F)

(defconstant GL_SAMPLE_MASK_EXT #x80A0)

(defconstant GL_1PASS_EXT #x80A1)

(defconstant GL_2PASS_0_EXT #x80A2)

(defconstant GL_2PASS_1_EXT #x80A3)

(defconstant GL_4PASS_0_EXT #x80A4)

(defconstant GL_4PASS_1_EXT #x80A5)

(defconstant GL_4PASS_2_EXT #x80A6)

(defconstant GL_4PASS_3_EXT #x80A7)

(defconstant GL_SAMPLE_BUFFERS_EXT #x80A8)

(defconstant GL_SAMPLES_EXT #x80A9)

(defconstant GL_SAMPLE_MASK_VALUE_EXT #x80AA)

(defconstant GL_SAMPLE_MASK_INVERT_EXT #x80AB)

(defconstant GL_SAMPLE_PATTERN_EXT #x80AC)

(defconstant GL_MULTISAMPLE_BIT_EXT #x20000000)

(defconstant GL_VERTEX_PRECLIP_SGIX #x83EE)

(defconstant GL_VERTEX_PRECLIP_HINT_SGIX #x83EF)

(defconstant GL_CONVOLUTION_HINT_SGIX #x8316)

(defconstant GL_PACK_RESAMPLE_SGIX #x842C)

(defconstant GL_UNPACK_RESAMPLE_SGIX #x842D)

(defconstant GL_RESAMPLE_REPLICATE_SGIX #x842E)

(defconstant GL_RESAMPLE_ZERO_FILL_SGIX #x842F)

(defconstant GL_RESAMPLE_DECIMATE_SGIX #x8430)

(defconstant GL_EYE_DISTANCE_TO_POINT_SGIS #x81F0)

(defconstant GL_OBJECT_DISTANCE_TO_POINT_SGIS #x81F1)

(defconstant GL_EYE_DISTANCE_TO_LINE_SGIS #x81F2)

(defconstant GL_OBJECT_DISTANCE_TO_LINE_SGIS #x81F3)

(defconstant GL_EYE_POINT_SGIS #x81F4)

(defconstant GL_OBJECT_POINT_SGIS #x81F5)

(defconstant GL_EYE_LINE_SGIS #x81F6)

(defconstant GL_OBJECT_LINE_SGIS #x81F7)

(defconstant GL_TEXTURE_COLOR_WRITEMASK_SGIS #x81EF)

(defconstant GL_DOT3_RGB_EXT #x8740)

(defconstant GL_DOT3_RGBA_EXT #x8741)

(defconstant GL_MIRROR_CLAMP_ATI #x8742)

(defconstant GL_MIRROR_CLAMP_TO_EDGE_ATI #x8743)

(defconstant GL_ALL_COMPLETED_NV #x84F2)

(defconstant GL_FENCE_STATUS_NV #x84F3)

(defconstant GL_FENCE_CONDITION_NV #x84F4)

(defconstant GL_MIRRORED_REPEAT_IBM #x8370)

(defconstant GL_EVAL_2D_NV #x86C0)

(defconstant GL_EVAL_TRIANGULAR_2D_NV #x86C1)

(defconstant GL_MAP_TESSELLATION_NV #x86C2)

(defconstant GL_MAP_ATTRIB_U_ORDER_NV #x86C3)

(defconstant GL_MAP_ATTRIB_V_ORDER_NV #x86C4)

(defconstant GL_EVAL_FRACTIONAL_TESSELLATION_NV #x86C5)

(defconstant GL_EVAL_VERTEX_ATTRIB0_NV #x86C6)

(defconstant GL_EVAL_VERTEX_ATTRIB1_NV #x86C7)

(defconstant GL_EVAL_VERTEX_ATTRIB2_NV #x86C8)

(defconstant GL_EVAL_VERTEX_ATTRIB3_NV #x86C9)

(defconstant GL_EVAL_VERTEX_ATTRIB4_NV #x86CA)

(defconstant GL_EVAL_VERTEX_ATTRIB5_NV #x86CB)

(defconstant GL_EVAL_VERTEX_ATTRIB6_NV #x86CC)

(defconstant GL_EVAL_VERTEX_ATTRIB7_NV #x86CD)

(defconstant GL_EVAL_VERTEX_ATTRIB8_NV #x86CE)

(defconstant GL_EVAL_VERTEX_ATTRIB9_NV #x86CF)

(defconstant GL_EVAL_VERTEX_ATTRIB10_NV #x86D0)

(defconstant GL_EVAL_VERTEX_ATTRIB11_NV #x86D1)

(defconstant GL_EVAL_VERTEX_ATTRIB12_NV #x86D2)

(defconstant GL_EVAL_VERTEX_ATTRIB13_NV #x86D3)

(defconstant GL_EVAL_VERTEX_ATTRIB14_NV #x86D4)

(defconstant GL_EVAL_VERTEX_ATTRIB15_NV #x86D5)

(defconstant GL_MAX_MAP_TESSELLATION_NV #x86D6)

(defconstant GL_MAX_RATIONAL_EVAL_ORDER_NV #x86D7)

(defconstant GL_DEPTH_STENCIL_NV #x84F9)

(defconstant GL_UNSIGNED_INT_24_8_NV #x84FA)

(defconstant GL_PER_STAGE_CONSTANTS_NV #x8535)

(defconstant GL_TEXTURE_RECTANGLE_NV #x84F5)

(defconstant GL_TEXTURE_BINDING_RECTANGLE_NV #x84F6)

(defconstant GL_PROXY_TEXTURE_RECTANGLE_NV #x84F7)

(defconstant GL_MAX_RECTANGLE_TEXTURE_SIZE_NV #x84F8)

(defconstant GL_OFFSET_TEXTURE_RECTANGLE_NV #x864C)

(defconstant GL_OFFSET_TEXTURE_RECTANGLE_SCALE_NV #x864D)

(defconstant GL_DOT_PRODUCT_TEXTURE_RECTANGLE_NV #x864E)

(defconstant GL_RGBA_UNSIGNED_DOT_PRODUCT_MAPPING_NV #x86D9)

(defconstant GL_UNSIGNED_INT_S8_S8_8_8_NV #x86DA)

(defconstant GL_UNSIGNED_INT_8_8_S8_S8_REV_NV #x86DB)

(defconstant GL_DSDT_MAG_INTENSITY_NV #x86DC)

(defconstant GL_SHADER_CONSISTENT_NV #x86DD)

(defconstant GL_TEXTURE_SHADER_NV #x86DE)

(defconstant GL_SHADER_OPERATION_NV #x86DF)

(defconstant GL_CULL_MODES_NV #x86E0)

(defconstant GL_OFFSET_TEXTURE_MATRIX_NV #x86E1)

(defconstant GL_OFFSET_TEXTURE_SCALE_NV #x86E2)

(defconstant GL_OFFSET_TEXTURE_BIAS_NV #x86E3)

(defconstant GL_OFFSET_TEXTURE_2D_MATRIX_NV #x86E1)

(defconstant GL_OFFSET_TEXTURE_2D_SCALE_NV #x86E2)

(defconstant GL_OFFSET_TEXTURE_2D_BIAS_NV #x86E3)

(defconstant GL_PREVIOUS_TEXTURE_INPUT_NV #x86E4)

(defconstant GL_CONST_EYE_NV #x86E5)

(defconstant GL_PASS_THROUGH_NV #x86E6)

(defconstant GL_CULL_FRAGMENT_NV #x86E7)

(defconstant GL_OFFSET_TEXTURE_2D_NV #x86E8)

(defconstant GL_DEPENDENT_AR_TEXTURE_2D_NV #x86E9)

(defconstant GL_DEPENDENT_GB_TEXTURE_2D_NV #x86EA)

(defconstant GL_DOT_PRODUCT_NV #x86EC)

(defconstant GL_DOT_PRODUCT_DEPTH_REPLACE_NV #x86ED)

(defconstant GL_DOT_PRODUCT_TEXTURE_2D_NV #x86EE)

(defconstant GL_DOT_PRODUCT_TEXTURE_CUBE_MAP_NV #x86F0)

(defconstant GL_DOT_PRODUCT_DIFFUSE_CUBE_MAP_NV #x86F1)

(defconstant GL_DOT_PRODUCT_REFLECT_CUBE_MAP_NV #x86F2)

(defconstant GL_DOT_PRODUCT_CONST_EYE_REFLECT_CUBE_MAP_NV #x86F3)

(defconstant GL_HILO_NV #x86F4)

(defconstant GL_DSDT_NV #x86F5)

(defconstant GL_DSDT_MAG_NV #x86F6)

(defconstant GL_DSDT_MAG_VIB_NV #x86F7)

(defconstant GL_HILO16_NV #x86F8)

(defconstant GL_SIGNED_HILO_NV #x86F9)

(defconstant GL_SIGNED_HILO16_NV #x86FA)

(defconstant GL_SIGNED_RGBA_NV #x86FB)

(defconstant GL_SIGNED_RGBA8_NV #x86FC)

(defconstant GL_SIGNED_RGB_NV #x86FE)

(defconstant GL_SIGNED_RGB8_NV #x86FF)

(defconstant GL_SIGNED_LUMINANCE_NV #x8701)

(defconstant GL_SIGNED_LUMINANCE8_NV #x8702)

(defconstant GL_SIGNED_LUMINANCE_ALPHA_NV #x8703)

(defconstant GL_SIGNED_LUMINANCE8_ALPHA8_NV #x8704)

(defconstant GL_SIGNED_ALPHA_NV #x8705)

(defconstant GL_SIGNED_ALPHA8_NV #x8706)

(defconstant GL_SIGNED_INTENSITY_NV #x8707)

(defconstant GL_SIGNED_INTENSITY8_NV #x8708)

(defconstant GL_DSDT8_NV #x8709)

(defconstant GL_DSDT8_MAG8_NV #x870A)

(defconstant GL_DSDT8_MAG8_INTENSITY8_NV #x870B)

(defconstant GL_SIGNED_RGB_UNSIGNED_ALPHA_NV #x870C)

(defconstant GL_SIGNED_RGB8_UNSIGNED_ALPHA8_NV #x870D)

(defconstant GL_HI_SCALE_NV #x870E)

(defconstant GL_LO_SCALE_NV #x870F)

(defconstant GL_DS_SCALE_NV #x8710)

(defconstant GL_DT_SCALE_NV #x8711)

(defconstant GL_MAGNITUDE_SCALE_NV #x8712)

(defconstant GL_VIBRANCE_SCALE_NV #x8713)

(defconstant GL_HI_BIAS_NV #x8714)

(defconstant GL_LO_BIAS_NV #x8715)

(defconstant GL_DS_BIAS_NV #x8716)

(defconstant GL_DT_BIAS_NV #x8717)

(defconstant GL_MAGNITUDE_BIAS_NV #x8718)

(defconstant GL_VIBRANCE_BIAS_NV #x8719)

(defconstant GL_TEXTURE_BORDER_VALUES_NV #x871A)

(defconstant GL_TEXTURE_HI_SIZE_NV #x871B)

(defconstant GL_TEXTURE_LO_SIZE_NV #x871C)

(defconstant GL_TEXTURE_DS_SIZE_NV #x871D)

(defconstant GL_TEXTURE_DT_SIZE_NV #x871E)

(defconstant GL_TEXTURE_MAG_SIZE_NV #x871F)

(defconstant GL_DOT_PRODUCT_TEXTURE_3D_NV #x86EF)

(defconstant GL_VERTEX_ARRAY_RANGE_WITHOUT_FLUSH_NV #x8533)

(defconstant GL_VERTEX_PROGRAM_NV #x8620)

(defconstant GL_VERTEX_STATE_PROGRAM_NV #x8621)

(defconstant GL_ATTRIB_ARRAY_SIZE_NV #x8623)

(defconstant GL_ATTRIB_ARRAY_STRIDE_NV #x8624)

(defconstant GL_ATTRIB_ARRAY_TYPE_NV #x8625)

(defconstant GL_CURRENT_ATTRIB_NV #x8626)

(defconstant GL_PROGRAM_LENGTH_NV #x8627)

(defconstant GL_PROGRAM_STRING_NV #x8628)

(defconstant GL_MODELVIEW_PROJECTION_NV #x8629)

(defconstant GL_IDENTITY_NV #x862A)

(defconstant GL_INVERSE_NV #x862B)

(defconstant GL_TRANSPOSE_NV #x862C)

(defconstant GL_INVERSE_TRANSPOSE_NV #x862D)

(defconstant GL_MAX_TRACK_MATRIX_STACK_DEPTH_NV #x862E)

(defconstant GL_MAX_TRACK_MATRICES_NV #x862F)

(defconstant GL_MATRIX0_NV #x8630)

(defconstant GL_MATRIX1_NV #x8631)

(defconstant GL_MATRIX2_NV #x8632)

(defconstant GL_MATRIX3_NV #x8633)

(defconstant GL_MATRIX4_NV #x8634)

(defconstant GL_MATRIX5_NV #x8635)

(defconstant GL_MATRIX6_NV #x8636)

(defconstant GL_MATRIX7_NV #x8637)

(defconstant GL_CURRENT_MATRIX_STACK_DEPTH_NV #x8640)

(defconstant GL_CURRENT_MATRIX_NV #x8641)

(defconstant GL_VERTEX_PROGRAM_POINT_SIZE_NV #x8642)

(defconstant GL_VERTEX_PROGRAM_TWO_SIDE_NV #x8643)

(defconstant GL_PROGRAM_PARAMETER_NV #x8644)

(defconstant GL_ATTRIB_ARRAY_POINTER_NV #x8645)

(defconstant GL_PROGRAM_TARGET_NV #x8646)

(defconstant GL_PROGRAM_RESIDENT_NV #x8647)

(defconstant GL_TRACK_MATRIX_NV #x8648)

(defconstant GL_TRACK_MATRIX_TRANSFORM_NV #x8649)

(defconstant GL_VERTEX_PROGRAM_BINDING_NV #x864A)

(defconstant GL_PROGRAM_ERROR_POSITION_NV #x864B)

(defconstant GL_VERTEX_ATTRIB_ARRAY0_NV #x8650)

(defconstant GL_VERTEX_ATTRIB_ARRAY1_NV #x8651)

(defconstant GL_VERTEX_ATTRIB_ARRAY2_NV #x8652)

(defconstant GL_VERTEX_ATTRIB_ARRAY3_NV #x8653)

(defconstant GL_VERTEX_ATTRIB_ARRAY4_NV #x8654)

(defconstant GL_VERTEX_ATTRIB_ARRAY5_NV #x8655)

(defconstant GL_VERTEX_ATTRIB_ARRAY6_NV #x8656)

(defconstant GL_VERTEX_ATTRIB_ARRAY7_NV #x8657)

(defconstant GL_VERTEX_ATTRIB_ARRAY8_NV #x8658)

(defconstant GL_VERTEX_ATTRIB_ARRAY9_NV #x8659)

(defconstant GL_VERTEX_ATTRIB_ARRAY10_NV #x865A)

(defconstant GL_VERTEX_ATTRIB_ARRAY11_NV #x865B)

(defconstant GL_VERTEX_ATTRIB_ARRAY12_NV #x865C)

(defconstant GL_VERTEX_ATTRIB_ARRAY13_NV #x865D)

(defconstant GL_VERTEX_ATTRIB_ARRAY14_NV #x865E)

(defconstant GL_VERTEX_ATTRIB_ARRAY15_NV #x865F)

(defconstant GL_MAP1_VERTEX_ATTRIB0_4_NV #x8660)

(defconstant GL_MAP1_VERTEX_ATTRIB1_4_NV #x8661)

(defconstant GL_MAP1_VERTEX_ATTRIB2_4_NV #x8662)

(defconstant GL_MAP1_VERTEX_ATTRIB3_4_NV #x8663)

(defconstant GL_MAP1_VERTEX_ATTRIB4_4_NV #x8664)

(defconstant GL_MAP1_VERTEX_ATTRIB5_4_NV #x8665)

(defconstant GL_MAP1_VERTEX_ATTRIB6_4_NV #x8666)

(defconstant GL_MAP1_VERTEX_ATTRIB7_4_NV #x8667)

(defconstant GL_MAP1_VERTEX_ATTRIB8_4_NV #x8668)

(defconstant GL_MAP1_VERTEX_ATTRIB9_4_NV #x8669)

(defconstant GL_MAP1_VERTEX_ATTRIB10_4_NV #x866A)

(defconstant GL_MAP1_VERTEX_ATTRIB11_4_NV #x866B)

(defconstant GL_MAP1_VERTEX_ATTRIB12_4_NV #x866C)

(defconstant GL_MAP1_VERTEX_ATTRIB13_4_NV #x866D)

(defconstant GL_MAP1_VERTEX_ATTRIB14_4_NV #x866E)

(defconstant GL_MAP1_VERTEX_ATTRIB15_4_NV #x866F)

(defconstant GL_MAP2_VERTEX_ATTRIB0_4_NV #x8670)

(defconstant GL_MAP2_VERTEX_ATTRIB1_4_NV #x8671)

(defconstant GL_MAP2_VERTEX_ATTRIB2_4_NV #x8672)

(defconstant GL_MAP2_VERTEX_ATTRIB3_4_NV #x8673)

(defconstant GL_MAP2_VERTEX_ATTRIB4_4_NV #x8674)

(defconstant GL_MAP2_VERTEX_ATTRIB5_4_NV #x8675)

(defconstant GL_MAP2_VERTEX_ATTRIB6_4_NV #x8676)

(defconstant GL_MAP2_VERTEX_ATTRIB7_4_NV #x8677)

(defconstant GL_MAP2_VERTEX_ATTRIB8_4_NV #x8678)

(defconstant GL_MAP2_VERTEX_ATTRIB9_4_NV #x8679)

(defconstant GL_MAP2_VERTEX_ATTRIB10_4_NV #x867A)

(defconstant GL_MAP2_VERTEX_ATTRIB11_4_NV #x867B)

(defconstant GL_MAP2_VERTEX_ATTRIB12_4_NV #x867C)

(defconstant GL_MAP2_VERTEX_ATTRIB13_4_NV #x867D)

(defconstant GL_MAP2_VERTEX_ATTRIB14_4_NV #x867E)

(defconstant GL_MAP2_VERTEX_ATTRIB15_4_NV #x867F)

(defconstant GL_TEXTURE_MAX_CLAMP_S_SGIX #x8369)

(defconstant GL_TEXTURE_MAX_CLAMP_T_SGIX #x836A)

(defconstant GL_TEXTURE_MAX_CLAMP_R_SGIX #x836B)

(defconstant GL_SCALEBIAS_HINT_SGIX #x8322)

(defconstant GL_INTERLACE_OML #x8980)

(defconstant GL_INTERLACE_READ_OML #x8981)

(defconstant GL_FORMAT_SUBSAMPLE_24_24_OML #x8982)

(defconstant GL_FORMAT_SUBSAMPLE_244_244_OML #x8983)

(defconstant GL_PACK_RESAMPLE_OML #x8984)

(defconstant GL_UNPACK_RESAMPLE_OML #x8985)

(defconstant GL_RESAMPLE_REPLICATE_OML #x8986)

(defconstant GL_RESAMPLE_ZERO_FILL_OML #x8987)

(defconstant GL_RESAMPLE_AVERAGE_OML #x8988)

(defconstant GL_RESAMPLE_DECIMATE_OML #x8989)

(defconstant GL_DEPTH_STENCIL_TO_RGBA_NV #x886E)

(defconstant GL_DEPTH_STENCIL_TO_BGRA_NV #x886F)

(defconstant GL_BUMP_ROT_MATRIX_ATI #x8775)

(defconstant GL_BUMP_ROT_MATRIX_SIZE_ATI #x8776)

(defconstant GL_BUMP_NUM_TEX_UNITS_ATI #x8777)

(defconstant GL_BUMP_TEX_UNITS_ATI #x8778)

(defconstant GL_DUDV_ATI #x8779)

(defconstant GL_DU8DV8_ATI #x877A)

(defconstant GL_BUMP_ENVMAP_ATI #x877B)

(defconstant GL_BUMP_TARGET_ATI #x877C)

(defconstant GL_FRAGMENT_SHADER_ATI #x8920)

(defconstant GL_REG_0_ATI #x8921)

(defconstant GL_REG_1_ATI #x8922)

(defconstant GL_REG_2_ATI #x8923)

(defconstant GL_REG_3_ATI #x8924)

(defconstant GL_REG_4_ATI #x8925)

(defconstant GL_REG_5_ATI #x8926)

(defconstant GL_REG_6_ATI #x8927)

(defconstant GL_REG_7_ATI #x8928)

(defconstant GL_REG_8_ATI #x8929)

(defconstant GL_REG_9_ATI #x892A)

(defconstant GL_REG_10_ATI #x892B)

(defconstant GL_REG_11_ATI #x892C)

(defconstant GL_REG_12_ATI #x892D)

(defconstant GL_REG_13_ATI #x892E)

(defconstant GL_REG_14_ATI #x892F)

(defconstant GL_REG_15_ATI #x8930)

(defconstant GL_REG_16_ATI #x8931)

(defconstant GL_REG_17_ATI #x8932)

(defconstant GL_REG_18_ATI #x8933)

(defconstant GL_REG_19_ATI #x8934)

(defconstant GL_REG_20_ATI #x8935)

(defconstant GL_REG_21_ATI #x8936)

(defconstant GL_REG_22_ATI #x8937)

(defconstant GL_REG_23_ATI #x8938)

(defconstant GL_REG_24_ATI #x8939)

(defconstant GL_REG_25_ATI #x893A)

(defconstant GL_REG_26_ATI #x893B)

(defconstant GL_REG_27_ATI #x893C)

(defconstant GL_REG_28_ATI #x893D)

(defconstant GL_REG_29_ATI #x893E)

(defconstant GL_REG_30_ATI #x893F)

(defconstant GL_REG_31_ATI #x8940)

(defconstant GL_CON_0_ATI #x8941)

(defconstant GL_CON_1_ATI #x8942)

(defconstant GL_CON_2_ATI #x8943)

(defconstant GL_CON_3_ATI #x8944)

(defconstant GL_CON_4_ATI #x8945)

(defconstant GL_CON_5_ATI #x8946)

(defconstant GL_CON_6_ATI #x8947)

(defconstant GL_CON_7_ATI #x8948)

(defconstant GL_CON_8_ATI #x8949)

(defconstant GL_CON_9_ATI #x894A)

(defconstant GL_CON_10_ATI #x894B)

(defconstant GL_CON_11_ATI #x894C)

(defconstant GL_CON_12_ATI #x894D)

(defconstant GL_CON_13_ATI #x894E)

(defconstant GL_CON_14_ATI #x894F)

(defconstant GL_CON_15_ATI #x8950)

(defconstant GL_CON_16_ATI #x8951)

(defconstant GL_CON_17_ATI #x8952)

(defconstant GL_CON_18_ATI #x8953)

(defconstant GL_CON_19_ATI #x8954)

(defconstant GL_CON_20_ATI #x8955)

(defconstant GL_CON_21_ATI #x8956)

(defconstant GL_CON_22_ATI #x8957)

(defconstant GL_CON_23_ATI #x8958)

(defconstant GL_CON_24_ATI #x8959)

(defconstant GL_CON_25_ATI #x895A)

(defconstant GL_CON_26_ATI #x895B)

(defconstant GL_CON_27_ATI #x895C)

(defconstant GL_CON_28_ATI #x895D)

(defconstant GL_CON_29_ATI #x895E)

(defconstant GL_CON_30_ATI #x895F)

(defconstant GL_CON_31_ATI #x8960)

(defconstant GL_MOV_ATI #x8961)

(defconstant GL_ADD_ATI #x8963)

(defconstant GL_MUL_ATI #x8964)

(defconstant GL_SUB_ATI #x8965)

(defconstant GL_DOT3_ATI #x8966)

(defconstant GL_DOT4_ATI #x8967)

(defconstant GL_MAD_ATI #x8968)

(defconstant GL_LERP_ATI #x8969)

(defconstant GL_CND_ATI #x896A)

(defconstant GL_CND0_ATI #x896B)

(defconstant GL_DOT2_ADD_ATI #x896C)

(defconstant GL_SECONDARY_INTERPOLATOR_ATI #x896D)

(defconstant GL_NUM_FRAGMENT_REGISTERS_ATI #x896E)

(defconstant GL_NUM_FRAGMENT_CONSTANTS_ATI #x896F)

(defconstant GL_NUM_PASSES_ATI #x8970)

(defconstant GL_NUM_INSTRUCTIONS_PER_PASS_ATI #x8971)

(defconstant GL_NUM_INSTRUCTIONS_TOTAL_ATI #x8972)

(defconstant GL_NUM_INPUT_INTERPOLATOR_COMPONENTS_ATI #x8973)

(defconstant GL_NUM_LOOPBACK_COMPONENTS_ATI #x8974)

(defconstant GL_COLOR_ALPHA_PAIRING_ATI #x8975)

(defconstant GL_SWIZZLE_STR_ATI #x8976)

(defconstant GL_SWIZZLE_STQ_ATI #x8977)

(defconstant GL_SWIZZLE_STR_DR_ATI #x8978)

(defconstant GL_SWIZZLE_STQ_DQ_ATI #x8979)

(defconstant GL_SWIZZLE_STRQ_ATI #x897A)

(defconstant GL_SWIZZLE_STRQ_DQ_ATI #x897B)

(defconstant GL_RED_BIT_ATI #x00000001)

(defconstant GL_GREEN_BIT_ATI #x00000002)

(defconstant GL_BLUE_BIT_ATI #x00000004)

(defconstant GL_2X_BIT_ATI #x00000001)

(defconstant GL_4X_BIT_ATI #x00000002)

(defconstant GL_8X_BIT_ATI #x00000004)

(defconstant GL_HALF_BIT_ATI #x00000008)

(defconstant GL_QUARTER_BIT_ATI #x00000010)

(defconstant GL_EIGHTH_BIT_ATI #x00000020)

(defconstant GL_SATURATE_BIT_ATI #x00000040)

(defconstant GL_COMP_BIT_ATI #x00000002)

(defconstant GL_NEGATE_BIT_ATI #x00000004)

(defconstant GL_BIAS_BIT_ATI #x00000008)

(defconstant GL_PN_TRIANGLES_ATI #x87F0)

(defconstant GL_MAX_PN_TRIANGLES_TESSELATION_LEVEL_ATI #x87F1)

(defconstant GL_PN_TRIANGLES_POINT_MODE_ATI #x87F2)

(defconstant GL_PN_TRIANGLES_NORMAL_MODE_ATI #x87F3)

(defconstant GL_PN_TRIANGLES_TESSELATION_LEVEL_ATI #x87F4)

(defconstant GL_PN_TRIANGLES_POINT_MODE_LINEAR_ATI #x87F5)

(defconstant GL_PN_TRIANGLES_POINT_MODE_CUBIC_ATI #x87F6)

(defconstant GL_PN_TRIANGLES_NORMAL_MODE_LINEAR_ATI #x87F7)

(defconstant GL_PN_TRIANGLES_NORMAL_MODE_QUADRATIC_ATI #x87F8)

(defconstant GL_STATIC_ATI #x8760)

(defconstant GL_DYNAMIC_ATI #x8761)

(defconstant GL_PRESERVE_ATI #x8762)

(defconstant GL_DISCARD_ATI #x8763)

(defconstant GL_OBJECT_BUFFER_SIZE_ATI #x8764)

(defconstant GL_OBJECT_BUFFER_USAGE_ATI #x8765)

(defconstant GL_ARRAY_OBJECT_BUFFER_ATI #x8766)

(defconstant GL_ARRAY_OBJECT_OFFSET_ATI #x8767)

(defconstant GL_VERTEX_SHADER_EXT #x8780)

(defconstant GL_VERTEX_SHADER_BINDING_EXT #x8781)

(defconstant GL_OP_INDEX_EXT #x8782)

(defconstant GL_OP_NEGATE_EXT #x8783)

(defconstant GL_OP_DOT3_EXT #x8784)

(defconstant GL_OP_DOT4_EXT #x8785)

(defconstant GL_OP_MUL_EXT #x8786)

(defconstant GL_OP_ADD_EXT #x8787)

(defconstant GL_OP_MADD_EXT #x8788)

(defconstant GL_OP_FRAC_EXT #x8789)

(defconstant GL_OP_MAX_EXT #x878A)

(defconstant GL_OP_MIN_EXT #x878B)

(defconstant GL_OP_SET_GE_EXT #x878C)

(defconstant GL_OP_SET_LT_EXT #x878D)

(defconstant GL_OP_CLAMP_EXT #x878E)

(defconstant GL_OP_FLOOR_EXT #x878F)

(defconstant GL_OP_ROUND_EXT #x8790)

(defconstant GL_OP_EXP_BASE_2_EXT #x8791)

(defconstant GL_OP_LOG_BASE_2_EXT #x8792)

(defconstant GL_OP_POWER_EXT #x8793)

(defconstant GL_OP_RECIP_EXT #x8794)

(defconstant GL_OP_RECIP_SQRT_EXT #x8795)

(defconstant GL_OP_SUB_EXT #x8796)

(defconstant GL_OP_CROSS_PRODUCT_EXT #x8797)

(defconstant GL_OP_MULTIPLY_MATRIX_EXT #x8798)

(defconstant GL_OP_MOV_EXT #x8799)

(defconstant GL_OUTPUT_VERTEX_EXT #x879A)

(defconstant GL_OUTPUT_COLOR0_EXT #x879B)

(defconstant GL_OUTPUT_COLOR1_EXT #x879C)

(defconstant GL_OUTPUT_TEXTURE_COORD0_EXT #x879D)

(defconstant GL_OUTPUT_TEXTURE_COORD1_EXT #x879E)

(defconstant GL_OUTPUT_TEXTURE_COORD2_EXT #x879F)

(defconstant GL_OUTPUT_TEXTURE_COORD3_EXT #x87A0)

(defconstant GL_OUTPUT_TEXTURE_COORD4_EXT #x87A1)

(defconstant GL_OUTPUT_TEXTURE_COORD5_EXT #x87A2)

(defconstant GL_OUTPUT_TEXTURE_COORD6_EXT #x87A3)

(defconstant GL_OUTPUT_TEXTURE_COORD7_EXT #x87A4)

(defconstant GL_OUTPUT_TEXTURE_COORD8_EXT #x87A5)

(defconstant GL_OUTPUT_TEXTURE_COORD9_EXT #x87A6)

(defconstant GL_OUTPUT_TEXTURE_COORD10_EXT #x87A7)

(defconstant GL_OUTPUT_TEXTURE_COORD11_EXT #x87A8)

(defconstant GL_OUTPUT_TEXTURE_COORD12_EXT #x87A9)

(defconstant GL_OUTPUT_TEXTURE_COORD13_EXT #x87AA)

(defconstant GL_OUTPUT_TEXTURE_COORD14_EXT #x87AB)

(defconstant GL_OUTPUT_TEXTURE_COORD15_EXT #x87AC)

(defconstant GL_OUTPUT_TEXTURE_COORD16_EXT #x87AD)

(defconstant GL_OUTPUT_TEXTURE_COORD17_EXT #x87AE)

(defconstant GL_OUTPUT_TEXTURE_COORD18_EXT #x87AF)

(defconstant GL_OUTPUT_TEXTURE_COORD19_EXT #x87B0)

(defconstant GL_OUTPUT_TEXTURE_COORD20_EXT #x87B1)

(defconstant GL_OUTPUT_TEXTURE_COORD21_EXT #x87B2)

(defconstant GL_OUTPUT_TEXTURE_COORD22_EXT #x87B3)

(defconstant GL_OUTPUT_TEXTURE_COORD23_EXT #x87B4)

(defconstant GL_OUTPUT_TEXTURE_COORD24_EXT #x87B5)

(defconstant GL_OUTPUT_TEXTURE_COORD25_EXT #x87B6)

(defconstant GL_OUTPUT_TEXTURE_COORD26_EXT #x87B7)

(defconstant GL_OUTPUT_TEXTURE_COORD27_EXT #x87B8)

(defconstant GL_OUTPUT_TEXTURE_COORD28_EXT #x87B9)

(defconstant GL_OUTPUT_TEXTURE_COORD29_EXT #x87BA)

(defconstant GL_OUTPUT_TEXTURE_COORD30_EXT #x87BB)

(defconstant GL_OUTPUT_TEXTURE_COORD31_EXT #x87BC)

(defconstant GL_OUTPUT_FOG_EXT #x87BD)

(defconstant GL_SCALAR_EXT #x87BE)

(defconstant GL_VECTOR_EXT #x87BF)

(defconstant GL_MATRIX_EXT #x87C0)

(defconstant GL_VARIANT_EXT #x87C1)

(defconstant GL_INVARIANT_EXT #x87C2)

(defconstant GL_LOCAL_CONSTANT_EXT #x87C3)

(defconstant GL_LOCAL_EXT #x87C4)

(defconstant GL_MAX_VERTEX_SHADER_INSTRUCTIONS_EXT #x87C5)

(defconstant GL_MAX_VERTEX_SHADER_VARIANTS_EXT #x87C6)

(defconstant GL_MAX_VERTEX_SHADER_INVARIANTS_EXT #x87C7)

(defconstant GL_MAX_VERTEX_SHADER_LOCAL_CONSTANTS_EXT #x87C8)

(defconstant GL_MAX_VERTEX_SHADER_LOCALS_EXT #x87C9)

(defconstant GL_MAX_OPTIMIZED_VERTEX_SHADER_INSTRUCTIONS_EXT #x87CA)

(defconstant GL_MAX_OPTIMIZED_VERTEX_SHADER_VARIANTS_EXT #x87CB)

(defconstant GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCAL_CONSTANTS_EXT #x87CC)

(defconstant GL_MAX_OPTIMIZED_VERTEX_SHADER_INVARIANTS_EXT #x87CD)

(defconstant GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCALS_EXT #x87CE)

(defconstant GL_VERTEX_SHADER_INSTRUCTIONS_EXT #x87CF)

(defconstant GL_VERTEX_SHADER_VARIANTS_EXT #x87D0)

(defconstant GL_VERTEX_SHADER_INVARIANTS_EXT #x87D1)

(defconstant GL_VERTEX_SHADER_LOCAL_CONSTANTS_EXT #x87D2)

(defconstant GL_VERTEX_SHADER_LOCALS_EXT #x87D3)

(defconstant GL_VERTEX_SHADER_OPTIMIZED_EXT #x87D4)

(defconstant GL_X_EXT #x87D5)

(defconstant GL_Y_EXT #x87D6)

(defconstant GL_Z_EXT #x87D7)

(defconstant GL_W_EXT #x87D8)

(defconstant GL_NEGATIVE_X_EXT #x87D9)

(defconstant GL_NEGATIVE_Y_EXT #x87DA)

(defconstant GL_NEGATIVE_Z_EXT #x87DB)

(defconstant GL_NEGATIVE_W_EXT #x87DC)

(defconstant GL_ZERO_EXT #x87DD)

(defconstant GL_ONE_EXT #x87DE)

(defconstant GL_NEGATIVE_ONE_EXT #x87DF)

(defconstant GL_NORMALIZED_RANGE_EXT #x87E0)

(defconstant GL_FULL_RANGE_EXT #x87E1)

(defconstant GL_CURRENT_VERTEX_EXT #x87E2)

(defconstant GL_MVP_MATRIX_EXT #x87E3)

(defconstant GL_VARIANT_VALUE_EXT #x87E4)

(defconstant GL_VARIANT_DATATYPE_EXT #x87E5)

(defconstant GL_VARIANT_ARRAY_STRIDE_EXT #x87E6)

(defconstant GL_VARIANT_ARRAY_TYPE_EXT #x87E7)

(defconstant GL_VARIANT_ARRAY_EXT #x87E8)

(defconstant GL_VARIANT_ARRAY_POINTER_EXT #x87E9)

(defconstant GL_INVARIANT_VALUE_EXT #x87EA)

(defconstant GL_INVARIANT_DATATYPE_EXT #x87EB)

(defconstant GL_LOCAL_CONSTANT_VALUE_EXT #x87EC)

(defconstant GL_LOCAL_CONSTANT_DATATYPE_EXT #x87ED)

(defconstant GL_MAX_VERTEX_STREAMS_ATI #x876B)

(defconstant GL_VERTEX_STREAM0_ATI #x876C)

(defconstant GL_VERTEX_STREAM1_ATI #x876D)

(defconstant GL_VERTEX_STREAM2_ATI #x876E)

(defconstant GL_VERTEX_STREAM3_ATI #x876F)

(defconstant GL_VERTEX_STREAM4_ATI #x8770)

(defconstant GL_VERTEX_STREAM5_ATI #x8771)

(defconstant GL_VERTEX_STREAM6_ATI #x8772)

(defconstant GL_VERTEX_STREAM7_ATI #x8773)

(defconstant GL_VERTEX_SOURCE_ATI #x8774)

(defconstant GL_ELEMENT_ARRAY_ATI #x8768)

(defconstant GL_ELEMENT_ARRAY_TYPE_ATI #x8769)

(defconstant GL_ELEMENT_ARRAY_POINTER_ATI #x876A)

(defconstant GL_QUAD_MESH_SUN #x8614)

(defconstant GL_TRIANGLE_MESH_SUN #x8615)

(defconstant GL_SLICE_ACCUM_SUN #x85CC)

(defconstant GL_MULTISAMPLE_FILTER_HINT_NV #x8534)

(defconstant GL_DEPTH_CLAMP_NV #x864F)

(defconstant GL_PIXEL_COUNTER_BITS_NV #x8864)

(defconstant GL_CURRENT_OCCLUSION_QUERY_ID_NV #x8865)

(defconstant GL_PIXEL_COUNT_NV #x8866)

(defconstant GL_PIXEL_COUNT_AVAILABLE_NV #x8867)

(defconstant GL_POINT_SPRITE_NV #x8861)

(defconstant GL_COORD_REPLACE_NV #x8862)

(defconstant GL_POINT_SPRITE_R_MODE_NV #x8863)

(defconstant GL_OFFSET_PROJECTIVE_TEXTURE_2D_NV #x8850)

(defconstant GL_OFFSET_PROJECTIVE_TEXTURE_2D_SCALE_NV #x8851)

(defconstant GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_NV #x8852)

(defconstant GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_SCALE_NV #x8853)

(defconstant GL_OFFSET_HILO_TEXTURE_2D_NV #x8854)

(defconstant GL_OFFSET_HILO_TEXTURE_RECTANGLE_NV #x8855)

(defconstant GL_OFFSET_HILO_PROJECTIVE_TEXTURE_2D_NV #x8856)

(defconstant GL_OFFSET_HILO_PROJECTIVE_TEXTURE_RECTANGLE_NV #x8857)

(defconstant GL_DEPENDENT_HILO_TEXTURE_2D_NV #x8858)

(defconstant GL_DEPENDENT_RGB_TEXTURE_3D_NV #x8859)

(defconstant GL_DEPENDENT_RGB_TEXTURE_CUBE_MAP_NV #x885A)

(defconstant GL_DOT_PRODUCT_PASS_THROUGH_NV #x885B)

(defconstant GL_DOT_PRODUCT_TEXTURE_1D_NV #x885C)

(defconstant GL_DOT_PRODUCT_AFFINE_DEPTH_REPLACE_NV #x885D)

(defconstant GL_HILO8_NV #x885E)

(defconstant GL_SIGNED_HILO8_NV #x885F)

(defconstant GL_FORCE_BLUE_TO_ONE_NV #x8860)

(defconstant GL_STENCIL_TEST_TWO_SIDE_EXT #x8910)

(defconstant GL_ACTIVE_STENCIL_FACE_EXT #x8911)

(defconstant GL_TEXT_FRAGMENT_SHADER_ATI #x8200)

(defconstant GL_UNPACK_CLIENT_STORAGE_APPLE #x85B2)

(defconstant GL_ELEMENT_ARRAY_APPLE #x8768)

(defconstant GL_ELEMENT_ARRAY_TYPE_APPLE #x8769)

(defconstant GL_ELEMENT_ARRAY_POINTER_APPLE #x876A)

(defconstant GL_DRAW_PIXELS_APPLE #x8A0A)

(defconstant GL_FENCE_APPLE #x8A0B)

(defconstant GL_VERTEX_ARRAY_BINDING_APPLE #x85B5)

(defconstant GL_VERTEX_ARRAY_RANGE_APPLE #x851D)

(defconstant GL_VERTEX_ARRAY_RANGE_LENGTH_APPLE #x851E)

(defconstant GL_VERTEX_ARRAY_STORAGE_HINT_APPLE #x851F)

(defconstant GL_VERTEX_ARRAY_RANGE_POINTER_APPLE #x8521)

(defconstant GL_STORAGE_CACHED_APPLE #x85BE)

(defconstant GL_STORAGE_SHARED_APPLE #x85BF)

(defconstant GL_YCBCR_422_APPLE #x85B9)

(defconstant GL_UNSIGNED_SHORT_8_8_APPLE #x85BA)

(defconstant GL_UNSIGNED_SHORT_8_8_REV_APPLE #x85BB)

(defconstant GL_RGB_S3TC #x83A0)

(defconstant GL_RGB4_S3TC #x83A1)

(defconstant GL_RGBA_S3TC #x83A2)

(defconstant GL_RGBA4_S3TC #x83A3)

(defconstant GL_MAX_DRAW_BUFFERS_ATI #x8824)

(defconstant GL_DRAW_BUFFER0_ATI #x8825)

(defconstant GL_DRAW_BUFFER1_ATI #x8826)

(defconstant GL_DRAW_BUFFER2_ATI #x8827)

(defconstant GL_DRAW_BUFFER3_ATI #x8828)

(defconstant GL_DRAW_BUFFER4_ATI #x8829)

(defconstant GL_DRAW_BUFFER5_ATI #x882A)

(defconstant GL_DRAW_BUFFER6_ATI #x882B)

(defconstant GL_DRAW_BUFFER7_ATI #x882C)

(defconstant GL_DRAW_BUFFER8_ATI #x882D)

(defconstant GL_DRAW_BUFFER9_ATI #x882E)

(defconstant GL_DRAW_BUFFER10_ATI #x882F)

(defconstant GL_DRAW_BUFFER11_ATI #x8830)

(defconstant GL_DRAW_BUFFER12_ATI #x8831)

(defconstant GL_DRAW_BUFFER13_ATI #x8832)

(defconstant GL_DRAW_BUFFER14_ATI #x8833)

(defconstant GL_DRAW_BUFFER15_ATI #x8834)

(defconstant GL_TYPE_RGBA_FLOAT_ATI #x8820)

(defconstant GL_COLOR_CLEAR_UNCLAMPED_VALUE_ATI #x8835)

(defconstant GL_MODULATE_ADD_ATI #x8744)

(defconstant GL_MODULATE_SIGNED_ADD_ATI #x8745)

(defconstant GL_MODULATE_SUBTRACT_ATI #x8746)

(defconstant GL_RGBA_FLOAT32_ATI #x8814)

(defconstant GL_RGB_FLOAT32_ATI #x8815)

(defconstant GL_ALPHA_FLOAT32_ATI #x8816)

(defconstant GL_INTENSITY_FLOAT32_ATI #x8817)

(defconstant GL_LUMINANCE_FLOAT32_ATI #x8818)

(defconstant GL_LUMINANCE_ALPHA_FLOAT32_ATI #x8819)

(defconstant GL_RGBA_FLOAT16_ATI #x881A)

(defconstant GL_RGB_FLOAT16_ATI #x881B)

(defconstant GL_ALPHA_FLOAT16_ATI #x881C)

(defconstant GL_INTENSITY_FLOAT16_ATI #x881D)

(defconstant GL_LUMINANCE_FLOAT16_ATI #x881E)

(defconstant GL_LUMINANCE_ALPHA_FLOAT16_ATI #x881F)

(defconstant GL_FLOAT_R_NV #x8880)

(defconstant GL_FLOAT_RG_NV #x8881)

(defconstant GL_FLOAT_RGB_NV #x8882)

(defconstant GL_FLOAT_RGBA_NV #x8883)

(defconstant GL_FLOAT_R16_NV #x8884)

(defconstant GL_FLOAT_R32_NV #x8885)

(defconstant GL_FLOAT_RG16_NV #x8886)

(defconstant GL_FLOAT_RG32_NV #x8887)

(defconstant GL_FLOAT_RGB16_NV #x8888)

(defconstant GL_FLOAT_RGB32_NV #x8889)

(defconstant GL_FLOAT_RGBA16_NV #x888A)

(defconstant GL_FLOAT_RGBA32_NV #x888B)

(defconstant GL_TEXTURE_FLOAT_COMPONENTS_NV #x888C)

(defconstant GL_FLOAT_CLEAR_COLOR_VALUE_NV #x888D)

(defconstant GL_FLOAT_RGBA_MODE_NV #x888E)

(defconstant GL_MAX_FRAGMENT_PROGRAM_LOCAL_PARAMETERS_NV #x8868)

(defconstant GL_FRAGMENT_PROGRAM_NV #x8870)

(defconstant GL_MAX_TEXTURE_COORDS_NV #x8871)

(defconstant GL_MAX_TEXTURE_IMAGE_UNITS_NV #x8872)

(defconstant GL_FRAGMENT_PROGRAM_BINDING_NV #x8873)

(defconstant GL_PROGRAM_ERROR_STRING_NV #x8874)

(defconstant GL_HALF_FLOAT_NV #x140B)

(defconstant GL_WRITE_PIXEL_DATA_RANGE_NV #x8878)

(defconstant GL_READ_PIXEL_DATA_RANGE_NV #x8879)

(defconstant GL_WRITE_PIXEL_DATA_RANGE_LENGTH_NV #x887A)

(defconstant GL_READ_PIXEL_DATA_RANGE_LENGTH_NV #x887B)

(defconstant GL_WRITE_PIXEL_DATA_RANGE_POINTER_NV #x887C)

(defconstant GL_READ_PIXEL_DATA_RANGE_POINTER_NV #x887D)

(defconstant GL_PRIMITIVE_RESTART_NV #x8558)

(defconstant GL_PRIMITIVE_RESTART_INDEX_NV #x8559)

(defconstant GL_TEXTURE_UNSIGNED_REMAP_MODE_NV #x888F)

(defconstant GL_STENCIL_BACK_FUNC_ATI #x8800)

(defconstant GL_STENCIL_BACK_FAIL_ATI #x8801)

(defconstant GL_STENCIL_BACK_PASS_DEPTH_FAIL_ATI #x8802)

(defconstant GL_STENCIL_BACK_PASS_DEPTH_PASS_ATI #x8803)

(defconstant GL_IMPLEMENTATION_COLOR_READ_TYPE_OES #x8B9A)

(defconstant GL_IMPLEMENTATION_COLOR_READ_FORMAT_OES #x8B9B)

(defconstant GL_DEPTH_BOUNDS_TEST_EXT #x8890)

(defconstant GL_DEPTH_BOUNDS_EXT #x8891)

(defconstant GL_MIRROR_CLAMP_EXT #x8742)

(defconstant GL_MIRROR_CLAMP_TO_EDGE_EXT #x8743)

(defconstant GL_MIRROR_CLAMP_TO_BORDER_EXT #x8912)

(defconstant GL_BLEND_EQUATION_RGB_EXT #x8009)

(defconstant GL_BLEND_EQUATION_ALPHA_EXT #x883D)

(defconstant GL_PACK_INVERT_MESA #x8758)

(defconstant GL_UNSIGNED_SHORT_8_8_MESA #x85BA)

(defconstant GL_UNSIGNED_SHORT_8_8_REV_MESA #x85BB)

(defconstant GL_YCBCR_MESA #x8757)

(defconstant GL_PIXEL_PACK_BUFFER_EXT #x88EB)

(defconstant GL_PIXEL_UNPACK_BUFFER_EXT #x88EC)

(defconstant GL_PIXEL_PACK_BUFFER_BINDING_EXT #x88ED)

(defconstant GL_PIXEL_UNPACK_BUFFER_BINDING_EXT #x88EF)

(defconstant GL_MAX_PROGRAM_EXEC_INSTRUCTIONS_NV #x88F4)

(defconstant GL_MAX_PROGRAM_CALL_DEPTH_NV #x88F5)

(defconstant GL_MAX_PROGRAM_IF_DEPTH_NV #x88F6)

(defconstant GL_MAX_PROGRAM_LOOP_DEPTH_NV #x88F7)

(defconstant GL_MAX_PROGRAM_LOOP_COUNT_NV #x88F8)

(defconstant GL_INVALID_FRAMEBUFFER_OPERATION_EXT #x0506)

(defconstant GL_MAX_RENDERBUFFER_SIZE_EXT #x84E8)

(defconstant GL_FRAMEBUFFER_BINDING_EXT #x8CA6)

(defconstant GL_RENDERBUFFER_BINDING_EXT #x8CA7)

(defconstant GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_EXT #x8CD0)

(defconstant GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_EXT #x8CD1)

(defconstant GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_EXT #x8CD2)

(defconstant GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_EXT #x8CD3)

(defconstant GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_EXT #x8CD4)

(defconstant GL_FRAMEBUFFER_COMPLETE_EXT #x8CD5)

(defconstant GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT #x8CD6)

(defconstant GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT #x8CD7)

(defconstant GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT #x8CD8)

(defconstant GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT #x8CD9)

(defconstant GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT #x8CDA)

(defconstant GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT #x8CDB)

(defconstant GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT #x8CDC)

(defconstant GL_FRAMEBUFFER_UNSUPPORTED_EXT #x8CDD)

(defconstant GL_MAX_COLOR_ATTACHMENTS_EXT #x8CDF)

(defconstant GL_COLOR_ATTACHMENT0_EXT #x8CE0)

(defconstant GL_COLOR_ATTACHMENT1_EXT #x8CE1)

(defconstant GL_COLOR_ATTACHMENT2_EXT #x8CE2)

(defconstant GL_COLOR_ATTACHMENT3_EXT #x8CE3)

(defconstant GL_COLOR_ATTACHMENT4_EXT #x8CE4)

(defconstant GL_COLOR_ATTACHMENT5_EXT #x8CE5)

(defconstant GL_COLOR_ATTACHMENT6_EXT #x8CE6)

(defconstant GL_COLOR_ATTACHMENT7_EXT #x8CE7)

(defconstant GL_COLOR_ATTACHMENT8_EXT #x8CE8)

(defconstant GL_COLOR_ATTACHMENT9_EXT #x8CE9)

(defconstant GL_COLOR_ATTACHMENT10_EXT #x8CEA)

(defconstant GL_COLOR_ATTACHMENT11_EXT #x8CEB)

(defconstant GL_COLOR_ATTACHMENT12_EXT #x8CEC)

(defconstant GL_COLOR_ATTACHMENT13_EXT #x8CED)

(defconstant GL_COLOR_ATTACHMENT14_EXT #x8CEE)

(defconstant GL_COLOR_ATTACHMENT15_EXT #x8CEF)

(defconstant GL_DEPTH_ATTACHMENT_EXT #x8D00)

(defconstant GL_STENCIL_ATTACHMENT_EXT #x8D20)

(defconstant GL_FRAMEBUFFER_EXT #x8D40)

(defconstant GL_RENDERBUFFER_EXT #x8D41)

(defconstant GL_RENDERBUFFER_WIDTH_EXT #x8D42)

(defconstant GL_RENDERBUFFER_HEIGHT_EXT #x8D43)

(defconstant GL_RENDERBUFFER_INTERNAL_FORMAT_EXT #x8D44)

(defconstant GL_STENCIL_INDEX1_EXT #x8D46)

(defconstant GL_STENCIL_INDEX4_EXT #x8D47)

(defconstant GL_STENCIL_INDEX8_EXT #x8D48)

(defconstant GL_STENCIL_INDEX16_EXT #x8D49)

(defconstant GL_RENDERBUFFER_RED_SIZE_EXT #x8D50)

(defconstant GL_RENDERBUFFER_GREEN_SIZE_EXT #x8D51)

(defconstant GL_RENDERBUFFER_BLUE_SIZE_EXT #x8D52)

(defconstant GL_RENDERBUFFER_ALPHA_SIZE_EXT #x8D53)

(defconstant GL_RENDERBUFFER_DEPTH_SIZE_EXT #x8D54)

(defconstant GL_RENDERBUFFER_STENCIL_SIZE_EXT #x8D55)

(defconstant GL_VERSION_1_4 1)

(defconstant GL_VERSION_1_5 1)

(defconstant GL_VERSION_2_0 1)

(defconstant GL_ARB_transpose_matrix 1)

(defconstant GL_ARB_multisample 1)

(defconstant GL_ARB_texture_env_add 1)

(defconstant GL_ARB_texture_cube_map 1)

(defconstant GL_ARB_texture_compression 1)

(defconstant GL_ARB_texture_border_clamp 1)

(defconstant GL_ARB_point_parameters 1)

(defconstant GL_ARB_vertex_blend 1)

(defconstant GL_ARB_matrix_palette 1)

(defconstant GL_ARB_texture_env_combine 1)

(defconstant GL_ARB_texture_env_crossbar 1)

(defconstant GL_ARB_texture_env_dot3 1)

(defconstant GL_ARB_texture_mirrored_repeat 1)

(defconstant GL_ARB_depth_texture 1)

(defconstant GL_ARB_shadow 1)

(defconstant GL_ARB_shadow_ambient 1)

(defconstant GL_ARB_window_pos 1)

(defconstant GL_ARB_vertex_program 1)

(defconstant GL_ARB_fragment_program 1)

(defconstant GL_ARB_vertex_buffer_object 1)

(defconstant GL_ARB_occlusion_query 1)

(defconstant GL_ARB_shader_objects 1)

(defconstant GL_ARB_vertex_shader 1)

(defconstant GL_ARB_fragment_shader 1)

(defconstant GL_ARB_shading_language_100 1)

(defconstant GL_ARB_texture_non_power_of_two 1)

(defconstant GL_ARB_point_sprite 1)

(defconstant GL_ARB_fragment_program_shadow 1)

(defconstant GL_ARB_draw_buffers 1)

(defconstant GL_ARB_texture_rectangle 1)

(defconstant GL_ARB_color_buffer_float 1)

(defconstant GL_ARB_half_float_pixel 1)

(defconstant GL_ARB_texture_float 1)

(defconstant GL_ARB_pixel_buffer_object 1)

(defconstant GL_EXT_abgr 1)

(defconstant GL_EXT_blend_color 1)

(defconstant GL_EXT_polygon_offset 1)

(defconstant GL_EXT_texture 1)

(defconstant GL_EXT_texture3D 1)

(defconstant GL_SGIS_texture_filter4 1)

(defconstant GL_EXT_subtexture 1)

(defconstant GL_EXT_copy_texture 1)

(defconstant GL_EXT_histogram 1)

(defconstant GL_EXT_convolution 1)

(defconstant GL_EXT_color_matrix 1)

(defconstant GL_SGI_color_table 1)

(defconstant GL_SGIX_pixel_texture 1)

(defconstant GL_SGIS_pixel_texture 1)

(defconstant GL_SGIS_texture4D 1)

(defconstant GL_SGI_texture_color_table 1)

(defconstant GL_EXT_cmyka 1)

(defconstant GL_EXT_texture_object 1)

(defconstant GL_SGIS_detail_texture 1)

(defconstant GL_SGIS_sharpen_texture 1)

(defconstant GL_EXT_packed_pixels 1)

(defconstant GL_SGIS_texture_lod 1)

(defconstant GL_SGIS_multisample 1)

(defconstant GL_EXT_rescale_normal 1)

(defconstant GL_EXT_vertex_array 1)

(defconstant GL_EXT_misc_attribute 1)

(defconstant GL_SGIS_generate_mipmap 1)

(defconstant GL_SGIX_clipmap 1)

(defconstant GL_SGIX_shadow 1)

(defconstant GL_SGIS_texture_edge_clamp 1)

(defconstant GL_SGIS_texture_border_clamp 1)

(defconstant GL_EXT_blend_minmax 1)

(defconstant GL_EXT_blend_subtract 1)

(defconstant GL_EXT_blend_logic_op 1)

(defconstant GL_SGIX_interlace 1)

(defconstant GL_SGIX_pixel_tiles 1)

(defconstant GL_SGIX_texture_select 1)

(defconstant GL_SGIX_sprite 1)

(defconstant GL_SGIX_texture_multi_buffer 1)

(defconstant GL_EXT_point_parameters 1)

(defconstant GL_SGIS_point_parameters 1)

(defconstant GL_SGIX_instruments 1)

(defconstant GL_SGIX_texture_scale_bias 1)

(defconstant GL_SGIX_framezoom 1)

(defconstant GL_SGIX_tag_sample_buffer 1)

(defconstant GL_SGIX_polynomial_ffd 1)

(defconstant GL_SGIX_reference_plane 1)

(defconstant GL_SGIX_flush_raster 1)

(defconstant GL_SGIX_depth_texture 1)

(defconstant GL_SGIS_fog_function 1)

(defconstant GL_SGIX_fog_offset 1)

(defconstant GL_HP_image_transform 1)

(defconstant GL_HP_convolution_border_modes 1)

(defconstant GL_SGIX_texture_add_env 1)

(defconstant GL_EXT_color_subtable 1)

(defconstant GL_PGI_vertex_hints 1)

(defconstant GL_PGI_misc_hints 1)

(defconstant GL_EXT_paletted_texture 1)

(defconstant GL_EXT_clip_volume_hint 1)

(defconstant GL_SGIX_list_priority 1)

(defconstant GL_SGIX_ir_instrument1 1)

(defconstant GL_SGIX_calligraphic_fragment 1)

(defconstant GL_SGIX_texture_lod_bias 1)

(defconstant GL_SGIX_shadow_ambient 1)

(defconstant GL_EXT_index_texture 1)

(defconstant GL_EXT_index_material 1)

(defconstant GL_EXT_index_func 1)

(defconstant GL_EXT_index_array_formats 1)

(defconstant GL_EXT_compiled_vertex_array 1)

(defconstant GL_EXT_cull_vertex 1)

(defconstant GL_SGIX_ycrcb 1)

(defconstant GL_SGIX_fragment_lighting 1)

(defconstant GL_IBM_rasterpos_clip 1)

(defconstant GL_HP_texture_lighting 1)

(defconstant GL_EXT_draw_range_elements 1)

(defconstant GL_WIN_phong_shading 1)

(defconstant GL_WIN_specular_fog 1)

(defconstant GL_EXT_light_texture 1)

(defconstant GL_SGIX_blend_alpha_minmax 1)

(defconstant GL_EXT_bgra 1)

(defconstant GL_SGIX_async 1)

(defconstant GL_SGIX_async_pixel 1)

(defconstant GL_SGIX_async_histogram 1)

(defconstant GL_INTEL_parallel_arrays 1)

(defconstant GL_HP_occlusion_test 1)

(defconstant GL_EXT_pixel_transform 1)

(defconstant GL_EXT_pixel_transform_color_table 1)

(defconstant GL_EXT_shared_texture_palette 1)

(defconstant GL_EXT_separate_specular_color 1)

(defconstant GL_EXT_secondary_color 1)

(defconstant GL_EXT_texture_perturb_normal 1)

(defconstant GL_EXT_multi_draw_arrays 1)

(defconstant GL_EXT_fog_coord 1)

(defconstant GL_REND_screen_coordinates 1)

(defconstant GL_EXT_coordinate_frame 1)

(defconstant GL_EXT_texture_env_combine 1)

(defconstant GL_APPLE_specular_vector 1)

(defconstant GL_APPLE_transform_hint 1)

(defconstant GL_SGIX_fog_scale 1)

(defconstant GL_SUNX_constant_data 1)

(defconstant GL_SUN_global_alpha 1)

(defconstant GL_SUN_triangle_list 1)

(defconstant GL_SUN_vertex 1)

(defconstant GL_EXT_blend_func_separate 1)

(defconstant GL_INGR_blend_func_separate 1)

(defconstant GL_INGR_color_clamp 1)

(defconstant GL_INGR_interlace_read 1)

(defconstant GL_EXT_stencil_wrap 1)

(defconstant GL_EXT_422_pixels 1)

(defconstant GL_NV_texgen_reflection 1)

(defconstant GL_SUN_convolution_border_modes 1)

(defconstant GL_EXT_texture_env_add 1)

(defconstant GL_EXT_texture_lod_bias 1)

(defconstant GL_EXT_texture_filter_anisotropic 1)

(defconstant GL_EXT_vertex_weighting 1)

(defconstant GL_NV_light_max_exponent 1)

(defconstant GL_NV_vertex_array_range 1)

(defconstant GL_NV_register_combiners 1)

(defconstant GL_NV_fog_distance 1)

(defconstant GL_NV_texgen_emboss 1)

(defconstant GL_NV_blend_square 1)

(defconstant GL_NV_texture_env_combine4 1)

(defconstant GL_MESA_resize_buffers 1)

(defconstant GL_MESA_window_pos 1)

(defconstant GL_IBM_cull_vertex 1)

(defconstant GL_IBM_multimode_draw_arrays 1)

(defconstant GL_IBM_vertex_array_lists 1)

(defconstant GL_SGIX_subsample 1)

(defconstant GL_SGIX_ycrcba 1)

(defconstant GL_SGIX_ycrcb_subsample 1)

(defconstant GL_SGIX_depth_pass_instrument 1)

(defconstant GL_3DFX_texture_compression_FXT1 1)

(defconstant GL_3DFX_multisample 1)

(defconstant GL_3DFX_tbuffer 1)

(defconstant GL_EXT_multisample 1)

(defconstant GL_SGIX_vertex_preclip 1)

(defconstant GL_SGIX_convolution_accuracy 1)

(defconstant GL_SGIX_resample 1)

(defconstant GL_SGIS_point_line_texgen 1)

(defconstant GL_SGIS_texture_color_mask 1)

(defconstant GL_SGIX_igloo_interface 1)

(defconstant GL_EXT_texture_env_dot3 1)

(defconstant GL_ATI_texture_mirror_once 1)

(defconstant GL_NV_fence 1)

(defconstant GL_NV_evaluators 1)

(defconstant GL_NV_packed_depth_stencil 1)

(defconstant GL_NV_register_combiners2 1)

(defconstant GL_NV_texture_compression_vtc 1)

(defconstant GL_NV_texture_rectangle 1)

(defconstant GL_NV_texture_shader 1)

(defconstant GL_NV_texture_shader2 1)

(defconstant GL_NV_vertex_array_range2 1)

(defconstant GL_NV_vertex_program 1)

(defconstant GL_SGIX_texture_coordinate_clamp 1)

(defconstant GL_SGIX_scalebias_hint 1)

(defconstant GL_OML_interlace 1)

(defconstant GL_OML_subsample 1)

(defconstant GL_OML_resample 1)

(defconstant GL_NV_copy_depth_to_color 1)

(defconstant GL_ATI_envmap_bumpmap 1)

(defconstant GL_ATI_fragment_shader 1)

(defconstant GL_ATI_pn_triangles 1)

(defconstant GL_ATI_vertex_array_object 1)

(defconstant GL_EXT_vertex_shader 1)

(defconstant GL_ATI_vertex_streams 1)

(defconstant GL_ATI_element_array 1)

(defconstant GL_SUN_mesh_array 1)

(defconstant GL_SUN_slice_accum 1)

(defconstant GL_NV_multisample_filter_hint 1)

(defconstant GL_NV_depth_clamp 1)

(defconstant GL_NV_occlusion_query 1)

(defconstant GL_NV_point_sprite 1)

(defconstant GL_NV_texture_shader3 1)

(defconstant GL_NV_vertex_program1_1 1)

(defconstant GL_EXT_shadow_funcs 1)

(defconstant GL_EXT_stencil_two_side 1)

(defconstant GL_ATI_text_fragment_shader 1)

(defconstant GL_APPLE_client_storage 1)

(defconstant GL_APPLE_element_array 1)

(defconstant GL_APPLE_fence 1)

(defconstant GL_APPLE_vertex_array_object 1)

(defconstant GL_APPLE_vertex_array_range 1)

(defconstant GL_APPLE_ycbcr_422 1)

(defconstant GL_S3_s3tc 1)

(defconstant GL_ATI_draw_buffers 1)

(defconstant GL_ATI_pixel_format_float 1)

(defconstant GL_ATI_texture_env_combine3 1)

(defconstant GL_ATI_texture_float 1)

(defconstant GL_NV_float_buffer 1)

(defconstant GL_NV_fragment_program 1)

(defconstant GL_NV_half_float 1)

(defconstant GL_NV_pixel_data_range 1)

(defconstant GL_NV_primitive_restart 1)

(defconstant GL_NV_texture_expand_normal 1)

(defconstant GL_NV_vertex_program2 1)

(defconstant GL_ATI_map_object_buffer 1)

(defconstant GL_ATI_separate_stencil 1)

(defconstant GL_ATI_vertex_attrib_array_object 1)

(defconstant GL_OES_read_format 1)

(defconstant GL_EXT_depth_bounds_test 1)

(defconstant GL_EXT_texture_mirror_clamp 1)

(defconstant GL_EXT_blend_equation_separate 1)

(defconstant GL_MESA_pack_invert 1)

(defconstant GL_MESA_ycbcr_texture 1)

(defconstant GL_EXT_pixel_buffer_object 1)

(defconstant GL_NV_fragment_program_option 1)

(defconstant GL_NV_fragment_program2 1)

(defconstant GL_NV_vertex_program2_option 1)

(defconstant GL_NV_vertex_program3 1)

(defconstant GL_EXT_framebuffer_object 1)

(defconstant GL_GREMEDY_string_marker 1)


