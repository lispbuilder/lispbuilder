
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


