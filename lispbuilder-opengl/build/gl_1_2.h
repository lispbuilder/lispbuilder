
/*
 * OpenGL 1.2
 */

#define GL_RESCALE_NORMAL			0x803A
#define GL_CLAMP_TO_EDGE			0x812F
#define GL_MAX_ELEMENTS_VERTICES		0x80E8
#define GL_MAX_ELEMENTS_INDICES			0x80E9
#define GL_BGR					0x80E0
#define GL_BGRA					0x80E1
#define GL_UNSIGNED_BYTE_3_3_2			0x8032
#define GL_UNSIGNED_BYTE_2_3_3_REV		0x8362
#define GL_UNSIGNED_SHORT_5_6_5			0x8363
#define GL_UNSIGNED_SHORT_5_6_5_REV		0x8364
#define GL_UNSIGNED_SHORT_4_4_4_4		0x8033
#define GL_UNSIGNED_SHORT_4_4_4_4_REV		0x8365
#define GL_UNSIGNED_SHORT_5_5_5_1		0x8034
#define GL_UNSIGNED_SHORT_1_5_5_5_REV		0x8366
#define GL_UNSIGNED_INT_8_8_8_8			0x8035
#define GL_UNSIGNED_INT_8_8_8_8_REV		0x8367
#define GL_UNSIGNED_INT_10_10_10_2		0x8036
#define GL_UNSIGNED_INT_2_10_10_10_REV		0x8368
#define GL_LIGHT_MODEL_COLOR_CONTROL		0x81F8
#define GL_SINGLE_COLOR				0x81F9
#define GL_SEPARATE_SPECULAR_COLOR		0x81FA
#define GL_TEXTURE_MIN_LOD			0x813A
#define GL_TEXTURE_MAX_LOD			0x813B
#define GL_TEXTURE_BASE_LEVEL			0x813C
#define GL_TEXTURE_MAX_LEVEL			0x813D
#define GL_SMOOTH_POINT_SIZE_RANGE		0x0B12
#define GL_SMOOTH_POINT_SIZE_GRANULARITY	0x0B13
#define GL_SMOOTH_LINE_WIDTH_RANGE		0x0B22
#define GL_SMOOTH_LINE_WIDTH_GRANULARITY	0x0B23
#define GL_ALIASED_POINT_SIZE_RANGE		0x846D
#define GL_ALIASED_LINE_WIDTH_RANGE		0x846E
#define GL_PACK_SKIP_IMAGES			0x806B
#define GL_PACK_IMAGE_HEIGHT			0x806C
#define GL_UNPACK_SKIP_IMAGES			0x806D
#define GL_UNPACK_IMAGE_HEIGHT			0x806E
#define GL_TEXTURE_3D				0x806F
#define GL_PROXY_TEXTURE_3D			0x8070
#define GL_TEXTURE_DEPTH			0x8071
#define GL_TEXTURE_WRAP_R			0x8072
#define GL_MAX_3D_TEXTURE_SIZE			0x8073
#define GL_TEXTURE_BINDING_3D			0x806A

GLAPI void GLAPIENTRY glDrawRangeElements( GLenum mode, GLuint start,
	GLuint end, GLsizei count, GLenum type, const GLvoid *indices );

GLAPI void GLAPIENTRY glTexImage3D( GLenum target, GLint level,
                                      GLint internalFormat,
                                      GLsizei width, GLsizei height,
                                      GLsizei depth, GLint border,
                                      GLenum format, GLenum type,
                                      const GLvoid *pixels );

GLAPI void GLAPIENTRY glTexSubImage3D( GLenum target, GLint level,
                                         GLint xoffset, GLint yoffset,
                                         GLint zoffset, GLsizei width,
                                         GLsizei height, GLsizei depth,
                                         GLenum format,
                                         GLenum type, const GLvoid *pixels);

GLAPI void GLAPIENTRY glCopyTexSubImage3D( GLenum target, GLint level,
                                             GLint xoffset, GLint yoffset,
                                             GLint zoffset, GLint x,
                                             GLint y, GLsizei width,
                                             GLsizei height );

typedef void (APIENTRYP PFNGLDRAWRANGEELEMENTSPROC) (GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type, const GLvoid *indices);
typedef void (APIENTRYP PFNGLTEXIMAGE3DPROC) (GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, const GLvoid *pixels);
typedef void (APIENTRYP PFNGLTEXSUBIMAGE3DPROC) (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const GLvoid *pixels);
typedef void (APIENTRYP PFNGLCOPYTEXSUBIMAGE3DPROC) (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height);


/*
 * GL_ARB_imaging
 */

#define GL_CONSTANT_COLOR			0x8001
#define GL_ONE_MINUS_CONSTANT_COLOR		0x8002
#define GL_CONSTANT_ALPHA			0x8003
#define GL_ONE_MINUS_CONSTANT_ALPHA		0x8004
#define GL_COLOR_TABLE				0x80D0
#define GL_POST_CONVOLUTION_COLOR_TABLE		0x80D1
#define GL_POST_COLOR_MATRIX_COLOR_TABLE	0x80D2
#define GL_PROXY_COLOR_TABLE			0x80D3
#define GL_PROXY_POST_CONVOLUTION_COLOR_TABLE	0x80D4
#define GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE	0x80D5
#define GL_COLOR_TABLE_SCALE			0x80D6
#define GL_COLOR_TABLE_BIAS			0x80D7
#define GL_COLOR_TABLE_FORMAT			0x80D8
#define GL_COLOR_TABLE_WIDTH			0x80D9
#define GL_COLOR_TABLE_RED_SIZE			0x80DA
#define GL_COLOR_TABLE_GREEN_SIZE		0x80DB
#define GL_COLOR_TABLE_BLUE_SIZE		0x80DC
#define GL_COLOR_TABLE_ALPHA_SIZE		0x80DD
#define GL_COLOR_TABLE_LUMINANCE_SIZE		0x80DE
#define GL_COLOR_TABLE_INTENSITY_SIZE		0x80DF
#define GL_CONVOLUTION_1D			0x8010
#define GL_CONVOLUTION_2D			0x8011
#define GL_SEPARABLE_2D				0x8012
#define GL_CONVOLUTION_BORDER_MODE		0x8013
#define GL_CONVOLUTION_FILTER_SCALE		0x8014
#define GL_CONVOLUTION_FILTER_BIAS		0x8015
#define GL_REDUCE				0x8016
#define GL_CONVOLUTION_FORMAT			0x8017
#define GL_CONVOLUTION_WIDTH			0x8018
#define GL_CONVOLUTION_HEIGHT			0x8019
#define GL_MAX_CONVOLUTION_WIDTH		0x801A
#define GL_MAX_CONVOLUTION_HEIGHT		0x801B
#define GL_POST_CONVOLUTION_RED_SCALE		0x801C
#define GL_POST_CONVOLUTION_GREEN_SCALE		0x801D
#define GL_POST_CONVOLUTION_BLUE_SCALE		0x801E
#define GL_POST_CONVOLUTION_ALPHA_SCALE		0x801F
#define GL_POST_CONVOLUTION_RED_BIAS		0x8020
#define GL_POST_CONVOLUTION_GREEN_BIAS		0x8021
#define GL_POST_CONVOLUTION_BLUE_BIAS		0x8022
#define GL_POST_CONVOLUTION_ALPHA_BIAS		0x8023
#define GL_CONSTANT_BORDER			0x8151
#define GL_REPLICATE_BORDER			0x8153
#define GL_CONVOLUTION_BORDER_COLOR		0x8154
#define GL_COLOR_MATRIX				0x80B1
#define GL_COLOR_MATRIX_STACK_DEPTH		0x80B2
#define GL_MAX_COLOR_MATRIX_STACK_DEPTH		0x80B3
#define GL_POST_COLOR_MATRIX_RED_SCALE		0x80B4
#define GL_POST_COLOR_MATRIX_GREEN_SCALE	0x80B5
#define GL_POST_COLOR_MATRIX_BLUE_SCALE		0x80B6
#define GL_POST_COLOR_MATRIX_ALPHA_SCALE	0x80B7
#define GL_POST_COLOR_MATRIX_RED_BIAS		0x80B8
#define GL_POST_COLOR_MATRIX_GREEN_BIAS		0x80B9
#define GL_POST_COLOR_MATRIX_BLUE_BIAS		0x80BA
#define GL_POST_COLOR_MATRIX_ALPHA_BIAS		0x80BB
#define GL_HISTOGRAM				0x8024
#define GL_PROXY_HISTOGRAM			0x8025
#define GL_HISTOGRAM_WIDTH			0x8026
#define GL_HISTOGRAM_FORMAT			0x8027
#define GL_HISTOGRAM_RED_SIZE			0x8028
#define GL_HISTOGRAM_GREEN_SIZE			0x8029
#define GL_HISTOGRAM_BLUE_SIZE			0x802A
#define GL_HISTOGRAM_ALPHA_SIZE			0x802B
#define GL_HISTOGRAM_LUMINANCE_SIZE		0x802C
#define GL_HISTOGRAM_SINK			0x802D
#define GL_MINMAX				0x802E
#define GL_MINMAX_FORMAT			0x802F
#define GL_MINMAX_SINK				0x8030
#define GL_TABLE_TOO_LARGE			0x8031
#define GL_BLEND_EQUATION			0x8009
#define GL_MIN					0x8007
#define GL_MAX					0x8008
#define GL_FUNC_ADD				0x8006
#define GL_FUNC_SUBTRACT			0x800A
#define GL_FUNC_REVERSE_SUBTRACT		0x800B
#define GL_BLEND_COLOR				0x8005


GLAPI void GLAPIENTRY glColorTable( GLenum target, GLenum internalformat,
                                    GLsizei width, GLenum format,
                                    GLenum type, const GLvoid *table );

GLAPI void GLAPIENTRY glColorSubTable( GLenum target,
                                       GLsizei start, GLsizei count,
                                       GLenum format, GLenum type,
                                       const GLvoid *data );

GLAPI void GLAPIENTRY glColorTableParameteriv(GLenum target, GLenum pname,
                                              const GLint *params);

GLAPI void GLAPIENTRY glColorTableParameterfv(GLenum target, GLenum pname,
                                              const GLfloat *params);

GLAPI void GLAPIENTRY glCopyColorSubTable( GLenum target, GLsizei start,
                                           GLint x, GLint y, GLsizei width );

GLAPI void GLAPIENTRY glCopyColorTable( GLenum target, GLenum internalformat,
                                        GLint x, GLint y, GLsizei width );

GLAPI void GLAPIENTRY glGetColorTable( GLenum target, GLenum format,
                                       GLenum type, GLvoid *table );

GLAPI void GLAPIENTRY glGetColorTableParameterfv( GLenum target, GLenum pname,
                                                  GLfloat *params );

GLAPI void GLAPIENTRY glGetColorTableParameteriv( GLenum target, GLenum pname,
                                                  GLint *params );

GLAPI void GLAPIENTRY glBlendEquation( GLenum mode );

GLAPI void GLAPIENTRY glBlendColor( GLclampf red, GLclampf green,
                                    GLclampf blue, GLclampf alpha );

GLAPI void GLAPIENTRY glHistogram( GLenum target, GLsizei width,
				   GLenum internalformat, GLboolean sink );

GLAPI void GLAPIENTRY glResetHistogram( GLenum target );

GLAPI void GLAPIENTRY glGetHistogram( GLenum target, GLboolean reset,
				      GLenum format, GLenum type,
				      GLvoid *values );

GLAPI void GLAPIENTRY glGetHistogramParameterfv( GLenum target, GLenum pname,
						 GLfloat *params );

GLAPI void GLAPIENTRY glGetHistogramParameteriv( GLenum target, GLenum pname,
						 GLint *params );

GLAPI void GLAPIENTRY glMinmax( GLenum target, GLenum internalformat,
				GLboolean sink );

GLAPI void GLAPIENTRY glResetMinmax( GLenum target );

GLAPI void GLAPIENTRY glGetMinmax( GLenum target, GLboolean reset,
                                   GLenum format, GLenum types,
                                   GLvoid *values );

GLAPI void GLAPIENTRY glGetMinmaxParameterfv( GLenum target, GLenum pname,
					      GLfloat *params );

GLAPI void GLAPIENTRY glGetMinmaxParameteriv( GLenum target, GLenum pname,
					      GLint *params );

GLAPI void GLAPIENTRY glConvolutionFilter1D( GLenum target,
	GLenum internalformat, GLsizei width, GLenum format, GLenum type,
	const GLvoid *image );

GLAPI void GLAPIENTRY glConvolutionFilter2D( GLenum target,
	GLenum internalformat, GLsizei width, GLsizei height, GLenum format,
	GLenum type, const GLvoid *image );

GLAPI void GLAPIENTRY glConvolutionParameterf( GLenum target, GLenum pname,
	GLfloat params );

GLAPI void GLAPIENTRY glConvolutionParameterfv( GLenum target, GLenum pname,
	const GLfloat *params );

GLAPI void GLAPIENTRY glConvolutionParameteri( GLenum target, GLenum pname,
	GLint params );

GLAPI void GLAPIENTRY glConvolutionParameteriv( GLenum target, GLenum pname,
	const GLint *params );

GLAPI void GLAPIENTRY glCopyConvolutionFilter1D( GLenum target,
	GLenum internalformat, GLint x, GLint y, GLsizei width );

GLAPI void GLAPIENTRY glCopyConvolutionFilter2D( GLenum target,
	GLenum internalformat, GLint x, GLint y, GLsizei width,
	GLsizei height);

GLAPI void GLAPIENTRY glGetConvolutionFilter( GLenum target, GLenum format,
	GLenum type, GLvoid *image );

GLAPI void GLAPIENTRY glGetConvolutionParameterfv( GLenum target, GLenum pname,
	GLfloat *params );

GLAPI void GLAPIENTRY glGetConvolutionParameteriv( GLenum target, GLenum pname,
	GLint *params );

GLAPI void GLAPIENTRY glSeparableFilter2D( GLenum target,
	GLenum internalformat, GLsizei width, GLsizei height, GLenum format,
	GLenum type, const GLvoid *row, const GLvoid *column );

GLAPI void GLAPIENTRY glGetSeparableFilter( GLenum target, GLenum format,
	GLenum type, GLvoid *row, GLvoid *column, GLvoid *span );

typedef void (APIENTRYP PFNGLBLENDCOLORPROC) (GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
typedef void (APIENTRYP PFNGLBLENDEQUATIONPROC) (GLenum mode);
typedef void (APIENTRYP PFNGLCOLORTABLEPROC) (GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type, const GLvoid *table);
typedef void (APIENTRYP PFNGLCOLORTABLEPARAMETERFVPROC) (GLenum target, GLenum pname, const GLfloat *params);
typedef void (APIENTRYP PFNGLCOLORTABLEPARAMETERIVPROC) (GLenum target, GLenum pname, const GLint *params);
typedef void (APIENTRYP PFNGLCOPYCOLORTABLEPROC) (GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width);
typedef void (APIENTRYP PFNGLGETCOLORTABLEPROC) (GLenum target, GLenum format, GLenum type, GLvoid *table);
typedef void (APIENTRYP PFNGLGETCOLORTABLEPARAMETERFVPROC) (GLenum target, GLenum pname, GLfloat *params);
typedef void (APIENTRYP PFNGLGETCOLORTABLEPARAMETERIVPROC) (GLenum target, GLenum pname, GLint *params);
typedef void (APIENTRYP PFNGLCOLORSUBTABLEPROC) (GLenum target, GLsizei start, GLsizei count, GLenum format, GLenum type, const GLvoid *data);
typedef void (APIENTRYP PFNGLCOPYCOLORSUBTABLEPROC) (GLenum target, GLsizei start, GLint x, GLint y, GLsizei width);
typedef void (APIENTRYP PFNGLCONVOLUTIONFILTER1DPROC) (GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type, const GLvoid *image);
typedef void (APIENTRYP PFNGLCONVOLUTIONFILTER2DPROC) (GLenum target, GLenum internalformat, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *image);
typedef void (APIENTRYP PFNGLCONVOLUTIONPARAMETERFPROC) (GLenum target, GLenum pname, GLfloat params);
typedef void (APIENTRYP PFNGLCONVOLUTIONPARAMETERFVPROC) (GLenum target, GLenum pname, const GLfloat *params);
typedef void (APIENTRYP PFNGLCONVOLUTIONPARAMETERIPROC) (GLenum target, GLenum pname, GLint params);
typedef void (APIENTRYP PFNGLCONVOLUTIONPARAMETERIVPROC) (GLenum target, GLenum pname, const GLint *params);
typedef void (APIENTRYP PFNGLCOPYCONVOLUTIONFILTER1DPROC) (GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width);
typedef void (APIENTRYP PFNGLCOPYCONVOLUTIONFILTER2DPROC) (GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height);
typedef void (APIENTRYP PFNGLGETCONVOLUTIONFILTERPROC) (GLenum target, GLenum format, GLenum type, GLvoid *image);
typedef void (APIENTRYP PFNGLGETCONVOLUTIONPARAMETERFVPROC) (GLenum target, GLenum pname, GLfloat *params);
typedef void (APIENTRYP PFNGLGETCONVOLUTIONPARAMETERIVPROC) (GLenum target, GLenum pname, GLint *params);
typedef void (APIENTRYP PFNGLGETSEPARABLEFILTERPROC) (GLenum target, GLenum format, GLenum type, GLvoid *row, GLvoid *column, GLvoid *span);
typedef void (APIENTRYP PFNGLSEPARABLEFILTER2DPROC) (GLenum target, GLenum internalformat, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *row, const GLvoid *column);
typedef void (APIENTRYP PFNGLGETHISTOGRAMPROC) (GLenum target, GLboolean reset, GLenum format, GLenum type, GLvoid *values);
typedef void (APIENTRYP PFNGLGETHISTOGRAMPARAMETERFVPROC) (GLenum target, GLenum pname, GLfloat *params);
typedef void (APIENTRYP PFNGLGETHISTOGRAMPARAMETERIVPROC) (GLenum target, GLenum pname, GLint *params);
typedef void (APIENTRYP PFNGLGETMINMAXPROC) (GLenum target, GLboolean reset, GLenum format, GLenum type, GLvoid *values);
typedef void (APIENTRYP PFNGLGETMINMAXPARAMETERFVPROC) (GLenum target, GLenum pname, GLfloat *params);
typedef void (APIENTRYP PFNGLGETMINMAXPARAMETERIVPROC) (GLenum target, GLenum pname, GLint *params);
typedef void (APIENTRYP PFNGLHISTOGRAMPROC) (GLenum target, GLsizei width, GLenum internalformat, GLboolean sink);
typedef void (APIENTRYP PFNGLMINMAXPROC) (GLenum target, GLenum internalformat, GLboolean sink);
typedef void (APIENTRYP PFNGLRESETHISTOGRAMPROC) (GLenum target);
typedef void (APIENTRYP PFNGLRESETMINMAXPROC) (GLenum target);


