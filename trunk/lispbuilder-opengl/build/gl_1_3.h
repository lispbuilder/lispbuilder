
/*
 * OpenGL 1.3
 */

/* multitexture */
#define GL_TEXTURE0				0x84C0
#define GL_TEXTURE1				0x84C1
#define GL_TEXTURE2				0x84C2
#define GL_TEXTURE3				0x84C3
#define GL_TEXTURE4				0x84C4
#define GL_TEXTURE5				0x84C5
#define GL_TEXTURE6				0x84C6
#define GL_TEXTURE7				0x84C7
#define GL_TEXTURE8				0x84C8
#define GL_TEXTURE9				0x84C9
#define GL_TEXTURE10				0x84CA
#define GL_TEXTURE11				0x84CB
#define GL_TEXTURE12				0x84CC
#define GL_TEXTURE13				0x84CD
#define GL_TEXTURE14				0x84CE
#define GL_TEXTURE15				0x84CF
#define GL_TEXTURE16				0x84D0
#define GL_TEXTURE17				0x84D1
#define GL_TEXTURE18				0x84D2
#define GL_TEXTURE19				0x84D3
#define GL_TEXTURE20				0x84D4
#define GL_TEXTURE21				0x84D5
#define GL_TEXTURE22				0x84D6
#define GL_TEXTURE23				0x84D7
#define GL_TEXTURE24				0x84D8
#define GL_TEXTURE25				0x84D9
#define GL_TEXTURE26				0x84DA
#define GL_TEXTURE27				0x84DB
#define GL_TEXTURE28				0x84DC
#define GL_TEXTURE29				0x84DD
#define GL_TEXTURE30				0x84DE
#define GL_TEXTURE31				0x84DF
#define GL_ACTIVE_TEXTURE			0x84E0
#define GL_CLIENT_ACTIVE_TEXTURE		0x84E1
#define GL_MAX_TEXTURE_UNITS			0x84E2
/* texture_cube_map */
#define GL_NORMAL_MAP				0x8511
#define GL_REFLECTION_MAP			0x8512
#define GL_TEXTURE_CUBE_MAP			0x8513
#define GL_TEXTURE_BINDING_CUBE_MAP		0x8514
#define GL_TEXTURE_CUBE_MAP_POSITIVE_X		0x8515
#define GL_TEXTURE_CUBE_MAP_NEGATIVE_X		0x8516
#define GL_TEXTURE_CUBE_MAP_POSITIVE_Y		0x8517
#define GL_TEXTURE_CUBE_MAP_NEGATIVE_Y		0x8518
#define GL_TEXTURE_CUBE_MAP_POSITIVE_Z		0x8519
#define GL_TEXTURE_CUBE_MAP_NEGATIVE_Z		0x851A
#define GL_PROXY_TEXTURE_CUBE_MAP		0x851B
#define GL_MAX_CUBE_MAP_TEXTURE_SIZE		0x851C
/* texture_compression */
#define GL_COMPRESSED_ALPHA			0x84E9
#define GL_COMPRESSED_LUMINANCE			0x84EA
#define GL_COMPRESSED_LUMINANCE_ALPHA		0x84EB
#define GL_COMPRESSED_INTENSITY			0x84EC
#define GL_COMPRESSED_RGB			0x84ED
#define GL_COMPRESSED_RGBA			0x84EE
#define GL_TEXTURE_COMPRESSION_HINT		0x84EF
#define GL_TEXTURE_COMPRESSED_IMAGE_SIZE	0x86A0
#define GL_TEXTURE_COMPRESSED			0x86A1
#define GL_NUM_COMPRESSED_TEXTURE_FORMATS	0x86A2
#define GL_COMPRESSED_TEXTURE_FORMATS		0x86A3
/* multisample */
#define GL_MULTISAMPLE				0x809D
#define GL_SAMPLE_ALPHA_TO_COVERAGE		0x809E
#define GL_SAMPLE_ALPHA_TO_ONE			0x809F
#define GL_SAMPLE_COVERAGE			0x80A0
#define GL_SAMPLE_BUFFERS			0x80A8
#define GL_SAMPLES				0x80A9
#define GL_SAMPLE_COVERAGE_VALUE		0x80AA
#define GL_SAMPLE_COVERAGE_INVERT		0x80AB
#define GL_MULTISAMPLE_BIT			0x20000000
/* transpose_matrix */
#define GL_TRANSPOSE_MODELVIEW_MATRIX		0x84E3
#define GL_TRANSPOSE_PROJECTION_MATRIX		0x84E4
#define GL_TRANSPOSE_TEXTURE_MATRIX		0x84E5
#define GL_TRANSPOSE_COLOR_MATRIX		0x84E6
/* texture_env_combine */
#define GL_COMBINE				0x8570
#define GL_COMBINE_RGB				0x8571
#define GL_COMBINE_ALPHA			0x8572
#define GL_SOURCE0_RGB				0x8580
#define GL_SOURCE1_RGB				0x8581
#define GL_SOURCE2_RGB				0x8582
#define GL_SOURCE0_ALPHA			0x8588
#define GL_SOURCE1_ALPHA			0x8589
#define GL_SOURCE2_ALPHA			0x858A
#define GL_OPERAND0_RGB				0x8590
#define GL_OPERAND1_RGB				0x8591
#define GL_OPERAND2_RGB				0x8592
#define GL_OPERAND0_ALPHA			0x8598
#define GL_OPERAND1_ALPHA			0x8599
#define GL_OPERAND2_ALPHA			0x859A
#define GL_RGB_SCALE				0x8573
#define GL_ADD_SIGNED				0x8574
#define GL_INTERPOLATE				0x8575
#define GL_SUBTRACT				0x84E7
#define GL_CONSTANT				0x8576
#define GL_PRIMARY_COLOR			0x8577
#define GL_PREVIOUS				0x8578
/* texture_env_dot3 */
#define GL_DOT3_RGB				0x86AE
#define GL_DOT3_RGBA				0x86AF
/* texture_border_clamp */
#define GL_CLAMP_TO_BORDER			0x812D

GLAPI void GLAPIENTRY glActiveTexture( GLenum texture );

GLAPI void GLAPIENTRY glClientActiveTexture( GLenum texture );

GLAPI void GLAPIENTRY glCompressedTexImage1D( GLenum target, GLint level, GLenum internalformat, GLsizei width, GLint border, GLsizei imageSize, const GLvoid *data );

GLAPI void GLAPIENTRY glCompressedTexImage2D( GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize, const GLvoid *data );

GLAPI void GLAPIENTRY glCompressedTexImage3D( GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLsizei imageSize, const GLvoid *data );

GLAPI void GLAPIENTRY glCompressedTexSubImage1D( GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLsizei imageSize, const GLvoid *data );

GLAPI void GLAPIENTRY glCompressedTexSubImage2D( GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, const GLvoid *data );

GLAPI void GLAPIENTRY glCompressedTexSubImage3D( GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLsizei imageSize, const GLvoid *data );

GLAPI void GLAPIENTRY glGetCompressedTexImage( GLenum target, GLint lod, GLvoid *img );

GLAPI void GLAPIENTRY glMultiTexCoord1d( GLenum target, GLdouble s );

GLAPI void GLAPIENTRY glMultiTexCoord1dv( GLenum target, const GLdouble *v );

GLAPI void GLAPIENTRY glMultiTexCoord1f( GLenum target, GLfloat s );

GLAPI void GLAPIENTRY glMultiTexCoord1fv( GLenum target, const GLfloat *v );

GLAPI void GLAPIENTRY glMultiTexCoord1i( GLenum target, GLint s );

GLAPI void GLAPIENTRY glMultiTexCoord1iv( GLenum target, const GLint *v );

GLAPI void GLAPIENTRY glMultiTexCoord1s( GLenum target, GLshort s );

GLAPI void GLAPIENTRY glMultiTexCoord1sv( GLenum target, const GLshort *v );

GLAPI void GLAPIENTRY glMultiTexCoord2d( GLenum target, GLdouble s, GLdouble t );

GLAPI void GLAPIENTRY glMultiTexCoord2dv( GLenum target, const GLdouble *v );

GLAPI void GLAPIENTRY glMultiTexCoord2f( GLenum target, GLfloat s, GLfloat t );

GLAPI void GLAPIENTRY glMultiTexCoord2fv( GLenum target, const GLfloat *v );

GLAPI void GLAPIENTRY glMultiTexCoord2i( GLenum target, GLint s, GLint t );

GLAPI void GLAPIENTRY glMultiTexCoord2iv( GLenum target, const GLint *v );

GLAPI void GLAPIENTRY glMultiTexCoord2s( GLenum target, GLshort s, GLshort t );

GLAPI void GLAPIENTRY glMultiTexCoord2sv( GLenum target, const GLshort *v );

GLAPI void GLAPIENTRY glMultiTexCoord3d( GLenum target, GLdouble s, GLdouble t, GLdouble r );

GLAPI void GLAPIENTRY glMultiTexCoord3dv( GLenum target, const GLdouble *v );

GLAPI void GLAPIENTRY glMultiTexCoord3f( GLenum target, GLfloat s, GLfloat t, GLfloat r );

GLAPI void GLAPIENTRY glMultiTexCoord3fv( GLenum target, const GLfloat *v );

GLAPI void GLAPIENTRY glMultiTexCoord3i( GLenum target, GLint s, GLint t, GLint r );

GLAPI void GLAPIENTRY glMultiTexCoord3iv( GLenum target, const GLint *v );

GLAPI void GLAPIENTRY glMultiTexCoord3s( GLenum target, GLshort s, GLshort t, GLshort r );

GLAPI void GLAPIENTRY glMultiTexCoord3sv( GLenum target, const GLshort *v );

GLAPI void GLAPIENTRY glMultiTexCoord4d( GLenum target, GLdouble s, GLdouble t, GLdouble r, GLdouble q );

GLAPI void GLAPIENTRY glMultiTexCoord4dv( GLenum target, const GLdouble *v );

GLAPI void GLAPIENTRY glMultiTexCoord4f( GLenum target, GLfloat s, GLfloat t, GLfloat r, GLfloat q );

GLAPI void GLAPIENTRY glMultiTexCoord4fv( GLenum target, const GLfloat *v );

GLAPI void GLAPIENTRY glMultiTexCoord4i( GLenum target, GLint s, GLint t, GLint r, GLint q );

GLAPI void GLAPIENTRY glMultiTexCoord4iv( GLenum target, const GLint *v );

GLAPI void GLAPIENTRY glMultiTexCoord4s( GLenum target, GLshort s, GLshort t, GLshort r, GLshort q );

GLAPI void GLAPIENTRY glMultiTexCoord4sv( GLenum target, const GLshort *v );


GLAPI void GLAPIENTRY glLoadTransposeMatrixd( const GLdouble m[16] );

GLAPI void GLAPIENTRY glLoadTransposeMatrixf( const GLfloat m[16] );

GLAPI void GLAPIENTRY glMultTransposeMatrixd( const GLdouble m[16] );

GLAPI void GLAPIENTRY glMultTransposeMatrixf( const GLfloat m[16] );

GLAPI void GLAPIENTRY glSampleCoverage( GLclampf value, GLboolean invert );

typedef void (APIENTRYP PFNGLACTIVETEXTUREPROC) (GLenum texture);
typedef void (APIENTRYP PFNGLCLIENTACTIVETEXTUREPROC) (GLenum texture);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1DPROC) (GLenum target, GLdouble s);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1DVPROC) (GLenum target, const GLdouble *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1FPROC) (GLenum target, GLfloat s);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1FVPROC) (GLenum target, const GLfloat *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1IPROC) (GLenum target, GLint s);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1IVPROC) (GLenum target, const GLint *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1SPROC) (GLenum target, GLshort s);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1SVPROC) (GLenum target, const GLshort *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2DPROC) (GLenum target, GLdouble s, GLdouble t);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2DVPROC) (GLenum target, const GLdouble *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2FPROC) (GLenum target, GLfloat s, GLfloat t);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2FVPROC) (GLenum target, const GLfloat *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2IPROC) (GLenum target, GLint s, GLint t);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2IVPROC) (GLenum target, const GLint *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2SPROC) (GLenum target, GLshort s, GLshort t);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2SVPROC) (GLenum target, const GLshort *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3DPROC) (GLenum target, GLdouble s, GLdouble t, GLdouble r);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3DVPROC) (GLenum target, const GLdouble *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3FPROC) (GLenum target, GLfloat s, GLfloat t, GLfloat r);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3FVPROC) (GLenum target, const GLfloat *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3IPROC) (GLenum target, GLint s, GLint t, GLint r);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3IVPROC) (GLenum target, const GLint *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3SPROC) (GLenum target, GLshort s, GLshort t, GLshort r);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3SVPROC) (GLenum target, const GLshort *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4DPROC) (GLenum target, GLdouble s, GLdouble t, GLdouble r, GLdouble q);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4DVPROC) (GLenum target, const GLdouble *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4FPROC) (GLenum target, GLfloat s, GLfloat t, GLfloat r, GLfloat q);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4FVPROC) (GLenum target, const GLfloat *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4IPROC) (GLenum target, GLint s, GLint t, GLint r, GLint q);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4IVPROC) (GLenum target, const GLint *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4SPROC) (GLenum target, GLshort s, GLshort t, GLshort r, GLshort q);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4SVPROC) (GLenum target, const GLshort *v);
typedef void (APIENTRYP PFNGLLOADTRANSPOSEMATRIXFPROC) (const GLfloat *m);
typedef void (APIENTRYP PFNGLLOADTRANSPOSEMATRIXDPROC) (const GLdouble *m);
typedef void (APIENTRYP PFNGLMULTTRANSPOSEMATRIXFPROC) (const GLfloat *m);
typedef void (APIENTRYP PFNGLMULTTRANSPOSEMATRIXDPROC) (const GLdouble *m);
typedef void (APIENTRYP PFNGLSAMPLECOVERAGEPROC) (GLclampf value, GLboolean invert);
typedef void (APIENTRYP PFNGLCOMPRESSEDTEXIMAGE3DPROC) (GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLsizei imageSize, const GLvoid *data);
typedef void (APIENTRYP PFNGLCOMPRESSEDTEXIMAGE2DPROC) (GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize, const GLvoid *data);
typedef void (APIENTRYP PFNGLCOMPRESSEDTEXIMAGE1DPROC) (GLenum target, GLint level, GLenum internalformat, GLsizei width, GLint border, GLsizei imageSize, const GLvoid *data);
typedef void (APIENTRYP PFNGLCOMPRESSEDTEXSUBIMAGE3DPROC) (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLsizei imageSize, const GLvoid *data);
typedef void (APIENTRYP PFNGLCOMPRESSEDTEXSUBIMAGE2DPROC) (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, const GLvoid *data);
typedef void (APIENTRYP PFNGLCOMPRESSEDTEXSUBIMAGE1DPROC) (GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLsizei imageSize, const GLvoid *data);
typedef void (APIENTRYP PFNGLGETCOMPRESSEDTEXIMAGEPROC) (GLenum target, GLint level, void *img);


/*
 * GL_ARB_multitexture (ARB extension 1 and OpenGL 1.2.1)
 */
#ifndef GL_ARB_multitexture
#define GL_ARB_multitexture 1

#define GL_TEXTURE0_ARB				0x84C0
#define GL_TEXTURE1_ARB				0x84C1
#define GL_TEXTURE2_ARB				0x84C2
#define GL_TEXTURE3_ARB				0x84C3
#define GL_TEXTURE4_ARB				0x84C4
#define GL_TEXTURE5_ARB				0x84C5
#define GL_TEXTURE6_ARB				0x84C6
#define GL_TEXTURE7_ARB				0x84C7
#define GL_TEXTURE8_ARB				0x84C8
#define GL_TEXTURE9_ARB				0x84C9
#define GL_TEXTURE10_ARB			0x84CA
#define GL_TEXTURE11_ARB			0x84CB
#define GL_TEXTURE12_ARB			0x84CC
#define GL_TEXTURE13_ARB			0x84CD
#define GL_TEXTURE14_ARB			0x84CE
#define GL_TEXTURE15_ARB			0x84CF
#define GL_TEXTURE16_ARB			0x84D0
#define GL_TEXTURE17_ARB			0x84D1
#define GL_TEXTURE18_ARB			0x84D2
#define GL_TEXTURE19_ARB			0x84D3
#define GL_TEXTURE20_ARB			0x84D4
#define GL_TEXTURE21_ARB			0x84D5
#define GL_TEXTURE22_ARB			0x84D6
#define GL_TEXTURE23_ARB			0x84D7
#define GL_TEXTURE24_ARB			0x84D8
#define GL_TEXTURE25_ARB			0x84D9
#define GL_TEXTURE26_ARB			0x84DA
#define GL_TEXTURE27_ARB			0x84DB
#define GL_TEXTURE28_ARB			0x84DC
#define GL_TEXTURE29_ARB			0x84DD
#define GL_TEXTURE30_ARB			0x84DE
#define GL_TEXTURE31_ARB			0x84DF
#define GL_ACTIVE_TEXTURE_ARB			0x84E0
#define GL_CLIENT_ACTIVE_TEXTURE_ARB		0x84E1
#define GL_MAX_TEXTURE_UNITS_ARB		0x84E2

GLAPI void GLAPIENTRY glActiveTextureARB(GLenum texture);
GLAPI void GLAPIENTRY glClientActiveTextureARB(GLenum texture);
GLAPI void GLAPIENTRY glMultiTexCoord1dARB(GLenum target, GLdouble s);
GLAPI void GLAPIENTRY glMultiTexCoord1dvARB(GLenum target, const GLdouble *v);
GLAPI void GLAPIENTRY glMultiTexCoord1fARB(GLenum target, GLfloat s);
GLAPI void GLAPIENTRY glMultiTexCoord1fvARB(GLenum target, const GLfloat *v);
GLAPI void GLAPIENTRY glMultiTexCoord1iARB(GLenum target, GLint s);
GLAPI void GLAPIENTRY glMultiTexCoord1ivARB(GLenum target, const GLint *v);
GLAPI void GLAPIENTRY glMultiTexCoord1sARB(GLenum target, GLshort s);
GLAPI void GLAPIENTRY glMultiTexCoord1svARB(GLenum target, const GLshort *v);
GLAPI void GLAPIENTRY glMultiTexCoord2dARB(GLenum target, GLdouble s, GLdouble t);
GLAPI void GLAPIENTRY glMultiTexCoord2dvARB(GLenum target, const GLdouble *v);
GLAPI void GLAPIENTRY glMultiTexCoord2fARB(GLenum target, GLfloat s, GLfloat t);
GLAPI void GLAPIENTRY glMultiTexCoord2fvARB(GLenum target, const GLfloat *v);
GLAPI void GLAPIENTRY glMultiTexCoord2iARB(GLenum target, GLint s, GLint t);
GLAPI void GLAPIENTRY glMultiTexCoord2ivARB(GLenum target, const GLint *v);
GLAPI void GLAPIENTRY glMultiTexCoord2sARB(GLenum target, GLshort s, GLshort t);
GLAPI void GLAPIENTRY glMultiTexCoord2svARB(GLenum target, const GLshort *v);
GLAPI void GLAPIENTRY glMultiTexCoord3dARB(GLenum target, GLdouble s, GLdouble t, GLdouble r);
GLAPI void GLAPIENTRY glMultiTexCoord3dvARB(GLenum target, const GLdouble *v);
GLAPI void GLAPIENTRY glMultiTexCoord3fARB(GLenum target, GLfloat s, GLfloat t, GLfloat r);
GLAPI void GLAPIENTRY glMultiTexCoord3fvARB(GLenum target, const GLfloat *v);
GLAPI void GLAPIENTRY glMultiTexCoord3iARB(GLenum target, GLint s, GLint t, GLint r);
GLAPI void GLAPIENTRY glMultiTexCoord3ivARB(GLenum target, const GLint *v);
GLAPI void GLAPIENTRY glMultiTexCoord3sARB(GLenum target, GLshort s, GLshort t, GLshort r);
GLAPI void GLAPIENTRY glMultiTexCoord3svARB(GLenum target, const GLshort *v);
GLAPI void GLAPIENTRY glMultiTexCoord4dARB(GLenum target, GLdouble s, GLdouble t, GLdouble r, GLdouble q);
GLAPI void GLAPIENTRY glMultiTexCoord4dvARB(GLenum target, const GLdouble *v);
GLAPI void GLAPIENTRY glMultiTexCoord4fARB(GLenum target, GLfloat s, GLfloat t, GLfloat r, GLfloat q);
GLAPI void GLAPIENTRY glMultiTexCoord4fvARB(GLenum target, const GLfloat *v);
GLAPI void GLAPIENTRY glMultiTexCoord4iARB(GLenum target, GLint s, GLint t, GLint r, GLint q);
GLAPI void GLAPIENTRY glMultiTexCoord4ivARB(GLenum target, const GLint *v);
GLAPI void GLAPIENTRY glMultiTexCoord4sARB(GLenum target, GLshort s, GLshort t, GLshort r, GLshort q);
GLAPI void GLAPIENTRY glMultiTexCoord4svARB(GLenum target, const GLshort *v);

typedef void (APIENTRYP PFNGLACTIVETEXTUREARBPROC) (GLenum texture);
typedef void (APIENTRYP PFNGLCLIENTACTIVETEXTUREARBPROC) (GLenum texture);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1DARBPROC) (GLenum target, GLdouble s);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1DVARBPROC) (GLenum target, const GLdouble *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1FARBPROC) (GLenum target, GLfloat s);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1FVARBPROC) (GLenum target, const GLfloat *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1IARBPROC) (GLenum target, GLint s);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1IVARBPROC) (GLenum target, const GLint *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1SARBPROC) (GLenum target, GLshort s);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1SVARBPROC) (GLenum target, const GLshort *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2DARBPROC) (GLenum target, GLdouble s, GLdouble t);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2DVARBPROC) (GLenum target, const GLdouble *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2FARBPROC) (GLenum target, GLfloat s, GLfloat t);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2FVARBPROC) (GLenum target, const GLfloat *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2IARBPROC) (GLenum target, GLint s, GLint t);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2IVARBPROC) (GLenum target, const GLint *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2SARBPROC) (GLenum target, GLshort s, GLshort t);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2SVARBPROC) (GLenum target, const GLshort *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3DARBPROC) (GLenum target, GLdouble s, GLdouble t, GLdouble r);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3DVARBPROC) (GLenum target, const GLdouble *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3FARBPROC) (GLenum target, GLfloat s, GLfloat t, GLfloat r);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3FVARBPROC) (GLenum target, const GLfloat *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3IARBPROC) (GLenum target, GLint s, GLint t, GLint r);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3IVARBPROC) (GLenum target, const GLint *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3SARBPROC) (GLenum target, GLshort s, GLshort t, GLshort r);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3SVARBPROC) (GLenum target, const GLshort *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4DARBPROC) (GLenum target, GLdouble s, GLdouble t, GLdouble r, GLdouble q);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4DVARBPROC) (GLenum target, const GLdouble *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4FARBPROC) (GLenum target, GLfloat s, GLfloat t, GLfloat r, GLfloat q);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4FVARBPROC) (GLenum target, const GLfloat *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4IARBPROC) (GLenum target, GLint s, GLint t, GLint r, GLint q);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4IVARBPROC) (GLenum target, const GLint *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4SARBPROC) (GLenum target, GLshort s, GLshort t, GLshort r, GLshort q);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4SVARBPROC) (GLenum target, const GLshort *v);

#endif /* GL_ARB_multitexture */



/*
 * Define this token if you want "old-style" header file behaviour (extensions
 * defined in gl.h).  Otherwise, extensions will be included from glext.h.
 */
#if defined(GL_GLEXT_LEGACY)

/* All extensions that used to be here are now found in glext.h */

#else  /* GL_GLEXT_LEGACY */

#include <GL/glext.h>

#endif  /* GL_GLEXT_LEGACY */



/*
 * ???. GL_MESA_trace
 * XXX obsolete
 */
#ifndef GL_MESA_trace
#define GL_MESA_trace 1

#define GL_TRACE_ALL_BITS_MESA			0xFFFF
#define GL_TRACE_OPERATIONS_BIT_MESA		0x0001
#define GL_TRACE_PRIMITIVES_BIT_MESA		0x0002
#define GL_TRACE_ARRAYS_BIT_MESA		0x0004
#define GL_TRACE_TEXTURES_BIT_MESA		0x0008
#define GL_TRACE_PIXELS_BIT_MESA		0x0010
#define GL_TRACE_ERRORS_BIT_MESA		0x0020
#define GL_TRACE_MASK_MESA			0x8755
#define GL_TRACE_NAME_MESA			0x8756

GLAPI void GLAPIENTRY glEnableTraceMESA( GLbitfield mask );
GLAPI void GLAPIENTRY glDisableTraceMESA( GLbitfield mask );
GLAPI void GLAPIENTRY glNewTraceMESA( GLbitfield mask, const GLubyte * traceName );
GLAPI void GLAPIENTRY glEndTraceMESA( void );
GLAPI void GLAPIENTRY glTraceAssertAttribMESA( GLbitfield attribMask );
GLAPI void GLAPIENTRY glTraceCommentMESA( const GLubyte * comment );
GLAPI void GLAPIENTRY glTraceTextureMESA( GLuint name, const GLubyte* comment );
GLAPI void GLAPIENTRY glTraceListMESA( GLuint name, const GLubyte* comment );
GLAPI void GLAPIENTRY glTracePointerMESA( GLvoid* pointer, const GLubyte* comment );
GLAPI void GLAPIENTRY glTracePointerRangeMESA( const GLvoid* first, const GLvoid* last, const GLubyte* comment );

#endif /* GL_MESA_trace */


/*
 * ???. GL_MESA_packed_depth_stencil
 * XXX obsolete
 */
#ifndef GL_MESA_packed_depth_stencil
#define GL_MESA_packed_depth_stencil 1

#define GL_DEPTH_STENCIL_MESA			0x8750
#define GL_UNSIGNED_INT_24_8_MESA		0x8751
#define GL_UNSIGNED_INT_8_24_REV_MESA		0x8752
#define GL_UNSIGNED_SHORT_15_1_MESA		0x8753
#define GL_UNSIGNED_SHORT_1_15_REV_MESA		0x8754

#endif /* GL_MESA_packed_depth_stencil */


#ifndef GL_MESA_program_debug
#define GL_MESA_program_debug 1

#define GL_FRAGMENT_PROGRAM_POSITION_MESA       0x8bb0
#define GL_FRAGMENT_PROGRAM_CALLBACK_MESA       0x8bb1
#define GL_FRAGMENT_PROGRAM_CALLBACK_FUNC_MESA  0x8bb2
#define GL_FRAGMENT_PROGRAM_CALLBACK_DATA_MESA  0x8bb3
#define GL_VERTEX_PROGRAM_POSITION_MESA         0x8bb4
#define GL_VERTEX_PROGRAM_CALLBACK_MESA         0x8bb5
#define GL_VERTEX_PROGRAM_CALLBACK_FUNC_MESA    0x8bb6
#define GL_VERTEX_PROGRAM_CALLBACK_DATA_MESA    0x8bb7

typedef void (*GLprogramcallbackMESA)(GLenum target, GLvoid *data);

GLAPI void GLAPIENTRY glProgramCallbackMESA(GLenum target, GLprogramcallbackMESA callback, GLvoid *data);

GLAPI void GLAPIENTRY glGetProgramRegisterfvMESA(GLenum target, GLsizei len, const GLubyte *name, GLfloat *v);

#endif /* GL_MESA_program_debug */


#ifndef GL_ATI_blend_equation_separate
#define GL_ATI_blend_equation_separate 1

#define GL_ALPHA_BLEND_EQUATION_ATI	        0x883D

GLAPI void GLAPIENTRY glBlendEquationSeparateATI( GLenum modeRGB, GLenum modeA );
typedef void (APIENTRYP PFNGLBLENDEQUATIONSEPARATEATIPROC) (GLenum modeRGB, GLenum modeA);

#endif /* GL_ATI_blend_equation_separate */

