%insert("lisphead") 
%{
;;;; OpenRM CFFI lisp wrapper
;;;; Part of the CL-Gardeners project
;;;; http://wiki.alu.org/Application_Builder
;;;; (C)2006 Luke J Crook
;;;; See COPYING for license

(in-package :lispbuilder-openrm)

;;;; Macro to handle defenum (thanks to Frank Buss for this SWIG/CFFI feature
; this handles anonymous enum types differently

(defmacro defenum (&body enums)	
 `(progn ,@(loop for value in enums
                 for index = 0 then (1+ index)
                 when (listp value) do (setf index (second value)
                                             value (first value))
                 collect `(defconstant ,value ,index))))

;;;; "rmdefs.h"
;; /* macros */
;; //#define RM_MIN(a,b) 		 ((a) < (b) ? (a) : (b))
(defun RM_MIN (a b)
  (if (< a b)
      a
      b))

;; //#define RM_MAX(a,b) 		 ((a) > (b) ? (a) : (b))
(defun RM_MAX (a b)
  (if (> a b)
      a
      b))

;; //#define RM_DEGREES_TO_RADIANS(a) ((a)*0.017453292)
(defun RM_DEGREES_TO_RADIANS (a)
  (* a 0.017453292))

;; //#define RM_RADIANS_TO_DEGREES(a) ((a) * 57.29577951)
(defun RM_RADIANS_TO_DEGREES (a)
  (* a 57.29577951))

;;  "rmcmpmgr.h"
(defconstant PAGE_SIZE_BITS 12);; /* 2^12 = 4096 */

(defun NUM_PAGES_BITS ()
  (- 32 PAGE_SIZE_BITS))
(defun NUM_ITEMS_PER_PAGE ()
  (ash 1 PAGE_SIZE_BITS))
(defun OFFSET_MASK ()
  (- (NUM_ITEMS_PER_PAGE) 1))
(defun NUM_PAGES ()
  (ash 1 (NUM_PAGES_BITS)))
(defun PAGE_INDEX_MASK ()
  (- (NUM_PAGES) 1))


;; //#define rmCompManagerGetPage(a) ( ((a) >> PAGE_SIZE_BITS) & PAGE_INDEX_MASK )
(defun rmCompManagerGetPage (a)
  (logand (PAGE_INDEX_MASK) (ash a (- PAGE_SIZE_BITS))))

;; //#define rmCompManagerGetOffset(a) ( ((a) & OFFSET_MASK) )
(defun rmCompManagerGetOffset (a)
  (logand a (OFFSET_MASK)))

;; //#define rmCompManagerSetPage(a, b) ( (a)  | (((b) & PAGE_INDEX_MASK) << PAGE_SIZE_BITS) )
(defun rmCompManagerSetPage (a b)
  (logior a 
	  (ash (logand b (PAGE_INDEX_MASK)) 
	       PAGE_SIZE_BITS)))

;; //#define rmCompManagerSetOffset(a, b) ( ((a) & (OFFSET_MASK)) | ( (b) & OFFSET_MASK) )
(defun rmCompManagerSetOffset (a b)
  (logior (logand a (OFFSET_MASK))
	  (logand b (OFFSET_MASK))))

(defconstant  RM_CACHEKEY_UNINITIALIZED_VALUE 2147483649)


(defcenum RMenum
	(:RM_WHACKED -1)
	(:RM_FALSE 0)
	(:RM_TRUE 1)
	(:RM_CHILL 2)
	(:RM_MUTEX_UNLOCK 3)
	(:RM_MUTEX_LOCK 4)
	(:RM_MUTEX_BUSY 5)
	(:RM_NATIVE_OPENGL #x010)
	(:RM_MESA_OPENGL #x011)
	(:RM_HARDWARE #x020)
	(:RM_SOFTWARE #x021)
	(:RM_OR #x030)
	(:RM_AND #x031)
	(:RM_SET #x032)
	(:RM_PS_PORTRAIT #x0100)
	(:RM_PS_LANDSCAPE #x0101)
	(:RM_PS_REGULAR #x0102)
	(:RM_PS_EPS #x0103)
	(:RM_PS_VECTOR #x0106)
	(:RM_PS_RASTER #x0107)
	(:RM_PS_SORT_FAST #x0110)
	(:RM_PS_SORT_BSP #x0111)
	(:RM_PS_SORT_FULL #x0111)
	(:RM_PS_SORT_HYBRID_SCREEN_BSP #x0112)
	(:RM_PS_SORT_HYBRID_DEPTH_BSP #x0113)
	(:RM_SCENE_CAMERA3D #x0121)
	(:RM_SCENE_VIEWPORT #x0122)
	(:RM_SCENE_BACKGROUND_COLOR #x0123)
	(:RM_SCENE_CAMERA2D #x0124)
	(:RM_SCENE_BACKGROUND_IMAGE_TILE #x0125)
	(:RM_SCENE_CLIP_PLANE0 #x0126)
	(:RM_SCENE_CLIP_PLANE1 #x0127)
	(:RM_SCENE_CLIP_PLANE2 #x0128)
	(:RM_SCENE_CLIP_PLANE3 #x0129)
	(:RM_SCENE_CLIP_PLANE4 #x012A)
	(:RM_SCENE_CLIP_PLANE5 #x012B)
	(:RM_SCENE_INV_PROJECTION #x012C)
	(:RM_SCENE_TEXTURE2D #x012D)
	(:RM_SCENE_TEXTURE3D #x012E)
	(:RM_SCENE_TEXTURE3D_UPDATE #x012F)
	(:RM_LINES #x0140)
	(:RM_LINE_STRIP #x0141)
	(:RM_TRIANGLES #x0142)
	(:RM_TRIANGLE_STRIP #x0143)
	(:RM_TRIANGLE_FAN #x0144)
	(:RM_QUADMESH #x0145)
	(:RM_POINTS #x0146)
	(:RM_POLYS #x0147)
	(:RM_QUAD_STRIP #x0148)
	(:RM_SPHERES #x0150)
	(:RM_BOX3D #x0151)
	(:RM_BOX3D_WIRE #x0152)
	(:RM_CONES #x0153)
	(:RM_CYLINDERS #x0154)
	(:RM_OCTMESH #x0158)
	(:RM_TEXT #x0160)
	(:RM_INDEXED_TEXT #x0161)
	(:RM_QUADS #x0162)
	(:RM_MARKERS2D #x0163)
	(:RM_CIRCLE2D #x0164)
	(:RM_BOX2D #x0165)
	(:RM_ELLIPSE2D #x0166)
	(:RM_SPRITE #x0167)
	(:RM_BITMAP #x0168)
	(:RM_INDEXED_BITMAP #x0169)
	(:RM_INDEXED_TFAN #x0170)
	(:RM_INDEXED_QUADS #x0171)
	(:RM_INDEXED_TRIANGLES #x0172)
	(:RM_INDEXED_TRIANGLE_STRIP #x0173)
	(:RM_INDEXED_QUAD_STRIP #x0174)
	(:RM_APP_DISPLAYLIST #x0175)
	(:RM_USERDEFINED_PRIM #x0180)
	(:RM_TEXTURE_WRAP_CLAMP #x0210)
	(:RM_TEXTURE_WRAP_REPEAT #x0211)
	(:RM_TEXTURE_FILTER_NEAREST #x0212)
	(:RM_TEXTURE_FILTER_LINEAR #x0213)
	(:RM_TEXTURE_FILTER_MIPMAP_NEAREST #x0214)
	(:RM_TEXTURE_FILTER_MIPMAP_LINEAR #x0215)
	(:RM_SHADER_SMOOTH #x0220)
	(:RM_SHADER_FLAT #x0221)
	(:RM_SHADER_NOLIGHT #x0222)
	(:RM_FRONT #x0230)
	(:RM_BACK #x0231)
	(:RM_FRONT_AND_BACK #x0232)
	(:RM_POINT #x0233)
	(:RM_LINE #x0234)
	(:RM_FILL #x0235)
	(:RM_CULL_NONE #x0240)
	(:RM_CULL_FRONT #x0241)
	(:RM_CULL_BACK #x0242)
	(:RM_CULL_FRONT_AND_BACK #x0243)
	(:RM_CCW #x0250)
	(:RM_CW #x0251)
	(:RM_TRANSFORM_GEOMETRY #x0260)
	(:RM_TRANSFORM_TEXTURE #x0261)
	(:RM_TRANSFORM_IGNORE #x0262)
	(:RM_ALL_CHANNELS #x0270)
	(:RM_LEFT_CHANNEL #x0271)
	(:RM_RIGHT_CHANNEL #x0272)
	(:RM_MONO_CHANNEL #x0273)
	(:RM_REDBLUE_STEREO_CHANNEL #x0274)
	(:RM_BLUERED_STEREO_CHANNEL #x0275)
	(:RM_MBUF_STEREO_CHANNEL #x0276)
	(:RM_OFFSCREEN_MONO_CHANNEL #x0277)
	(:RM_OFFSCREEN_REDBLUE_STEREO_CHANNEL #x0278)
	(:RM_OFFSCREEN_BLUERED_STEREO_CHANNEL #x0279)
	(:RM_PROJECTION_ORTHOGRAPHIC #x0301)
	(:RM_PROJECTION_PERSPECTIVE #x0302)
	(:RM_LIGHT_POINT #x0310)
	(:RM_LIGHT_DIRECTIONAL #x0311)
	(:RM_LIGHT_SPOT #x0312)
	(:RM_LIGHT0 #x0320)
	(:RM_LIGHT1 #x0321)
	(:RM_LIGHT2 #x0322)
	(:RM_LIGHT3 #x0323)
	(:RM_LIGHT4 #x0324)
	(:RM_LIGHT5 #x0325)
	(:RM_LIGHT6 #x0326)
	(:RM_LIGHT7 #x0327)
	(:RM_IMAGE_ALPHA #x0401)
	(:RM_IMAGE_LUMINANCE #x0404)
	(:RM_IMAGE_LUMINANCE_ALPHA #x0405)
	(:RM_IMAGE_RGB #x0406)
	(:RM_IMAGE_RGBA #x0407)
	(:RM_IMAGE_DEPTH #x0408)
	(:RM_UNSIGNED_BYTE #x0409)
	(:RM_FLOAT #x040A)
	(:RM_SHORT #x040B)
	(:RM_UNSIGNED_SHORT #x040C)
	(:RM_IMAGE_MIRROR_WIDTH #x0410)
	(:RM_IMAGE_MIRROR_HEIGHT #x0411)
	(:RM_IMAGE_MIRROR_DEPTH #x0412)
	(:RM_COPY_DATA #x0420)
	(:RM_DONT_COPY_DATA #x0421)
	(:RM_COPY_UNDEFINED #x0422)
	(:RM_LINES_SOLID #x0501)
	(:RM_LINES_DASHED #x0502)
	(:RM_LINES_DOTTED #x0503)
	(:RM_LINES_DOT_DASH #x0504)
	(:RM_LINES_DASH_DASH_DOT #x0505)
	(:RM_LINEWIDTH_NARROW #x0510)
	(:RM_LINEWIDTH_MEDIUM #x0511)
	(:RM_LINEWIDTH_HEAVY #x0512)
	(:RM_LINEWIDTH_1 #x0513)
	(:RM_LINEWIDTH_2 #x0514)
	(:RM_LINEWIDTH_3 #x0515)
	(:RM_LINEWIDTH_4 #x0516)
	(:RM_LINEWIDTH_5 #x0517)
	(:RM_LINEWIDTH_6 #x0518)
	(:RM_LINEWIDTH_7 #x0519)
	(:RM_LINEWIDTH_8 #x051A)
	(:RM_LEFT #x0520)
	(:RM_CENTER #x0521)
	(:RM_RIGHT #x0522)
	(:RM_TOP #x0523)
	(:RM_BOTTOM #x0524)
	(:RM_PRINT_TERSE #x0520)
	(:RM_PRINT_VERBOSE #x0521)
	(:RM_RENDERPASS_OPAQUE #x0600)
	(:RM_RENDERPASS_TRANSPARENT #x0601)
	(:RM_RENDERPASS_3D #x0602)
	(:RM_RENDERPASS_2D #x0603)
	(:RM_RENDERPASS_ALL #x0604)
	(:RM_NOTIFY_SILENCE #x0610)
	(:RM_NOTIFY_FULL #x0611)
	(:RM_VIEW #x0620)
	(:RM_RENDER #x0621)
	(:RM_PIPE_SERIAL #x0630)
	(:RM_PIPE_MULTISTAGE #x0631)
	(:RM_PIPE_MULTISTAGE_PARALLEL #x0632)
	(:RM_PIPE_MULTISTAGE_VIEW_PARALLEL #x0633)
	(:RM_PIPE_SERIAL_NOBLOCK #x0634)
	(:RM_PIPE_MULTISTAGE_NOBLOCK #x0635)
	(:RM_PIPE_MULTISTAGE_PARALLEL_NOBLOCK #x0636)
	(:RM_PIPE_GLX #x0650)
	(:RM_PIPE_WGL #x0651)
	(:RM_PIPE_CR #x0652)
	(:RM_PIPE_NOPLATFORM #x0653)
	(:RM_DEFAULT_NODE_PICK_TRAVERSAL_MASK #x0700)
	(:RM_DEFAULT_NODE_TRAVERSAL_MASK #x0701))


(defcstruct RMmatrix
	(m :float :count 16))

;; See "rmtypes.h" below.
(defcstruct _object_info
	(name :char :count 64)
	(posted :int)
	(rpass_vdims :int)
	(rpass_opacity :int)
	(channel :int)
	(pickEnable :int))

;; See "rmtypes.h" below.
(defcstruct RMpipe
	(offscreen :int)
	(processingMode :int)
	(channel_format :int)
	(xwindow_width :int)
	(xwindow_height :int)
	(xflags :int)
	(opaque3DEnable :int)
	(transparent3DEnable :int)
	(opaque2DEnable :int)
	(initMatrixStack :int)
	(frameNumber :int)
	(displayListEnableBool :int)
	(contextCache :pointer)
	(displayLists :pointer)
	(mtControl :pointer)
	(hdc :pointer)
	(hwnd :pointer)
	(hRC :pointer)
	(myRank :int)
	(globalNPE :int)
	(targetPlatform :int)
	(channel_render_func :pointer)
	(postRenderBarrierFunc :pointer)
	(postrenderfunc :pointer)
	(postrender_depthbufferfunc :pointer)
	(swapBuffersFunc :pointer)
	(shutdownFunc :pointer)
	(postFBClearBarrierFunc :pointer)
	(createContextFunc :pointer)
	(targetFrameRate :int)
	(timePerFrame :pointer)
	(timePerFrameMS :double)
	(lastTimeStart :pointer)
	(lastRenderTime :pointer)
	(timeSyncFunc :pointer)
	(caps :pointer)
	(fbClearNode :pointer)
	(localMaskStack :int :count 65)
	(localMaskStackTop :int))

;; See "rmpublic.h" below.
(defun rmColor3DNew (a)
	(rmVertex3DNew a))

;; See "rmpublic.h" below.
(defun rmColor3DDelete (a)
	(rmVertex3DDelete a))

;; See "rmaux.h" below.
(defconstant RMAUX_DEFAULT_SPIN_THRESHOLD 3.0)

%}

%module openrm
%{
#include "rmdefs.h"
#include "rmthread.h"
//#include "rmcmpmgr.h" // Skip this one.
#include "rmtypes.h"
//#include "rmx.h"
#include "rmvmap.h"
#include "rmw.h"
// #include "rmcr.h" // Chromium support. Not used.
#include "rmpipe.h"
#include "rmpublic.h"
#include "rmdeflts.h"
#include "rmps.h"
#include "rmaux.h"
#include "rm.h"
#include "rmppm.h"
#include "rmjpeg.h"
%}

// "rmdefs.h"
// TODO: Fix %ignores
%ignore RM_MAXFLOAT; // Not implemented
%ignore RM_MINFLOAT; // Not implemented
%ignore RM_MIN;      // Implemented in Lisp above 
%ignore RM_MAX;      // Implemented in Lisp above
%ignore RM_DEGREES_TO_RADIANS;  // Implemented in Lisp above
%ignore RM_RADIANS_TO_DEGREES;  // Implemented in Lisp above
%ignore RMenum;
typedef int RMenum;
%include "rmdefs.h"


// "rmthread.h"
#define RM_WIN
#define RM_EXPORT_API
typedef void *HANDLE;
//typedef HANDLE        RMmutex;
//typedef unsigned long RMthread;
%include "rmthread.h"

// "rmtypes.h"
// TODO:
//   Check the stuct RMpipeOGLCapabilities. Function pointers do not seem to be defined correctly.
//   Need to modify for Linux. Check RM_X.
#define GLAPIENTRY
#define MAX_MASK_STACK_DEPTH 64
#define MAX_FRAME_SAMPLES 6
%ignore RM_CACHEKEY_UNINITIALIZED_VALUE; // Value defined above in Lisp
%ignore _object_info;  // Defined in Lisp as this contains a staticly defined array of chars.
%ignore RMpipe; // Defined in Lisp as this contains a staticly defined array of ints.
%ignore RMmatrix;
%ignore rmAppDisplayList;
%ignore rmAppDisplayList;
%ignore rmBitmap;
%ignore rmBox2d;
%ignore rmBox3d;
%ignore rmBox3dWire;
%ignore rmCircle2d;
%ignore rmCones;
%ignore rmCylinders;
%ignore rmEllipse2d;
%ignore rmIndexedBitmap;
%ignore rmIndexedQuads;
%ignore rmIndexedQuadStrip;
%ignore rmIndexedText;
%ignore rmIndexedTriangleFan;
%ignore rmIndexedTriangles;
%ignore rmIndexedTriangleStrip;
%ignore rmLineStrip;
%ignore rmLinesDisjoint;
%ignore rmMarkers2D;
%ignore rmOctmesh;
%ignore rmPoints;
%ignore rmPolys;
%ignore rmQuadmesh;
%ignore rmQuads;
%ignore rmQuadStrip;
%ignore rmSpheres;
%ignore rmSprite;
%ignore rmText;
%ignore rmTrianglesDisjoint;
%ignore rmTriangleFan;
%ignore rmTrianglesConnected;


typedef unsigned int GLenum;
typedef unsigned char GLboolean;
typedef unsigned int GLbitfield;
typedef signed char GLbyte;
typedef short GLshort;
typedef int GLint;
typedef int GLsizei;
typedef unsigned char GLubyte;
typedef unsigned short GLushort;
typedef unsigned int GLuint;
typedef float GLfloat;
typedef float GLclampf;
typedef double GLdouble;
typedef double GLclampd;
typedef void GLvoid;

struct HINSTANCE__ { int unused; }; typedef struct HINSTANCE__ *HINSTANCE;
struct HWND__ { int unused; }; 
typedef struct HWND__ *HWND;
struct HGLRC__ { int unused; }; 
typedef struct HGLRC__ *HGLRC;
struct HDC__ { int unused; }; 
typedef struct HDC__ *HDC;
%include "rmtypes.h"

// "rmx.h"
// TODO:
//   Not supported in this version.
// %include "rmx.h" // X. 

// "rmvmap.h"
// VERIFY:
%include "rmvmap.h"

// "rmw.h"
// VERIFY:
%ignore rmwText;
%ignore rmwSetupOpenGL;
%include "rmw.h" // Win32

// %include "rmcr.h" // Chromium support. Not used.

// "rmpipe.h"
// VERIFY:
%include "rmpipe.h"

// "rmpublic.h"
// TODO:
//	VCOPY
//	V3COPY
//	V2COPY
//	V4COPY
//	rmInternalMarker2DGetNpts
//	rmInternalMarker2DSetNpts
//	rmInternalMarker2DGetVerts
//	rmInternalMarker2DSetBFlag
//	rmInternalMarker2DGetBFlag
%ignore rmColor3DNew; // Defined in Lisp above.
%ignore rmColor3DDelete; // Defined in Lisp above.
%ignore rmSwapBuffers;
%include "rmpublic.h"

// "rmdeflts.h"
%ignore RM_PS_PORTRAIT;
%ignore RM_SHADER_SMOOTH;
#define RM_PS_PORTRAIT		0x0100;     
#define RM_SHADER_SMOOTH  	0x0220;
%include "rmdeflts.h"

// "rmps.h"
// VERIFY:
%include "rmps.h"

// Remove the following from rmaux.h
//#ifdef RM_WIN
//RM_EXPORT_API  LONG WINAPI rmauxWndProc (HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
//#endif
%ignore RMAUX_DEFAULT_SPIN_THRESHOLD;
%include "rmaux.h"

// "rm.h"
// VERIFY:
%include "rm.h"

%include "rmppm.h"
%include "rmjpeg.h"