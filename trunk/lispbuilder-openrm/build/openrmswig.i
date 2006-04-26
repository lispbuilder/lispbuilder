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
%}

// "rmdefs.h"
// TODO: Fix %ignores
%ignore RM_MAXFLOAT; // Not implemented
%ignore RM_MINFLOAT; // Not implemented
%ignore RM_MIN;      // Implemented in Lisp above 
%ignore RM_MAX;      // Implemented in Lisp above
%ignore RM_DEGREES_TO_RADIANS;  // Implemented in Lisp above
%ignore RM_RADIANS_TO_DEGREES;  // Implemented in Lisp above
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
