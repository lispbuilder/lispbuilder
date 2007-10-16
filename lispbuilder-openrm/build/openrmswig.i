%insert("lisphead") 
%{
;;;; OpenRM CFFI lisp wrapper
;;;; Part of the CL-Gardeners project
;;;; http://wiki.alu.org/Application_Builder
;;;; (C)2006 Luke J Crook
;;;; See COPYING for license
;;;; 
;;;; This .i file has been tested with SDL version 1.2.11

(in-package :lispbuilder-openrm-cffi)

;;; This is to handle a C macro where 1 is shifted left n times
(defun 1<<(x) (ash 1 x))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'openrm-lispify)
(defun openrm-lispify (name flag &optional (package *package*))
  (labels ((find-sub (src lst)
	     (when (>= (length lst)
		       (length src))
	       (if (and (equal src (subseq lst 0 (length src)))
			(not (equal (nth (length src)
					 lst) #\_)))
		   t
		   nil)))
	   (replace-sub (new old lis)
	     (append new (nthcdr (length old) lis)))
	   (next-char (char)
	     (if char
		 (cond
		   ((upper-case-p char)
		    'upper)
		   ((lower-case-p char)
		    'lower)
		   (t nil))
		 nil))
	      (helper (lst last prev-last rest &aux (c (car lst)))
	        (declare (ignore prev-last))
		(cond
		  ((null lst)
		   rest)
		  ((upper-case-p c)
		   (let ((new '(#\R #\M #\_)) (old '(#\R #\M)))
		     (when (and (not (find-sub '(#\R #\M #\I #\_) lst))
				(find-sub old lst))
		       (setf lst (replace-sub new old lst)
			     c (first new))))		   
		   (helper (cdr lst) 'upper last
			   (cond
			     ((or (equal last 'lower)
;; 				  (equal last 'digit)
				  )
			      (list* c #\- rest))
			     ((and (equal last 'upper)
				   (equal (next-char (cadr lst)) 'lower))
			      (list* c #\- rest))
			     (t (cons c rest)))))
		  ((lower-case-p c)
		   (helper (cdr lst) 'lower last (cons (char-upcase c) rest)))
		  ((digit-char-p c)
		   (helper (cdr lst) 'digit last
			   (case last
			     ((upper lower) (list* c #\- rest))
			     (t (cons c rest)))))
		  ((char-equal c #\_)
		   (helper (cdr lst) '_ last (cons #\- rest)))
		  (t
		   (error "Invalid character: ~A" c)))))
    (let ((fix (case flag
		    ((constant variable) "*")
		    (enumvalue "")
		    (t ""))))
      (intern
       (concatenate
	'string
	fix
	(nreverse (helper (concatenate 'list name) nil nil nil))
	fix)
       package))))
))

;;;; Lispifies the following 'C' keywords:
;;;; scancode 		=    SCANCODE
;;;; SDL_ALL_HOTKEYS 	=    *SDL-ALL-HOTKEYS*
;;;; SDLKey 		=    SDL-KEY
;;;; RMenum		=    RM-ENUM
;;;; SDL_GetKeyRepeat	=    SDL-GET-KEY-REPEAT
;;;; SDL_RWFromFP	=    SDL-RW-FROM-FP
;;;; SDL_HasSSE 	=    SDL-HAS-SSE
;;;; SDL_HasSSE2 	=    SDL-HAS-SSE-2
;;;; RMcolor4D		=    RM-COLOR-4D
;;;; SDL_Has3DNow 	=    SDL-HAS-3D-NOW
;;;; SDL_WriteBE32	=    SDL-WRITE-BE-32
;;;; SDLK_SLASH		=    :KEY-SLASH
;;;; SDLK_F1		=    :KEY-F-1
;;;; KMOD_LSHIFT	=    :KEY-MOD-LSHIFT


;;;; FLOAT-POINTER is used by the CFFI translation functions
;;;; see the typemap definition below.
(defctype float-pointer :pointer)

;; (defcstruct RMmatrix
;; 	(m :float :count 16))


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

%}

%module openrm

%feature("intern_function","openrm-lispify");

%typemap(cin) float* "float-pointer";

// "rmdefs.h"
// TODO: Fix %ignores
%ignore RM_MIN;      // Use MIN
%ignore RM_MAX;      // Use MAX
%ignore RM_DEGREES_TO_RADIANS;  // Implemented in Lisp above
%ignore RM_RADIANS_TO_DEGREES;  // Implemented in Lisp above
%include "rmdefs.h"


// "rmthread.h"
#define RM_WIN
#define RM_EXPORT_API
typedef void *HANDLE;
//typedef HANDLE        RMmutex;
//typedef unsigned long RMthread;
%include "rmthread.h"

//%include "rmcmpmgr.h" // Skip this one. Will include if ever required.

// "rmtypes.h"
// TODO:
//   Check the stuct RMpipeOGLCapabilities. Function pointers do not seem to be defined correctly.
//   Need to modify for Linux. Check RM_X.
#define GLAPIENTRY
#define MAX_MASK_STACK_DEPTH 64 // Defined inline in RMpipe. Not cool.
#define MAX_FRAME_SAMPLES 6  	// Defined inline in RMpipe. Not cool.

//%typemap(cin) char name[OBJECT_NAME_LENGTH] "(name :char :count 64)";
%ignore _object_info;  	  	// Defined above as this contains a staticly defined array of chars.
%ignore RMpipe; 		// Defined above as this contains a staticly defined array of ints.

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

struct HINSTANCE__ { int unused; }; 
typedef struct HINSTANCE__ *HINSTANCE;
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
%ignore rmColor3DNew;			// Defined in Lisp above.
%ignore rmColor3DDelete; 		// Defined in Lisp above.
%include "rmpublic.h"

// "rmdeflts.h"
%ignore RM_PS_PORTRAIT;			// This is an enum, not a #define. SWIG can't assign an enum to a define.
%ignore RM_SHADER_SMOOTH;		// This is an enum, not a #define. SWIG can't assign an enum to a define.
#define RM_PS_PORTRAIT		0x0100;     
#define RM_SHADER_SMOOTH  	0x0220;
%include "rmdeflts.h"

%include "rmps.h"

#define WINAPI
%include "rmaux.h"

%include "rm.h"
%include "rmppm.h"
%include "rmjpeg.h"



%insert("lisphead") %{
;;;; "rmdefs.h"

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

;; See "rmpublic.h" below.
(defun rm-Color-3D-New (a)
	(rm-Vertex-3D-New a))

;; See "rmpublic.h" below.
(defun rm-Color-3D-Delete (a)
	(rm-Vertex-3D-Delete a))

%}