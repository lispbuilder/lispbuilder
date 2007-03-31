
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




;;;SWIG wrapper code starts here

(cl:defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(cl:progn ,@(cl:loop for value in enums
                        for index = 0 then (cl:1+ index)
                        when (cl:listp value) do (cl:setf index (cl:second value)
                                                          value (cl:first value))
                        collect `(cl:defconstant ,value ,index))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'swig-lispify)
    (cl:defun swig-lispify (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 ((lower digit) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit 
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c)))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t ""))))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list name) cl:nil cl:nil))
            fix)
           package))))))

;;;SWIG wrapper code ends here


(cl:defconstant #.(openrm-lispify "RM_VERSION_MAJOR" 'constant) 1)

(cl:defconstant #.(openrm-lispify "RM_VERSION_MINOR" 'constant) 5)

(cl:defconstant #.(openrm-lispify "RM_VERSION_REV" 'constant) 2)

(cffi:defcenum #.(openrm-lispify "RMenum" 'enumname)
	(#.(openrm-lispify "RM_WHACKED" 'enumvalue :keyword) -1)
	(#.(openrm-lispify "RM_FALSE" 'enumvalue :keyword) 0)
	(#.(openrm-lispify "RM_TRUE" 'enumvalue :keyword) 1)
	(#.(openrm-lispify "RM_CHILL" 'enumvalue :keyword) 1)
	(#.(openrm-lispify "RM_MUTEX_UNLOCK" 'enumvalue :keyword) 0)
	(#.(openrm-lispify "RM_MUTEX_LOCK" 'enumvalue :keyword) 1)
	(#.(openrm-lispify "RM_MUTEX_BUSY" 'enumvalue :keyword) 2)
	(#.(openrm-lispify "RM_NATIVE_OPENGL" 'enumvalue :keyword) #x010)
	(#.(openrm-lispify "RM_MESA_OPENGL" 'enumvalue :keyword) #x011)
	(#.(openrm-lispify "RM_HARDWARE" 'enumvalue :keyword) #x020)
	(#.(openrm-lispify "RM_SOFTWARE" 'enumvalue :keyword) #x021)
	(#.(openrm-lispify "RM_OR" 'enumvalue :keyword) #x030)
	(#.(openrm-lispify "RM_AND" 'enumvalue :keyword) #x031)
	(#.(openrm-lispify "RM_SET" 'enumvalue :keyword) #x032)
	(#.(openrm-lispify "RM_PS_PORTRAIT" 'enumvalue :keyword) #x0100)
	(#.(openrm-lispify "RM_PS_LANDSCAPE" 'enumvalue :keyword) #x0101)
	(#.(openrm-lispify "RM_PS_REGULAR" 'enumvalue :keyword) #x0102)
	(#.(openrm-lispify "RM_PS_EPS" 'enumvalue :keyword) #x0103)
	(#.(openrm-lispify "RM_PS_VECTOR" 'enumvalue :keyword) #x0106)
	(#.(openrm-lispify "RM_PS_RASTER" 'enumvalue :keyword) #x0107)
	(#.(openrm-lispify "RM_PS_SORT_FAST" 'enumvalue :keyword) #x0110)
	(#.(openrm-lispify "RM_PS_SORT_BSP" 'enumvalue :keyword) #x0111)
	(#.(openrm-lispify "RM_PS_SORT_FULL" 'enumvalue :keyword) #x0111)
	(#.(openrm-lispify "RM_PS_SORT_HYBRID_SCREEN_BSP" 'enumvalue :keyword) #x0112)
	(#.(openrm-lispify "RM_PS_SORT_HYBRID_DEPTH_BSP" 'enumvalue :keyword) #x0113)
	(#.(openrm-lispify "RM_SCENE_CAMERA3D" 'enumvalue :keyword) #x0121)
	(#.(openrm-lispify "RM_SCENE_VIEWPORT" 'enumvalue :keyword) #x0122)
	(#.(openrm-lispify "RM_SCENE_BACKGROUND_COLOR" 'enumvalue :keyword) #x0123)
	(#.(openrm-lispify "RM_SCENE_CAMERA2D" 'enumvalue :keyword) #x0124)
	(#.(openrm-lispify "RM_SCENE_BACKGROUND_IMAGE_TILE" 'enumvalue :keyword) #x0125)
	(#.(openrm-lispify "RM_SCENE_CLIP_PLANE0" 'enumvalue :keyword) #x0126)
	(#.(openrm-lispify "RM_SCENE_CLIP_PLANE1" 'enumvalue :keyword) #x0127)
	(#.(openrm-lispify "RM_SCENE_CLIP_PLANE2" 'enumvalue :keyword) #x0128)
	(#.(openrm-lispify "RM_SCENE_CLIP_PLANE3" 'enumvalue :keyword) #x0129)
	(#.(openrm-lispify "RM_SCENE_CLIP_PLANE4" 'enumvalue :keyword) #x012A)
	(#.(openrm-lispify "RM_SCENE_CLIP_PLANE5" 'enumvalue :keyword) #x012B)
	(#.(openrm-lispify "RM_SCENE_INV_PROJECTION" 'enumvalue :keyword) #x012C)
	(#.(openrm-lispify "RM_SCENE_TEXTURE2D" 'enumvalue :keyword) #x012D)
	(#.(openrm-lispify "RM_SCENE_TEXTURE3D" 'enumvalue :keyword) #x012E)
	(#.(openrm-lispify "RM_SCENE_TEXTURE3D_UPDATE" 'enumvalue :keyword) #x012F)
	(#.(openrm-lispify "RM_LINES" 'enumvalue :keyword) #x0140)
	(#.(openrm-lispify "RM_LINE_STRIP" 'enumvalue :keyword) #x0141)
	(#.(openrm-lispify "RM_TRIANGLES" 'enumvalue :keyword) #x0142)
	(#.(openrm-lispify "RM_TRIANGLE_STRIP" 'enumvalue :keyword) #x0143)
	(#.(openrm-lispify "RM_TRIANGLE_FAN" 'enumvalue :keyword) #x0144)
	(#.(openrm-lispify "RM_QUADMESH" 'enumvalue :keyword) #x0145)
	(#.(openrm-lispify "RM_POINTS" 'enumvalue :keyword) #x0146)
	(#.(openrm-lispify "RM_POLYS" 'enumvalue :keyword) #x0147)
	(#.(openrm-lispify "RM_QUAD_STRIP" 'enumvalue :keyword) #x0148)
	(#.(openrm-lispify "RM_SPHERES" 'enumvalue :keyword) #x0150)
	(#.(openrm-lispify "RM_BOX3D" 'enumvalue :keyword) #x0151)
	(#.(openrm-lispify "RM_BOX3D_WIRE" 'enumvalue :keyword) #x0152)
	(#.(openrm-lispify "RM_CONES" 'enumvalue :keyword) #x0153)
	(#.(openrm-lispify "RM_CYLINDERS" 'enumvalue :keyword) #x0154)
	(#.(openrm-lispify "RM_OCTMESH" 'enumvalue :keyword) #x0158)
	(#.(openrm-lispify "RM_TEXT" 'enumvalue :keyword) #x0160)
	(#.(openrm-lispify "RM_INDEXED_TEXT" 'enumvalue :keyword) #x0161)
	(#.(openrm-lispify "RM_QUADS" 'enumvalue :keyword) #x0162)
	(#.(openrm-lispify "RM_MARKERS2D" 'enumvalue :keyword) #x0163)
	(#.(openrm-lispify "RM_CIRCLE2D" 'enumvalue :keyword) #x0164)
	(#.(openrm-lispify "RM_BOX2D" 'enumvalue :keyword) #x0165)
	(#.(openrm-lispify "RM_ELLIPSE2D" 'enumvalue :keyword) #x0166)
	(#.(openrm-lispify "RM_SPRITE" 'enumvalue :keyword) #x0167)
	(#.(openrm-lispify "RM_BITMAP" 'enumvalue :keyword) #x0168)
	(#.(openrm-lispify "RM_INDEXED_BITMAP" 'enumvalue :keyword) #x0169)
	(#.(openrm-lispify "RM_INDEXED_TFAN" 'enumvalue :keyword) #x0170)
	(#.(openrm-lispify "RM_INDEXED_QUADS" 'enumvalue :keyword) #x0171)
	(#.(openrm-lispify "RM_INDEXED_TRIANGLES" 'enumvalue :keyword) #x0172)
	(#.(openrm-lispify "RM_INDEXED_TRIANGLE_STRIP" 'enumvalue :keyword) #x0173)
	(#.(openrm-lispify "RM_INDEXED_QUAD_STRIP" 'enumvalue :keyword) #x0174)
	(#.(openrm-lispify "RM_APP_DISPLAYLIST" 'enumvalue :keyword) #x0175)
	(#.(openrm-lispify "RM_USERDEFINED_PRIM" 'enumvalue :keyword) #x0180)
	(#.(openrm-lispify "RM_TEXTURE_WRAP_CLAMP" 'enumvalue :keyword) #x0210)
	(#.(openrm-lispify "RM_TEXTURE_WRAP_REPEAT" 'enumvalue :keyword) #x0211)
	(#.(openrm-lispify "RM_TEXTURE_FILTER_NEAREST" 'enumvalue :keyword) #x0212)
	(#.(openrm-lispify "RM_TEXTURE_FILTER_LINEAR" 'enumvalue :keyword) #x0213)
	(#.(openrm-lispify "RM_TEXTURE_FILTER_MIPMAP_NEAREST" 'enumvalue :keyword) #x0214)
	(#.(openrm-lispify "RM_TEXTURE_FILTER_MIPMAP_LINEAR" 'enumvalue :keyword) #x0215)
	(#.(openrm-lispify "RM_SHADER_SMOOTH" 'enumvalue :keyword) #x0220)
	(#.(openrm-lispify "RM_SHADER_FLAT" 'enumvalue :keyword) #x0221)
	(#.(openrm-lispify "RM_SHADER_NOLIGHT" 'enumvalue :keyword) #x0222)
	(#.(openrm-lispify "RM_FRONT" 'enumvalue :keyword) #x0230)
	(#.(openrm-lispify "RM_BACK" 'enumvalue :keyword) #x0231)
	(#.(openrm-lispify "RM_FRONT_AND_BACK" 'enumvalue :keyword) #x0232)
	(#.(openrm-lispify "RM_POINT" 'enumvalue :keyword) #x0233)
	(#.(openrm-lispify "RM_LINE" 'enumvalue :keyword) #x0234)
	(#.(openrm-lispify "RM_FILL" 'enumvalue :keyword) #x0235)
	(#.(openrm-lispify "RM_CULL_NONE" 'enumvalue :keyword) #x0240)
	(#.(openrm-lispify "RM_CULL_FRONT" 'enumvalue :keyword) #x0241)
	(#.(openrm-lispify "RM_CULL_BACK" 'enumvalue :keyword) #x0242)
	(#.(openrm-lispify "RM_CULL_FRONT_AND_BACK" 'enumvalue :keyword) #x0243)
	(#.(openrm-lispify "RM_CCW" 'enumvalue :keyword) #x0250)
	(#.(openrm-lispify "RM_CW" 'enumvalue :keyword) #x0251)
	(#.(openrm-lispify "RM_TRANSFORM_GEOMETRY" 'enumvalue :keyword) #x0260)
	(#.(openrm-lispify "RM_TRANSFORM_TEXTURE" 'enumvalue :keyword) #x0261)
	(#.(openrm-lispify "RM_TRANSFORM_IGNORE" 'enumvalue :keyword) #x0262)
	(#.(openrm-lispify "RM_ALL_CHANNELS" 'enumvalue :keyword) #x0270)
	(#.(openrm-lispify "RM_LEFT_CHANNEL" 'enumvalue :keyword) #x0271)
	(#.(openrm-lispify "RM_RIGHT_CHANNEL" 'enumvalue :keyword) #x0272)
	(#.(openrm-lispify "RM_MONO_CHANNEL" 'enumvalue :keyword) #x0273)
	(#.(openrm-lispify "RM_REDBLUE_STEREO_CHANNEL" 'enumvalue :keyword) #x0274)
	(#.(openrm-lispify "RM_BLUERED_STEREO_CHANNEL" 'enumvalue :keyword) #x0275)
	(#.(openrm-lispify "RM_MBUF_STEREO_CHANNEL" 'enumvalue :keyword) #x0276)
	(#.(openrm-lispify "RM_OFFSCREEN_MONO_CHANNEL" 'enumvalue :keyword) #x0277)
	(#.(openrm-lispify "RM_OFFSCREEN_REDBLUE_STEREO_CHANNEL" 'enumvalue :keyword) #x0278)
	(#.(openrm-lispify "RM_OFFSCREEN_BLUERED_STEREO_CHANNEL" 'enumvalue :keyword) #x0279)
	(#.(openrm-lispify "RM_PROJECTION_ORTHOGRAPHIC" 'enumvalue :keyword) #x0301)
	(#.(openrm-lispify "RM_PROJECTION_PERSPECTIVE" 'enumvalue :keyword) #x0302)
	(#.(openrm-lispify "RM_LIGHT_POINT" 'enumvalue :keyword) #x0310)
	(#.(openrm-lispify "RM_LIGHT_DIRECTIONAL" 'enumvalue :keyword) #x0311)
	(#.(openrm-lispify "RM_LIGHT_SPOT" 'enumvalue :keyword) #x0312)
	(#.(openrm-lispify "RM_LIGHT0" 'enumvalue :keyword) #x0320)
	(#.(openrm-lispify "RM_LIGHT1" 'enumvalue :keyword) #x0321)
	(#.(openrm-lispify "RM_LIGHT2" 'enumvalue :keyword) #x0322)
	(#.(openrm-lispify "RM_LIGHT3" 'enumvalue :keyword) #x0323)
	(#.(openrm-lispify "RM_LIGHT4" 'enumvalue :keyword) #x0324)
	(#.(openrm-lispify "RM_LIGHT5" 'enumvalue :keyword) #x0325)
	(#.(openrm-lispify "RM_LIGHT6" 'enumvalue :keyword) #x0326)
	(#.(openrm-lispify "RM_LIGHT7" 'enumvalue :keyword) #x0327)
	(#.(openrm-lispify "RM_IMAGE_ALPHA" 'enumvalue :keyword) #x0401)
	(#.(openrm-lispify "RM_IMAGE_LUMINANCE" 'enumvalue :keyword) #x0404)
	(#.(openrm-lispify "RM_IMAGE_LUMINANCE_ALPHA" 'enumvalue :keyword) #x0405)
	(#.(openrm-lispify "RM_IMAGE_RGB" 'enumvalue :keyword) #x0406)
	(#.(openrm-lispify "RM_IMAGE_RGBA" 'enumvalue :keyword) #x0407)
	(#.(openrm-lispify "RM_IMAGE_DEPTH" 'enumvalue :keyword) #x0408)
	(#.(openrm-lispify "RM_UNSIGNED_BYTE" 'enumvalue :keyword) #x0409)
	(#.(openrm-lispify "RM_FLOAT" 'enumvalue :keyword) #x040A)
	(#.(openrm-lispify "RM_SHORT" 'enumvalue :keyword) #x040B)
	(#.(openrm-lispify "RM_UNSIGNED_SHORT" 'enumvalue :keyword) #x040C)
	(#.(openrm-lispify "RM_IMAGE_MIRROR_WIDTH" 'enumvalue :keyword) #x0410)
	(#.(openrm-lispify "RM_IMAGE_MIRROR_HEIGHT" 'enumvalue :keyword) #x0411)
	(#.(openrm-lispify "RM_IMAGE_MIRROR_DEPTH" 'enumvalue :keyword) #x0412)
	(#.(openrm-lispify "RM_COPY_DATA" 'enumvalue :keyword) #x0420)
	(#.(openrm-lispify "RM_DONT_COPY_DATA" 'enumvalue :keyword) #x0421)
	(#.(openrm-lispify "RM_COPY_UNDEFINED" 'enumvalue :keyword) #x0422)
	(#.(openrm-lispify "RM_LINES_SOLID" 'enumvalue :keyword) #x0501)
	(#.(openrm-lispify "RM_LINES_DASHED" 'enumvalue :keyword) #x0502)
	(#.(openrm-lispify "RM_LINES_DOTTED" 'enumvalue :keyword) #x0503)
	(#.(openrm-lispify "RM_LINES_DOT_DASH" 'enumvalue :keyword) #x0504)
	(#.(openrm-lispify "RM_LINES_DASH_DASH_DOT" 'enumvalue :keyword) #x0505)
	(#.(openrm-lispify "RM_LINEWIDTH_NARROW" 'enumvalue :keyword) #x0510)
	(#.(openrm-lispify "RM_LINEWIDTH_MEDIUM" 'enumvalue :keyword) #x0511)
	(#.(openrm-lispify "RM_LINEWIDTH_HEAVY" 'enumvalue :keyword) #x0512)
	(#.(openrm-lispify "RM_LINEWIDTH_1" 'enumvalue :keyword) #x0513)
	(#.(openrm-lispify "RM_LINEWIDTH_2" 'enumvalue :keyword) #x0514)
	(#.(openrm-lispify "RM_LINEWIDTH_3" 'enumvalue :keyword) #x0515)
	(#.(openrm-lispify "RM_LINEWIDTH_4" 'enumvalue :keyword) #x0516)
	(#.(openrm-lispify "RM_LINEWIDTH_5" 'enumvalue :keyword) #x0517)
	(#.(openrm-lispify "RM_LINEWIDTH_6" 'enumvalue :keyword) #x0518)
	(#.(openrm-lispify "RM_LINEWIDTH_7" 'enumvalue :keyword) #x0519)
	(#.(openrm-lispify "RM_LINEWIDTH_8" 'enumvalue :keyword) #x051A)
	(#.(openrm-lispify "RM_LEFT" 'enumvalue :keyword) #x0520)
	(#.(openrm-lispify "RM_CENTER" 'enumvalue :keyword) #x0521)
	(#.(openrm-lispify "RM_RIGHT" 'enumvalue :keyword) #x0522)
	(#.(openrm-lispify "RM_TOP" 'enumvalue :keyword) #x0523)
	(#.(openrm-lispify "RM_BOTTOM" 'enumvalue :keyword) #x0524)
	(#.(openrm-lispify "RM_PRINT_TERSE" 'enumvalue :keyword) #x0520)
	(#.(openrm-lispify "RM_PRINT_VERBOSE" 'enumvalue :keyword) #x0521)
	(#.(openrm-lispify "RM_RENDERPASS_OPAQUE" 'enumvalue :keyword) #x0600)
	(#.(openrm-lispify "RM_RENDERPASS_TRANSPARENT" 'enumvalue :keyword) #x0601)
	(#.(openrm-lispify "RM_RENDERPASS_3D" 'enumvalue :keyword) #x0602)
	(#.(openrm-lispify "RM_RENDERPASS_2D" 'enumvalue :keyword) #x0603)
	(#.(openrm-lispify "RM_RENDERPASS_ALL" 'enumvalue :keyword) #x0604)
	(#.(openrm-lispify "RM_NOTIFY_SILENCE" 'enumvalue :keyword) #x0610)
	(#.(openrm-lispify "RM_NOTIFY_FULL" 'enumvalue :keyword) #x0611)
	(#.(openrm-lispify "RM_VIEW" 'enumvalue :keyword) #x0620)
	(#.(openrm-lispify "RM_RENDER" 'enumvalue :keyword) #x0621)
	(#.(openrm-lispify "RM_PIPE_SERIAL" 'enumvalue :keyword) #x0630)
	(#.(openrm-lispify "RM_PIPE_MULTISTAGE" 'enumvalue :keyword) #x0631)
	(#.(openrm-lispify "RM_PIPE_MULTISTAGE_PARALLEL" 'enumvalue :keyword) #x0632)
	(#.(openrm-lispify "RM_PIPE_MULTISTAGE_VIEW_PARALLEL" 'enumvalue :keyword) #x0633)
	(#.(openrm-lispify "RM_PIPE_SERIAL_NOBLOCK" 'enumvalue :keyword) #x0634)
	(#.(openrm-lispify "RM_PIPE_MULTISTAGE_NOBLOCK" 'enumvalue :keyword) #x0635)
	(#.(openrm-lispify "RM_PIPE_MULTISTAGE_PARALLEL_NOBLOCK" 'enumvalue :keyword) #x0636)
	(#.(openrm-lispify "RM_PIPE_GLX" 'enumvalue :keyword) #x0650)
	(#.(openrm-lispify "RM_PIPE_WGL" 'enumvalue :keyword) #x0651)
	(#.(openrm-lispify "RM_PIPE_CR" 'enumvalue :keyword) #x0652)
	(#.(openrm-lispify "RM_PIPE_NOPLATFORM" 'enumvalue :keyword) #x0653)
	(#.(openrm-lispify "RM_DEFAULT_NODE_PICK_TRAVERSAL_MASK" 'enumvalue :keyword) #x0700)
	(#.(openrm-lispify "RM_DEFAULT_NODE_TRAVERSAL_MASK" 'enumvalue :keyword) #x0701))

(cl:defconstant #.(openrm-lispify "RM_PI" 'constant) 3.1415926535897931d0)

(cl:defconstant #.(openrm-lispify "RM_TWO_PI" 'constant) 6.2831853071795862d0)

(cl:defconstant #.(openrm-lispify "RM_SQRT2" 'constant) 1.4142135623730951d0)

(cl:defconstant #.(openrm-lispify "RM_SQRT1_2" 'constant) 0.7071067811865476d0)

(cl:defconstant #.(openrm-lispify "RM_MAXFLOAT" 'constant) 1.0d+20)

(cl:defconstant #.(openrm-lispify "RM_MINFLOAT" 'constant) -1.0d+20)

(cl:defconstant #.(openrm-lispify "RM_MAX_STRING_LENGTH" 'constant) 64)

(cl:defconstant #.(openrm-lispify "RM_SINGLEBUFFERED" 'constant) 0)

(cl:defconstant #.(openrm-lispify "RM_DOUBLEBUFFERED" 'constant) 1)

(cl:defconstant #.(openrm-lispify "RM_MAX_MIPMAPS" 'constant) 16)

(cl:defconstant #.(openrm-lispify "RM_MAX_LIGHTS" 'constant) 8)

(cl:defconstant #.(openrm-lispify "RM_MAX_MULTITEXTURES" 'constant) 8)

(cl:defconstant #.(openrm-lispify "RM_FEEDBACK_MIN_BUFFER_SIZE" 'constant) 65536)

(cl:defconstant #.(openrm-lispify "RM_NUM_FONT_FACES" 'constant) 5)

(cl:defconstant #.(openrm-lispify "RM_FONT_SERIF" 'constant) 0)

(cl:defconstant #.(openrm-lispify "RM_FONT_SANS" 'constant) 1)

(cl:defconstant #.(openrm-lispify "RM_FONT_MONO" 'constant) 2)

(cl:defconstant #.(openrm-lispify "RM_FONT_SYMBOL" 'constant) 3)

(cl:defconstant #.(openrm-lispify "RM_FONT_DINGBATS" 'constant) 4)

(cl:defconstant #.(openrm-lispify "RM_NUM_FONT_SIZES" 'constant) 7)

(cl:defconstant #.(openrm-lispify "RM_FONT_XXS" 'constant) 0)

(cl:defconstant #.(openrm-lispify "RM_FONT_XS" 'constant) 1)

(cl:defconstant #.(openrm-lispify "RM_FONT_S" 'constant) 2)

(cl:defconstant #.(openrm-lispify "RM_FONT_M" 'constant) 3)

(cl:defconstant #.(openrm-lispify "RM_FONT_L" 'constant) 4)

(cl:defconstant #.(openrm-lispify "RM_FONT_XL" 'constant) 5)

(cl:defconstant #.(openrm-lispify "RM_FONT_XXL" 'constant) 6)

(cl:defconstant #.(openrm-lispify "RM_NUM_FONT_STYLES" 'constant) 4)

(cl:defconstant #.(openrm-lispify "RM_MAX_LINESTYLES" 'constant) 5)

(cl:defconstant #.(openrm-lispify "RM_MAX_LINEWEIGHTS" 'constant) 8)

(cl:defconstant #.(openrm-lispify "RM_SPHERES_8" 'constant) 1)

(cl:defconstant #.(openrm-lispify "RM_SPHERES_32" 'constant) 2)

(cl:defconstant #.(openrm-lispify "RM_SPHERES_128" 'constant) 3)

(cl:defconstant #.(openrm-lispify "RM_SPHERES_512" 'constant) 4)

(cl:defconstant #.(openrm-lispify "RM_CONES_4" 'constant) 4)

(cl:defconstant #.(openrm-lispify "RM_CONES_8" 'constant) 8)

(cl:defconstant #.(openrm-lispify "RM_CONES_12" 'constant) 12)

(cl:defconstant #.(openrm-lispify "RM_CONES_16" 'constant) 16)

(cl:defconstant #.(openrm-lispify "RM_CONES_32" 'constant) 32)

(cl:defconstant #.(openrm-lispify "RM_CONES_64" 'constant) 64)

(cl:defconstant #.(openrm-lispify "RM_CONES_128" 'constant) 128)

(cl:defconstant #.(openrm-lispify "RM_CYLINDERS_4" 'constant) 4)

(cl:defconstant #.(openrm-lispify "RM_CYLINDERS_8" 'constant) 8)

(cl:defconstant #.(openrm-lispify "RM_CYLINDERS_12" 'constant) 12)

(cl:defconstant #.(openrm-lispify "RM_CYLINDERS_16" 'constant) 16)

(cl:defconstant #.(openrm-lispify "RM_CYLINDERS_32" 'constant) 32)

(cl:defconstant #.(openrm-lispify "RM_CYLINDERS_64" 'constant) 64)

(cl:defconstant #.(openrm-lispify "RM_CYLINDERS_128" 'constant) 128)

(cl:defconstant #.(openrm-lispify "RM_OCTMESH_1" 'constant) #x0001)

(cl:defconstant #.(openrm-lispify "RM_OCTMESH_2" 'constant) #x0002)

(cl:defconstant #.(openrm-lispify "RM_OCTMESH_4" 'constant) #x0004)

(cl:defconstant #.(openrm-lispify "RM_OCTMESH_8" 'constant) #x0008)

(cl:defconstant #.(openrm-lispify "RM_OCTMESH_16" 'constant) #x0010)

(cl:defconstant #.(openrm-lispify "RM_OCTMESH_DIVISOR_MASK" 'constant) #x001F)

(cl:defconstant #.(openrm-lispify "RM_OCTMESH_2DTEXTURES_MIN_MEMORY" 'constant) #x0040)

(cl:defconstant #.(openrm-lispify "RM_OCTMESH_2DTEXTURES_MAX_PERFORMANCE" 'constant) #x0080)

(cffi:defcfun ("rmMutexNew" #.(openrm-lispify "rmMutexNew" 'function)) :pointer
  (initLockState #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmMutexDelete" #.(openrm-lispify "rmMutexDelete" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (arg0 :pointer))

(cffi:defcfun ("rmMutexLock" #.(openrm-lispify "rmMutexLock" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (arg0 :pointer))

(cffi:defcfun ("rmMutexUnlock" #.(openrm-lispify "rmMutexUnlock" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (arg0 :pointer))

(cffi:defcfun ("rmMutexTryLock" #.(openrm-lispify "rmMutexTryLock" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmThreadCreate" #.(openrm-lispify "rmThreadCreate" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (threadID :pointer)
  (threadFunc :pointer)
  (args :pointer))

(cffi:defcfun ("rmThreadJoin" #.(openrm-lispify "rmThreadJoin" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (threadID :pointer)
  (threadReturn :pointer))

(cl:defconstant #.(openrm-lispify "MAX_MASK_STACK_DEPTH" 'constant) 64)

(cl:defconstant #.(openrm-lispify "MAX_FRAME_SAMPLES" 'constant) 6)

(cffi:defcstruct #.(openrm-lispify "HINSTANCE__" 'classname)
	(#.(openrm-lispify "unused" 'slotname) :int))

(cffi:defcstruct #.(openrm-lispify "HWND__" 'classname)
	(#.(openrm-lispify "unused" 'slotname) :int))

(cffi:defcstruct #.(openrm-lispify "HGLRC__" 'classname)
	(#.(openrm-lispify "unused" 'slotname) :int))

(cffi:defcstruct #.(openrm-lispify "HDC__" 'classname)
	(#.(openrm-lispify "unused" 'slotname) :int))

(cl:defconstant #.(openrm-lispify "MAX_VIS_CMAP_ENTRIES" 'constant) 256)

(cl:defconstant #.(openrm-lispify "RMV_DEFAULT_MAP_SIZE" 'constant) 256)

(cl:defconstant #.(openrm-lispify "OBJECT_NAME_LENGTH" 'constant) 64)

(cffi:defcstruct #.(openrm-lispify "RMcolor1D" 'classname)
	(#.(openrm-lispify "lum" 'slotname) :float))

(cffi:defcstruct #.(openrm-lispify "RMcolor2D" 'classname)
	(#.(openrm-lispify "lum" 'slotname) :float)
	(#.(openrm-lispify "alpha" 'slotname) :float))

(cffi:defcstruct #.(openrm-lispify "RMcolor3D" 'classname)
	(#.(openrm-lispify "r" 'slotname) :float)
	(#.(openrm-lispify "g" 'slotname) :float)
	(#.(openrm-lispify "b" 'slotname) :float))

(cffi:defcstruct #.(openrm-lispify "RMcolor4D" 'classname)
	(#.(openrm-lispify "r" 'slotname) :float)
	(#.(openrm-lispify "g" 'slotname) :float)
	(#.(openrm-lispify "b" 'slotname) :float)
	(#.(openrm-lispify "a" 'slotname) :float))

(cffi:defcstruct #.(openrm-lispify "RMvertex2D" 'classname)
	(#.(openrm-lispify "x" 'slotname) :float)
	(#.(openrm-lispify "y" 'slotname) :float))

(cffi:defcstruct #.(openrm-lispify "RMvertex3D" 'classname)
	(#.(openrm-lispify "x" 'slotname) :float)
	(#.(openrm-lispify "y" 'slotname) :float)
	(#.(openrm-lispify "z" 'slotname) :float))

(cffi:defcstruct #.(openrm-lispify "RMvertex4D" 'classname)
	(#.(openrm-lispify "x" 'slotname) :float)
	(#.(openrm-lispify "y" 'slotname) :float)
	(#.(openrm-lispify "z" 'slotname) :float)
	(#.(openrm-lispify "w" 'slotname) :float))

(cffi:defcstruct #.(openrm-lispify "RMmatrix" 'classname)
	(#.(openrm-lispify "m" 'slotname) :pointer))

(cffi:defcstruct #.(openrm-lispify "RMarray" 'classname)
	(#.(openrm-lispify "nItems" 'slotname) :int)
	(#.(openrm-lispify "currentArraySize" 'slotname) :int)
	(#.(openrm-lispify "elementSize" 'slotname) :int)
	(#.(openrm-lispify "chunkSize" 'slotname) :int)
	(#.(openrm-lispify "data" 'slotname) :pointer))

(cffi:defcstruct #.(openrm-lispify "RMinternalMarker2D" 'classname)
	(#.(openrm-lispify "npts" 'slotname) :int)
	(#.(openrm-lispify "gl_begin_flag" 'slotname) :int)
	(#.(openrm-lispify "vlist" 'slotname) :pointer))

(cffi:defcstruct #.(openrm-lispify "RMvisMap" 'classname)
	(#.(openrm-lispify "nentries" 'slotname) :int)
	(#.(openrm-lispify "r" 'slotname) :pointer)
	(#.(openrm-lispify "g" 'slotname) :pointer)
	(#.(openrm-lispify "b" 'slotname) :pointer)
	(#.(openrm-lispify "a" 'slotname) :pointer)
	(#.(openrm-lispify "transfer_min" 'slotname) :float)
	(#.(openrm-lispify "transfer_max" 'slotname) :float))

(cffi:defcstruct #.(openrm-lispify "RMbitmap" 'classname)
	(#.(openrm-lispify "w" 'slotname) :int)
	(#.(openrm-lispify "h" 'slotname) :int)
	(#.(openrm-lispify "bytes_per_scanline" 'slotname) :int)
	(#.(openrm-lispify "pbsize" 'slotname) :int)
	(#.(openrm-lispify "pixeldata" 'slotname) :pointer))

(cffi:defcstruct #.(openrm-lispify "RMimage" 'classname)
	(#.(openrm-lispify "ndims" 'slotname) :int)
	(#.(openrm-lispify "w" 'slotname) :int)
	(#.(openrm-lispify "h" 'slotname) :int)
	(#.(openrm-lispify "d" 'slotname) :int)
	(#.(openrm-lispify "image_format" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "image_type" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "xzoom" 'slotname) :float)
	(#.(openrm-lispify "yzoom" 'slotname) :float)
	(#.(openrm-lispify "copy_flag" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "pixeldata" 'slotname) :pointer)
	(#.(openrm-lispify "appfreefunc" 'slotname) :pointer)
	(#.(openrm-lispify "vismap" 'slotname) :pointer)
	(#.(openrm-lispify "scale" 'slotname) :float)
	(#.(openrm-lispify "bias" 'slotname) :float)
	(#.(openrm-lispify "bytes_per_component" 'slotname) :int)
	(#.(openrm-lispify "bytes_per_scanline" 'slotname) :int)
	(#.(openrm-lispify "elements" 'slotname) :int)
	(#.(openrm-lispify "pbsize" 'slotname) :unsigned-int)
	(#.(openrm-lispify "compListIndx" 'slotname) :int)
	(#.(openrm-lispify "cacheKey" 'slotname) :unsigned-int))

(cffi:defcstruct #.(openrm-lispify "RMcamera2D" 'classname)
	(#.(openrm-lispify "xmin" 'slotname) :float)
	(#.(openrm-lispify "ymin" 'slotname) :float)
	(#.(openrm-lispify "xmax" 'slotname) :float)
	(#.(openrm-lispify "ymax" 'slotname) :float)
	(#.(openrm-lispify "aspect_ratio" 'slotname) :float))

(cffi:defcstruct #.(openrm-lispify "_RMStereoCameraControls" 'classname)
	(#.(openrm-lispify "preframe_leftchannel_func" 'slotname) :pointer)
	(#.(openrm-lispify "preframe_rightchannel_func" 'slotname) :pointer))

(cffi:defcstruct #.(openrm-lispify "RMcamera3D" 'classname)
	(#.(openrm-lispify "eye" 'slotname) #.(openrm-lispify "RMvertex3D" 'classname))
	(#.(openrm-lispify "at" 'slotname) #.(openrm-lispify "RMvertex3D" 'classname))
	(#.(openrm-lispify "up" 'slotname) #.(openrm-lispify "RMvertex3D" 'classname))
	(#.(openrm-lispify "hither" 'slotname) :float)
	(#.(openrm-lispify "yon" 'slotname) :float)
	(#.(openrm-lispify "fov" 'slotname) :float)
	(#.(openrm-lispify "aspect" 'slotname) :float)
	(#.(openrm-lispify "projection" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "isStereo" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "degrees_eye_separation" 'slotname) :float)
	(#.(openrm-lispify "focalDistance" 'slotname) :float))

(cffi:defcstruct #.(openrm-lispify "RMtextProps" 'classname)
	(#.(openrm-lispify "fontEnum" 'slotname) :int)
	(#.(openrm-lispify "italicEnum" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "boldEnum" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "sizeEnum" 'slotname) :int)
	(#.(openrm-lispify "hJustify" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "vJustify" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "listbase" 'slotname) :int)
	(#.(openrm-lispify "listoffset" 'slotname) :int)
	(#.(openrm-lispify "fontinfo" 'slotname) :pointer)
	(#.(openrm-lispify "compListIndx" 'slotname) :int))

(cffi:defcstruct #.(openrm-lispify "_bounding_box" 'classname)
	(#.(openrm-lispify "min" 'slotname) #.(openrm-lispify "RMvertex3D" 'classname))
	(#.(openrm-lispify "max" 'slotname) #.(openrm-lispify "RMvertex3D" 'classname)))

(cffi:defcstruct #.(openrm-lispify "RMclipPlane" 'classname)
	(#.(openrm-lispify "enabled" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "point" 'slotname) #.(openrm-lispify "RMvertex3D" 'classname))
	(#.(openrm-lispify "normal" 'slotname) #.(openrm-lispify "RMvertex3D" 'classname))
	(#.(openrm-lispify "a" 'slotname) :float)
	(#.(openrm-lispify "b" 'slotname) :float)
	(#.(openrm-lispify "c" 'slotname) :float)
	(#.(openrm-lispify "d" 'slotname) :float))

(cffi:defcstruct #.(openrm-lispify "RMlight" 'classname)
	(#.(openrm-lispify "ltype" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "ambientLightColor" 'slotname) #.(openrm-lispify "RMcolor4D" 'classname))
	(#.(openrm-lispify "diffuseLightColor" 'slotname) #.(openrm-lispify "RMcolor4D" 'classname))
	(#.(openrm-lispify "specularLightColor" 'slotname) #.(openrm-lispify "RMcolor4D" 'classname))
	(#.(openrm-lispify "lightXYZ" 'slotname) #.(openrm-lispify "RMvertex3D" 'classname))
	(#.(openrm-lispify "spotCutoff" 'slotname) :float)
	(#.(openrm-lispify "spotExponent" 'slotname) :float)
	(#.(openrm-lispify "spotDirection" 'slotname) #.(openrm-lispify "RMvertex3D" 'classname))
	(#.(openrm-lispify "constantAttenuation" 'slotname) :float)
	(#.(openrm-lispify "linearAttenuation" 'slotname) :float)
	(#.(openrm-lispify "quadraticAttenuation" 'slotname) :float)
	(#.(openrm-lispify "enabled" 'slotname) #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcstruct #.(openrm-lispify "RMlightModel" 'classname)
	(#.(openrm-lispify "globalAmbient" 'slotname) #.(openrm-lispify "RMcolor4D" 'classname))
	(#.(openrm-lispify "twoSideEnable" 'slotname) :unsigned-int)
	(#.(openrm-lispify "localViewerEnable" 'slotname) :unsigned-int))

(cffi:defcstruct #.(openrm-lispify "RMtexture" 'classname)
	(#.(openrm-lispify "images" 'slotname) :pointer)
	(#.(openrm-lispify "appTextureID" 'slotname) :pointer)
	(#.(openrm-lispify "dims" 'slotname) :int)
	(#.(openrm-lispify "nmipmaps" 'slotname) :int)
	(#.(openrm-lispify "mag_filter_mode" 'slotname) :unsigned-int)
	(#.(openrm-lispify "min_filter_mode" 'slotname) :unsigned-int)
	(#.(openrm-lispify "wrap_mode" 'slotname) :unsigned-int)
	(#.(openrm-lispify "oglTexelFormat" 'slotname) :pointer)
	(#.(openrm-lispify "envMode" 'slotname) :unsigned-int)
	(#.(openrm-lispify "blendColor" 'slotname) :pointer)
	(#.(openrm-lispify "borderWidth" 'slotname) :int)
	(#.(openrm-lispify "residency_status" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "compListIndx" 'slotname) :int)
	(#.(openrm-lispify "cacheKeyID" 'slotname) :unsigned-int)
	(#.(openrm-lispify "cacheKeyData" 'slotname) :unsigned-int)
	(#.(openrm-lispify "refCount" 'slotname) :int))

(cffi:defcstruct #.(openrm-lispify "RMfog" 'classname)
	(#.(openrm-lispify "fogMode" 'slotname) :unsigned-int)
	(#.(openrm-lispify "fogDensity" 'slotname) :float)
	(#.(openrm-lispify "fogStart" 'slotname) :float)
	(#.(openrm-lispify "fogEnd" 'slotname) :float)
	(#.(openrm-lispify "fogColor" 'slotname) #.(openrm-lispify "RMcolor4D" 'classname)))

(cffi:defcstruct #.(openrm-lispify "RMstate" 'classname)
	(#.(openrm-lispify "model" 'slotname) #.(openrm-lispify "RMmatrix" 'classname))
	(#.(openrm-lispify "view" 'slotname) #.(openrm-lispify "RMmatrix" 'classname))
	(#.(openrm-lispify "modelView" 'slotname) #.(openrm-lispify "RMmatrix" 'classname))
	(#.(openrm-lispify "projection" 'slotname) #.(openrm-lispify "RMmatrix" 'classname))
	(#.(openrm-lispify "composite" 'slotname) #.(openrm-lispify "RMmatrix" 'classname))
	(#.(openrm-lispify "pick" 'slotname) #.(openrm-lispify "RMmatrix" 'classname))
	(#.(openrm-lispify "textureMatrix" 'slotname) #.(openrm-lispify "RMmatrix" 'classname))
	(#.(openrm-lispify "projection_inverse" 'slotname) #.(openrm-lispify "RMmatrix" 'classname))
	(#.(openrm-lispify "vpm" 'slotname) #.(openrm-lispify "RMmatrix" 'classname))
	(#.(openrm-lispify "vp" 'slotname) :pointer)
	(#.(openrm-lispify "aspect_ratio" 'slotname) :float)
	(#.(openrm-lispify "focal_length" 'slotname) :float)
	(#.(openrm-lispify "w" 'slotname) :int)
	(#.(openrm-lispify "h" 'slotname) :int)
	(#.(openrm-lispify "rendermode" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "renderpass" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "renderPassDims" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "which_channel" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "texture" 'slotname) :pointer)
	(#.(openrm-lispify "texture_mode" 'slotname) :int)
	(#.(openrm-lispify "attrib_stack_depth" 'slotname) :int)
	(#.(openrm-lispify "ambient" 'slotname) #.(openrm-lispify "RMcolor4D" 'classname))
	(#.(openrm-lispify "diffuse" 'slotname) #.(openrm-lispify "RMcolor4D" 'classname))
	(#.(openrm-lispify "specular" 'slotname) #.(openrm-lispify "RMcolor4D" 'classname))
	(#.(openrm-lispify "emissive" 'slotname) #.(openrm-lispify "RMcolor4D" 'classname))
	(#.(openrm-lispify "unlit_color" 'slotname) #.(openrm-lispify "RMcolor4D" 'classname))
	(#.(openrm-lispify "specular_exponent" 'slotname) :float)
	(#.(openrm-lispify "opacity" 'slotname) :float)
	(#.(openrm-lispify "pointsize" 'slotname) :float)
	(#.(openrm-lispify "linewidth" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "linestyle" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "shademodel" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "poly_mode_face" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "poly_mode_drawstyle" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "cull_mode" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "front_face" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "cp0" 'slotname) :pointer)
	(#.(openrm-lispify "cp1" 'slotname) :pointer)
	(#.(openrm-lispify "cp2" 'slotname) :pointer)
	(#.(openrm-lispify "cp3" 'slotname) :pointer)
	(#.(openrm-lispify "cp4" 'slotname) :pointer)
	(#.(openrm-lispify "cp5" 'slotname) :pointer)
	(#.(openrm-lispify "fogActive" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "fog" 'slotname) #.(openrm-lispify "RMfog" 'classname))
	(#.(openrm-lispify "lmodel" 'slotname) :pointer)
	(#.(openrm-lispify "lightSources" 'slotname) :pointer)
	(#.(openrm-lispify "textProps" 'slotname) :pointer)
	(#.(openrm-lispify "frameNumber" 'slotname) :int)
	(#.(openrm-lispify "colorMaterialActive" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "lightingActive" 'slotname) #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcstruct #.(openrm-lispify "RMstateCache" 'classname)
	(#.(openrm-lispify "colorMaterialActive" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "lightingActive" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "texturingActive" 'slotname) #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcstruct #.(openrm-lispify "_surface_properties" 'classname)
	(#.(openrm-lispify "ambient_color" 'slotname) :pointer)
	(#.(openrm-lispify "diffuse_color" 'slotname) :pointer)
	(#.(openrm-lispify "specular_color" 'slotname) :pointer)
	(#.(openrm-lispify "unlit_color" 'slotname) :pointer)
	(#.(openrm-lispify "specular_exponent" 'slotname) float-pointer)
	(#.(openrm-lispify "opacity" 'slotname) float-pointer))

(cffi:defcstruct #.(openrm-lispify "_rendermode_properties" 'classname)
	(#.(openrm-lispify "shademodel" 'slotname) :pointer)
	(#.(openrm-lispify "poly_mode_face" 'slotname) :pointer)
	(#.(openrm-lispify "poly_mode_drawstyle" 'slotname) :pointer)
	(#.(openrm-lispify "cull_mode" 'slotname) :pointer)
	(#.(openrm-lispify "front_face" 'slotname) :pointer)
	(#.(openrm-lispify "pointsize" 'slotname) float-pointer)
	(#.(openrm-lispify "linewidth" 'slotname) :pointer)
	(#.(openrm-lispify "linestyle" 'slotname) :pointer)
	(#.(openrm-lispify "normalizeNormals" 'slotname) :pointer))

(cffi:defcstruct #.(openrm-lispify "internals_RMsceneParms" 'classname)
	(#.(openrm-lispify "viewport" 'slotname) float-pointer)
	(#.(openrm-lispify "camera3d" 'slotname) :pointer)
	(#.(openrm-lispify "camera2d" 'slotname) :pointer)
	(#.(openrm-lispify "textures" 'slotname) :pointer)
	(#.(openrm-lispify "haveAnyTextures" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "cp0" 'slotname) :pointer)
	(#.(openrm-lispify "cp1" 'slotname) :pointer)
	(#.(openrm-lispify "cp2" 'slotname) :pointer)
	(#.(openrm-lispify "cp3" 'slotname) :pointer)
	(#.(openrm-lispify "cp4" 'slotname) :pointer)
	(#.(openrm-lispify "cp5" 'slotname) :pointer)
	(#.(openrm-lispify "lmodel" 'slotname) :pointer)
	(#.(openrm-lispify "lightSources" 'slotname) :pointer)
	(#.(openrm-lispify "textProps" 'slotname) :pointer)
	(#.(openrm-lispify "fog" 'slotname) :pointer))

(cffi:defcstruct #.(openrm-lispify "internals_RMfbClear" 'classname)
	(#.(openrm-lispify "bgColor" 'slotname) :pointer)
	(#.(openrm-lispify "bgImageTile" 'slotname) :pointer)
	(#.(openrm-lispify "depthValue" 'slotname) float-pointer)
	(#.(openrm-lispify "depthImage" 'slotname) :pointer))

(cffi:defcstruct #.(openrm-lispify "internals_RMtransformationStruct" 'classname)
	(#.(openrm-lispify "pre" 'slotname) #.(openrm-lispify "RMmatrix" 'classname))
	(#.(openrm-lispify "s" 'slotname) #.(openrm-lispify "RMmatrix" 'classname))
	(#.(openrm-lispify "r" 'slotname) #.(openrm-lispify "RMmatrix" 'classname))
	(#.(openrm-lispify "s2" 'slotname) #.(openrm-lispify "RMmatrix" 'classname))
	(#.(openrm-lispify "translate" 'slotname) #.(openrm-lispify "RMvertex3D" 'classname))
	(#.(openrm-lispify "post" 'slotname) #.(openrm-lispify "RMmatrix" 'classname))
	(#.(openrm-lispify "transform_mode" 'slotname) #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcstruct #.(openrm-lispify "RMnode" 'classname)
	(#.(openrm-lispify "parent" 'slotname) :pointer)
	(#.(openrm-lispify "nchildren" 'slotname) :int)
	(#.(openrm-lispify "children" 'slotname) :pointer)
	(#.(openrm-lispify "nprims" 'slotname) :int)
	(#.(openrm-lispify "prims" 'slotname) :pointer)
	(#.(openrm-lispify "sprops" 'slotname) :pointer)
	(#.(openrm-lispify "rprops" 'slotname) :pointer)
	(#.(openrm-lispify "scene_parms" 'slotname) :pointer)
	(#.(openrm-lispify "fbClear" 'slotname) :pointer)
	(#.(openrm-lispify "bbox" 'slotname) #.(openrm-lispify "_bounding_box" 'classname))
	(#.(openrm-lispify "center" 'slotname) #.(openrm-lispify "RMvertex3D" 'classname))
	(#.(openrm-lispify "transforms" 'slotname) :pointer)
	(#.(openrm-lispify "object_info" 'slotname) :pointer)
	(#.(openrm-lispify "viewPretraverseCallback" 'slotname) :pointer)
	(#.(openrm-lispify "viewPosttraverseCallback" 'slotname) :pointer)
	(#.(openrm-lispify "renderPretraverseCallback" 'slotname) :pointer)
	(#.(openrm-lispify "renderPosttraverseCallback" 'slotname) :pointer)
	(#.(openrm-lispify "viewSwitchCallback" 'slotname) :pointer)
	(#.(openrm-lispify "viewRenderOrderCallback" 'slotname) :pointer)
	(#.(openrm-lispify "clientData" 'slotname) :pointer)
	(#.(openrm-lispify "clientDataFreeFunc" 'slotname) :pointer)
	(#.(openrm-lispify "refcount" 'slotname) :int)
	(#.(openrm-lispify "attribMask" 'slotname) :unsigned-int)
	(#.(openrm-lispify "nodeMutex" 'slotname) :pointer)
	(#.(openrm-lispify "compListIndx" 'slotname) :int))

(cffi:defcstruct #.(openrm-lispify "RMfontRegistry" 'classname)
	(#.(openrm-lispify "initialized" 'slotname) :int)
	(#.(openrm-lispify "refcount" 'slotname) :int)
	(#.(openrm-lispify "listbase" 'slotname) :int)
	(#.(openrm-lispify "listoffset" 'slotname) :int)
	(#.(openrm-lispify "listCount" 'slotname) :int)
	(#.(openrm-lispify "fontinfo" 'slotname) :pointer))

(cffi:defcstruct #.(openrm-lispify "RMcontextCache" 'classname)
	(#.(openrm-lispify "primDisplayListIDs" 'slotname) :pointer)
	(#.(openrm-lispify "primCacheKeys" 'slotname) :pointer)
	(#.(openrm-lispify "numPrimDisplayListIDs" 'slotname) :int)
	(#.(openrm-lispify "numPrimCacheKeys" 'slotname) :int)
	(#.(openrm-lispify "imgDisplayListIDs" 'slotname) :pointer)
	(#.(openrm-lispify "imgCacheKeys" 'slotname) :pointer)
	(#.(openrm-lispify "numImgDisplayListIDs" 'slotname) :int)
	(#.(openrm-lispify "numImgCacheKeys" 'slotname) :int)
	(#.(openrm-lispify "textureIDs" 'slotname) :pointer)
	(#.(openrm-lispify "textureIDCacheKeys" 'slotname) :pointer)
	(#.(openrm-lispify "textureDataCacheKeys" 'slotname) :pointer)
	(#.(openrm-lispify "numTextureIDs" 'slotname) :int)
	(#.(openrm-lispify "numTextureIDCacheKeys" 'slotname) :int)
	(#.(openrm-lispify "numTextureDataCacheKeys" 'slotname) :int)
	(#.(openrm-lispify "sphereIDs" 'slotname) :pointer)
	(#.(openrm-lispify "coneIDs" 'slotname) :pointer)
	(#.(openrm-lispify "flipConeIDs" 'slotname) :pointer)
	(#.(openrm-lispify "cylinderIDs" 'slotname) :pointer)
	(#.(openrm-lispify "flipCylinderIDs" 'slotname) :pointer)
	(#.(openrm-lispify "pipeFontRegistry" 'slotname) :pointer))

(cffi:defcstruct #.(openrm-lispify "RMtime" 'classname)
	(#.(openrm-lispify "sec" 'slotname) :long)
	(#.(openrm-lispify "usec" 'slotname) :long))

(cffi:defcstruct #.(openrm-lispify "RMpipeOGLCapabilities" 'classname)
	(#.(openrm-lispify "haveMultiTexturing" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "numTextureUnits" 'slotname) :int)
	(#.(openrm-lispify "activeTextureARB" 'slotname) :pointer)
	(#.(openrm-lispify "multiTexCoord1fvARB" 'slotname) :pointer)
	(#.(openrm-lispify "multiTexCoord2fvARB" 'slotname) :pointer)
	(#.(openrm-lispify "multiTexCoord3fvARB" 'slotname) :pointer)
	(#.(openrm-lispify "have3DTextures" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "rm_glTexImage3D" 'slotname) :pointer)
	(#.(openrm-lispify "rm_glTexSubImage3D" 'slotname) :pointer))

(cffi:defcstruct #.(openrm-lispify "RMprimitiveDataBlob" 'classname)
	(#.(openrm-lispify "ptr" 'slotname) :pointer)
	(#.(openrm-lispify "copyflag" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "nthings" 'slotname) :int)
	(#.(openrm-lispify "blobtype" 'slotname) :int)
	(#.(openrm-lispify "stride" 'slotname) :int)
	(#.(openrm-lispify "veclen" 'slotname) :int)
	(#.(openrm-lispify "appfreefunc" 'slotname) :pointer))

(cffi:defcstruct #.(openrm-lispify "RMprimitive" 'classname)
	(#.(openrm-lispify "type" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "blobs" 'slotname) :pointer)
	(#.(openrm-lispify "multiTextureCoordBlobs" 'slotname) :pointer)
	(#.(openrm-lispify "multiTextureCoordBlobsMask" 'slotname) :int)
	(#.(openrm-lispify "numMultiTextureCoordBlobs" 'slotname) :int)
	(#.(openrm-lispify "renderfunc" 'slotname) :pointer)
	(#.(openrm-lispify "clientData" 'slotname) :pointer)
	(#.(openrm-lispify "clientDataFreeFunc" 'slotname) :pointer)
	(#.(openrm-lispify "p1" 'slotname) :pointer)
	(#.(openrm-lispify "flags1" 'slotname) :unsigned-int)
	(#.(openrm-lispify "model_flag" 'slotname) :unsigned-int)
	(#.(openrm-lispify "display_list_enable" 'slotname) :int)
	(#.(openrm-lispify "utility" 'slotname) :int)
	(#.(openrm-lispify "compListIndx" 'slotname) :int)
	(#.(openrm-lispify "cacheKey" 'slotname) :unsigned-int)
	(#.(openrm-lispify "utilCacheKey" 'slotname) :unsigned-int)
	(#.(openrm-lispify "bmin" 'slotname) :pointer)
	(#.(openrm-lispify "bmax" 'slotname) :pointer)
	(#.(openrm-lispify "primitiveComputeBoundingBoxFunc" 'slotname) :pointer))

(cffi:defcfun ("rmAppDisplayList" #.(openrm-lispify "rmAppDisplayList" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmBitmap" #.(openrm-lispify "rmBitmap" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmBox2d" #.(openrm-lispify "rmBox2d" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmBox3d" #.(openrm-lispify "rmBox3d" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmBox3dWire" #.(openrm-lispify "rmBox3dWire" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmCircle2d" #.(openrm-lispify "rmCircle2d" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmCones" #.(openrm-lispify "rmCones" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmCylinders" #.(openrm-lispify "rmCylinders" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmEllipse2d" #.(openrm-lispify "rmEllipse2d" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmIndexedBitmap" #.(openrm-lispify "rmIndexedBitmap" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmIndexedQuads" #.(openrm-lispify "rmIndexedQuads" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmIndexedQuadStrip" #.(openrm-lispify "rmIndexedQuadStrip" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmIndexedText" #.(openrm-lispify "rmIndexedText" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmIndexedTriangleFan" #.(openrm-lispify "rmIndexedTriangleFan" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmIndexedTriangles" #.(openrm-lispify "rmIndexedTriangles" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmIndexedTriangleStrip" #.(openrm-lispify "rmIndexedTriangleStrip" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmLineStrip" #.(openrm-lispify "rmLineStrip" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmLinesDisjoint" #.(openrm-lispify "rmLinesDisjoint" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmMarkers2D" #.(openrm-lispify "rmMarkers2D" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmOctmesh" #.(openrm-lispify "rmOctmesh" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmPoints" #.(openrm-lispify "rmPoints" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmPolys" #.(openrm-lispify "rmPolys" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmQuadmesh" #.(openrm-lispify "rmQuadmesh" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmQuads" #.(openrm-lispify "rmQuads" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmQuadStrip" #.(openrm-lispify "rmQuadStrip" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmSpheres" #.(openrm-lispify "rmSpheres" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmSprite" #.(openrm-lispify "rmSprite" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmText" #.(openrm-lispify "rmText" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmTrianglesDisjoint" #.(openrm-lispify "rmTrianglesDisjoint" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmTriangleFan" #.(openrm-lispify "rmTriangleFan" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmTrianglesConnected" #.(openrm-lispify "rmTrianglesConnected" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcstruct #.(openrm-lispify "RMpick" 'classname)
	(#.(openrm-lispify "node" 'slotname) :pointer)
	(#.(openrm-lispify "zval" 'slotname) :float)
	(#.(openrm-lispify "index" 'slotname) :unsigned-int)
	(#.(openrm-lispify "prim_index" 'slotname) :unsigned-int))

(cffi:defcfun ("rmVismapNew" #.(openrm-lispify "rmVismapNew" 'function)) :pointer
  (size :int))

(cffi:defcfun ("rmVismapDup" #.(openrm-lispify "rmVismapDup" 'function)) :pointer
  (toDuplicate :pointer))

(cffi:defcfun ("rmVismapDelete" #.(openrm-lispify "rmVismapDelete" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toDelete :pointer))

(cffi:defcfun ("rmVismapSetColor3D" #.(openrm-lispify "rmVismapSetColor3D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (indx :int)
  (newColor :pointer))

(cffi:defcfun ("rmVismapGetColor3D" #.(openrm-lispify "rmVismapGetColor3D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (indx :int)
  (retColor :pointer))

(cffi:defcfun ("rmVismapSetColor4D" #.(openrm-lispify "rmVismapSetColor4D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (indx :int)
  (newColor :pointer))

(cffi:defcfun ("rmVismapGetColor4D" #.(openrm-lispify "rmVismapGetColor4D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (indx :int)
  (retColor :pointer))

(cffi:defcfun ("rmVismapSetTfMin" #.(openrm-lispify "rmVismapSetTfMin" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newTFMin :float))

(cffi:defcfun ("rmVismapGetTfMin" #.(openrm-lispify "rmVismapGetTfMin" 'function)) :float
  (toQuery :pointer))

(cffi:defcfun ("rmVismapSetTfMax" #.(openrm-lispify "rmVismapSetTfMax" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newTFMax :float))

(cffi:defcfun ("rmVismapGetTfMax" #.(openrm-lispify "rmVismapGetTfMax" 'function)) :float
  (toQuery :pointer))

(cffi:defcfun ("rmVismapIndexFromData" #.(openrm-lispify "rmVismapIndexFromData" 'function)) :int
  (map :pointer)
  (val :float))

(cffi:defcfun ("rmVismapSetSize" #.(openrm-lispify "rmVismapSetSize" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newSize :int))

(cffi:defcfun ("rmVismapGetSize" #.(openrm-lispify "rmVismapGetSize" 'function)) :int
  (toQuery :pointer))

(cffi:defcfun ("rmVismapSetAlpha" #.(openrm-lispify "rmVismapSetAlpha" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (indx :int)
  (newAlpha :float))

(cffi:defcfun ("rmVismapGetAlpha" #.(openrm-lispify "rmVismapGetAlpha" 'function)) :float
  (toQuery :pointer)
  (indx :int))

(cffi:defcfun ("rmDefaultVismap" #.(openrm-lispify "rmDefaultVismap" 'function)) :pointer)

(cffi:defcfun ("rmPipeSetWindow" #.(openrm-lispify "rmPipeSetWindow" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer)
  (hWnd :pointer)
  (width :int)
  (height :int))

(cffi:defcfun ("rmPipeGetWindow" #.(openrm-lispify "rmPipeGetWindow" 'function)) :pointer
  (p :pointer))

(cffi:defcfun ("rmwText" #.(openrm-lispify "rmwText" 'function)) :void
  (p :pointer)
  (r :pointer)
  (s :pointer)
  (renderPipe :pointer)
  (rsc :pointer))

(cffi:defcfun ("rmPipeSwapBuffersWin32" #.(openrm-lispify "rmPipeSwapBuffersWin32" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer))

(cffi:defcfun ("rmwPipeCreateContext" #.(openrm-lispify "rmwPipeCreateContext" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer))

(cffi:defcfun ("rmPipeGetContext" #.(openrm-lispify "rmPipeGetContext" 'function)) :pointer
  (p :pointer))

(cffi:defcfun ("rmPipeSetContext" #.(openrm-lispify "rmPipeSetContext" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer)
  (theContext :pointer))

(cffi:defcfun ("rmwSetupOpenGL" #.(openrm-lispify "rmwSetupOpenGL" 'function)) :pointer
  (hWnd :pointer))

(cffi:defcfun ("rmPipeNew" #.(openrm-lispify "rmPipeNew" 'function)) :pointer
  (targetPlatform #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmPipeCreateContext" #.(openrm-lispify "rmPipeCreateContext" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer))

(cffi:defcfun ("rmPipeDelete" #.(openrm-lispify "rmPipeDelete" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toDelete :pointer))

(cffi:defcfun ("rmPipeMakeCurrent" #.(openrm-lispify "rmPipeMakeCurrent" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (pipe :pointer))

(cffi:defcfun ("rmPipeClose" #.(openrm-lispify "rmPipeClose" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toClose :pointer))

(cffi:defcfun ("rmPipeSwapBuffers" #.(openrm-lispify "rmPipeSwapBuffers" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer))

(cffi:defcfun ("rmPipeSetChannelFormat" #.(openrm-lispify "rmPipeSetChannelFormat" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newChannelFormat #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmPipeGetChannelFormat" #.(openrm-lispify "rmPipeGetChannelFormat" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmPipeSetInitMatrixStackMode" #.(openrm-lispify "rmPipeSetInitMatrixStackMode" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newMode #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmPipeGetInitMatrixStackMode" #.(openrm-lispify "rmPipeGetInitMatrixStackMode" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmPipeSetRenderPassEnable" #.(openrm-lispify "rmPipeSetRenderPassEnable" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (t_arg0 :pointer)
  (opaque3DEnable #.(openrm-lispify "RMenum" 'enumname))
  (transparent3DEnable #.(openrm-lispify "RMenum" 'enumname))
  (opaque2DEnable #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmPipeGetRenderPassEnable" #.(openrm-lispify "rmPipeGetRenderPassEnable" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (t_arg0 :pointer)
  (opaque3DEnableReturn :pointer)
  (transparent3DEnableReturn :pointer)
  (opaque2DEnableReturn :pointer))

(cffi:defcfun ("rmPipeSetWindowSize" #.(openrm-lispify "rmPipeSetWindowSize" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newWidth :int)
  (newHeight :int))

(cffi:defcfun ("rmPipeGetWindowSize" #.(openrm-lispify "rmPipeGetWindowSize" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (widthReturn :pointer)
  (heightReturn :pointer))

(cffi:defcfun ("rmPipeSetSwapBuffersFunc" #.(openrm-lispify "rmPipeSetSwapBuffersFunc" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newFunc :pointer))

(cffi:defcfun ("rmPipeSetPostRenderFunc" #.(openrm-lispify "rmPipeSetPostRenderFunc" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (postRenderFunc :pointer))

(cffi:defcfun ("rmPipeSetPostRenderDepthFunc" #.(openrm-lispify "rmPipeSetPostRenderDepthFunc" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (postRenderDepthFunc :pointer))

(cffi:defcfun ("rmPipeSetPostRenderBarrierFunc" #.(openrm-lispify "rmPipeSetPostRenderBarrierFunc" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (barrierFunc :pointer))

(cffi:defcfun ("rmPipeGetFrameNumber" #.(openrm-lispify "rmPipeGetFrameNumber" 'function)) :int
  (toQuery :pointer))

(cffi:defcfun ("rmPipeGetProcessingMode" #.(openrm-lispify "rmPipeGetProcessingMode" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmPipeSetProcessingMode" #.(openrm-lispify "rmPipeSetProcessingMode" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newMode #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmPipeProcessingModeIsMultithreaded" #.(openrm-lispify "rmPipeProcessingModeIsMultithreaded" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmPipeSetCommSize" #.(openrm-lispify "rmPipeSetCommSize" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (globalNPE :int))

(cffi:defcfun ("rmPipeGetCommSize" #.(openrm-lispify "rmPipeGetCommSize" 'function)) :int
  (toQuery :pointer))

(cffi:defcfun ("rmPipeSetRank" #.(openrm-lispify "rmPipeSetRank" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (myRank :int))

(cffi:defcfun ("rmPipeGetRank" #.(openrm-lispify "rmPipeGetRank" 'function)) :int
  (toQuery :pointer))

(cffi:defcfun ("rmPipeGetDisplayListEnable" #.(openrm-lispify "rmPipeGetDisplayListEnable" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmPipeSetDisplayListEnable" #.(openrm-lispify "rmPipeSetDisplayListEnable" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newMode #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmPipeSetFrameRate" #.(openrm-lispify "rmPipeSetFrameRate" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newFramesPerSecond :int))

(cffi:defcfun ("rmPipeGetFrameRate" #.(openrm-lispify "rmPipeGetFrameRate" 'function)) :int
  (toQuery :pointer))

(cffi:defcfun ("rmPipeGetNumMultitextureUnits" #.(openrm-lispify "rmPipeGetNumMultitextureUnits" 'function)) :int
  (toQuery :pointer))

(cffi:defcfun ("rmPipeSetSceneBackgroundColor" #.(openrm-lispify "rmPipeSetSceneBackgroundColor" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newColor :pointer))

(cffi:defcfun ("rmPipeGetSceneBackgroundColor" #.(openrm-lispify "rmPipeGetSceneBackgroundColor" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnColor :pointer))

(cffi:defcfun ("rmPipeSetSceneBackgroundImage" #.(openrm-lispify "rmPipeSetSceneBackgroundImage" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newImageTile :pointer))

(cffi:defcfun ("rmPipeGetSceneBackgroundImage" #.(openrm-lispify "rmPipeGetSceneBackgroundImage" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnImageTile :pointer))

(cffi:defcfun ("rmPipeSetSceneDepthImage" #.(openrm-lispify "rmPipeSetSceneDepthImage" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newDepthImage :pointer))

(cffi:defcfun ("rmPipeGetSceneDepthImage" #.(openrm-lispify "rmPipeGetSceneDepthImage" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnDepthImage :pointer))

(cffi:defcfun ("rmPipeSetSceneDepthValue" #.(openrm-lispify "rmPipeSetSceneDepthValue" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newDepthValue float-pointer))

(cffi:defcfun ("rmPipeGetSceneDepthValue" #.(openrm-lispify "rmPipeGetSceneDepthValue" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnDepthValue float-pointer))

(cffi:defcfun ("rmArrayAdd" #.(openrm-lispify "rmArrayAdd" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newData :pointer))

(cffi:defcfun ("rmArrayDelete" #.(openrm-lispify "rmArrayDelete" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (a :pointer))

(cffi:defcfun ("rmArrayGet" #.(openrm-lispify "rmArrayGet" 'function)) :pointer
  (toQuery :pointer)
  (indx :int))

(cffi:defcfun ("rmArrayNew" #.(openrm-lispify "rmArrayNew" 'function)) :pointer
  (initSize :int)
  (chunkSize :int)
  (elementSize :int))

(cffi:defcfun ("rmArrayNumItems" #.(openrm-lispify "rmArrayNumItems" 'function)) :int
  (toQuery :pointer))

(cffi:defcfun ("rmArraySort" #.(openrm-lispify "rmArraySort" 'function)) :void
  (arg0 :pointer)
  (compareFunc :pointer))

(cffi:defcfun ("rmBitmapNew" #.(openrm-lispify "rmBitmapNew" 'function)) :pointer
  (width :int)
  (height :int))

(cffi:defcfun ("rmBitmapCopy" #.(openrm-lispify "rmBitmapCopy" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (dst :pointer)
  (src :pointer))

(cffi:defcfun ("rmBitmapDup" #.(openrm-lispify "rmBitmapDup" 'function)) :pointer
  (src :pointer))

(cffi:defcfun ("rmBitmapDelete" #.(openrm-lispify "rmBitmapDelete" 'function)) :void
  (toDelete :pointer))

(cffi:defcfun ("rmBitmapSetPixelData" #.(openrm-lispify "rmBitmapSetPixelData" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (pixeldata :pointer))

(cffi:defcfun ("rmBitmapGetPixelData" #.(openrm-lispify "rmBitmapGetPixelData" 'function)) :pointer
  (toQuery :pointer))

(cffi:defcfun ("rmBitmapGetSize" #.(openrm-lispify "rmBitmapGetSize" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (widthReturn :pointer)
  (heightReturn :pointer)
  (bytesWidthReturn :pointer))

(cffi:defcfun ("rmBitmapSetBit" #.(openrm-lispify "rmBitmapSetBit" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (columnIndex :int)
  (rowIndex :int))

(cffi:defcfun ("rmCamera2DNew" #.(openrm-lispify "rmCamera2DNew" 'function)) :pointer)

(cffi:defcfun ("rmCamera2DCopy" #.(openrm-lispify "rmCamera2DCopy" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (dst :pointer)
  (src :pointer))

(cffi:defcfun ("rmCamera2DDelete" #.(openrm-lispify "rmCamera2DDelete" 'function)) :void
  (toDelete :pointer))

(cffi:defcfun ("rmDefaultCamera2D" #.(openrm-lispify "rmDefaultCamera2D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (c :pointer))

(cffi:defcfun ("rmCamera2DSetAspectRatio" #.(openrm-lispify "rmCamera2DSetAspectRatio" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newAspect :float))

(cffi:defcfun ("rmCamera2DResetAspectRatio" #.(openrm-lispify "rmCamera2DResetAspectRatio" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (vp float-pointer)
  (windowWidth :int)
  (windowWeight :int))

(cffi:defcfun ("rmCamera2DGetAspectRatio" #.(openrm-lispify "rmCamera2DGetAspectRatio" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (retValue float-pointer))

(cffi:defcfun ("rmCamera2DSetExtents" #.(openrm-lispify "rmCamera2DSetExtents" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (xmin :float)
  (ymin :float)
  (xmax :float)
  (ymax :float))

(cffi:defcfun ("rmCamera2DGetExtents" #.(openrm-lispify "rmCamera2DGetExtents" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (xmin float-pointer)
  (ymin float-pointer)
  (xmax float-pointer)
  (ymax float-pointer))

(cffi:defcfun ("rmCamera2DComputeViewMatrix" #.(openrm-lispify "rmCamera2DComputeViewMatrix" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (c :pointer)
  (m :pointer))

(cffi:defcfun ("rmCamera2DComputeViewFromGeometry" #.(openrm-lispify "rmCamera2DComputeViewFromGeometry" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (source :pointer))

(cffi:defcfun ("rmCamera3DNew" #.(openrm-lispify "rmCamera3DNew" 'function)) :pointer)

(cffi:defcfun ("rmCamera3DCopy" #.(openrm-lispify "rmCamera3DCopy" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (dst :pointer)
  (src :pointer))

(cffi:defcfun ("rmCamera3DDelete" #.(openrm-lispify "rmCamera3DDelete" 'function)) :void
  (toDelete :pointer))

(cffi:defcfun ("rmDefaultCamera3D" #.(openrm-lispify "rmDefaultCamera3D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (c :pointer))

(cffi:defcfun ("rmCamera3DSetAspectRatio" #.(openrm-lispify "rmCamera3DSetAspectRatio" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newAspect :float))

(cffi:defcfun ("rmCamera3DResetAspectRatio" #.(openrm-lispify "rmCamera3DResetAspectRatio" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (vp float-pointer)
  (windowWidth :int)
  (windowHeight :int))

(cffi:defcfun ("rmCamera3DGetAspectRatio" #.(openrm-lispify "rmCamera3DGetAspectRatio" 'function)) :float
  (toQuery :pointer))

(cffi:defcfun ("rmCamera3DSetProjection" #.(openrm-lispify "rmCamera3DSetProjection" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (newVal #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmCamera3DGetProjection" #.(openrm-lispify "rmCamera3DGetProjection" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmCamera3DSetEye" #.(openrm-lispify "rmCamera3DSetEye" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newEye :pointer))

(cffi:defcfun ("rmCamera3DGetEye" #.(openrm-lispify "rmCamera3DGetEye" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnEye :pointer))

(cffi:defcfun ("rmCamera3DSetAt" #.(openrm-lispify "rmCamera3DSetAt" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newAt :pointer))

(cffi:defcfun ("rmCamera3DGetAt" #.(openrm-lispify "rmCamera3DGetAt" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnAt :pointer))

(cffi:defcfun ("rmCamera3DSetUpVector" #.(openrm-lispify "rmCamera3DSetUpVector" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newUpVector :pointer))

(cffi:defcfun ("rmCamera3DGetUpVector" #.(openrm-lispify "rmCamera3DGetUpVector" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnUpVector :pointer))

(cffi:defcfun ("rmCamera3DSetFOV" #.(openrm-lispify "rmCamera3DSetFOV" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (newFOV :float))

(cffi:defcfun ("rmCamera3DGetFOV" #.(openrm-lispify "rmCamera3DGetFOV" 'function)) :float
  (toQuery :pointer))

(cffi:defcfun ("rmCamera3DSetHither" #.(openrm-lispify "rmCamera3DSetHither" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newHither :float))

(cffi:defcfun ("rmCamera3DGetHither" #.(openrm-lispify "rmCamera3DGetHither" 'function)) :float
  (toQuery :pointer))

(cffi:defcfun ("rmCamera3DSetYon" #.(openrm-lispify "rmCamera3DSetYon" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newYon :float))

(cffi:defcfun ("rmCamera3DGetYon" #.(openrm-lispify "rmCamera3DGetYon" 'function)) :float
  (toQuery :pointer))

(cffi:defcfun ("rmCamera3DSetStereo" #.(openrm-lispify "rmCamera3DSetStereo" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newBoolValue #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmCamera3DGetStereo" #.(openrm-lispify "rmCamera3DGetStereo" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmCamera3DSetEyeSeparation" #.(openrm-lispify "rmCamera3DSetEyeSeparation" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newval :float))

(cffi:defcfun ("rmCamera3DGetEyeSeparation" #.(openrm-lispify "rmCamera3DGetEyeSeparation" 'function)) :float
  (toQuery :pointer))

(cffi:defcfun ("rmCamera3DSetFocalDistance" #.(openrm-lispify "rmCamera3DSetFocalDistance" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newval :float))

(cffi:defcfun ("rmCamera3DGetFocalDistance" #.(openrm-lispify "rmCamera3DGetFocalDistance" 'function)) :float
  (toQuery :pointer))

(cffi:defcfun ("rmCamera3DComputeViewMatrix" #.(openrm-lispify "rmCamera3DComputeViewMatrix" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (source :pointer)
  (viewReturn :pointer)
  (projectionReturn :pointer))

(cffi:defcfun ("rmCamera3DComputeViewFromGeometry" #.(openrm-lispify "rmCamera3DComputeViewFromGeometry" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (source :pointer)
  (windowWidth :int)
  (windowHeight :int))

(cffi:defcfun ("rmClipPlaneNew" #.(openrm-lispify "rmClipPlaneNew" 'function)) :pointer)

(cffi:defcfun ("rmClipPlaneDelete" #.(openrm-lispify "rmClipPlaneDelete" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toDelete :pointer))

(cffi:defcfun ("rmClipPlaneSetPointNormal" #.(openrm-lispify "rmClipPlaneSetPointNormal" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (point :pointer)
  (normal :pointer))

(cffi:defcfun ("rmClipPlaneGetPointNormal" #.(openrm-lispify "rmClipPlaneGetPointNormal" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (point :pointer)
  (normal :pointer))

(cffi:defcfun ("rmClipPlaneEnable" #.(openrm-lispify "rmClipPlaneEnable" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer))

(cffi:defcfun ("rmClipPlaneDisable" #.(openrm-lispify "rmClipPlaneDisable" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer))

(cffi:defcfun ("rmClipPlaneIsEnabled" #.(openrm-lispify "rmClipPlaneIsEnabled" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmComponentManagerPrintStatus" #.(openrm-lispify "rmComponentManagerPrintStatus" 'function)) :void)

(cffi:defcfun ("rmError" #.(openrm-lispify "rmError" 'function)) :void
  (msg :string))

(cffi:defcfun ("rmNotice" #.(openrm-lispify "rmNotice" 'function)) :void
  (msg :string))

(cffi:defcfun ("rmNotifyLevel" #.(openrm-lispify "rmNotifyLevel" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (level #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmSplash" #.(openrm-lispify "rmSplash" 'function)) :void
  (msg :string))

(cffi:defcfun ("rmWarning" #.(openrm-lispify "rmWarning" 'function)) :void
  (msg :string))

(cffi:defcfun ("rmGetEnum" #.(openrm-lispify "rmGetEnum" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (queryTag #.(openrm-lispify "RMenum" 'enumname))
  (returnValue :pointer))

(cffi:defcfun ("rmSetEnum" #.(openrm-lispify "rmSetEnum" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (setTag #.(openrm-lispify "RMenum" 'enumname))
  (newValue #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmFogNew" #.(openrm-lispify "rmFogNew" 'function)) :pointer)

(cffi:defcfun ("rmFogDup" #.(openrm-lispify "rmFogDup" 'function)) :pointer
  (toDuplicate :pointer))

(cffi:defcfun ("rmFogDelete" #.(openrm-lispify "rmFogDelete" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toDelete :pointer))

(cffi:defcfun ("rmFogSetMode" #.(openrm-lispify "rmFogSetMode" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newMode :unsigned-int))

(cffi:defcfun ("rmFogGetMode" #.(openrm-lispify "rmFogGetMode" 'function)) :unsigned-int
  (toQuery :pointer))

(cffi:defcfun ("rmFogSetColor" #.(openrm-lispify "rmFogSetColor" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newColor :pointer))

(cffi:defcfun ("rmFogGetColor" #.(openrm-lispify "rmFogGetColor" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnColor :pointer))

(cffi:defcfun ("rmFogSetDensity" #.(openrm-lispify "rmFogSetDensity" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newDensity :float))

(cffi:defcfun ("rmFogGetDensity" #.(openrm-lispify "rmFogGetDensity" 'function)) :float
  (toQuery :pointer))

(cffi:defcfun ("rmFogSetStartEnd" #.(openrm-lispify "rmFogSetStartEnd" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newStart :float)
  (newEnd :float))

(cffi:defcfun ("rmFogGetStartEnd" #.(openrm-lispify "rmFogGetStartEnd" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (startReturn float-pointer)
  (endReturn float-pointer))

(cffi:defcfun ("rmFrame" #.(openrm-lispify "rmFrame" 'function)) :void
  (drawToPipe :pointer)
  (rootedTree :pointer))

(cffi:defcfun ("rmImageNew" #.(openrm-lispify "rmImageNew" 'function)) :pointer
  (ndims :int)
  (width :int)
  (height :int)
  (depth :int)
  (formatEnum #.(openrm-lispify "RMenum" 'enumname))
  (typeEnum #.(openrm-lispify "RMenum" 'enumname))
  (copyFlag #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmImageDup" #.(openrm-lispify "rmImageDup" 'function)) :pointer
  (toDuplicate :pointer))

(cffi:defcfun ("rmImageDelete" #.(openrm-lispify "rmImageDelete" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toDelete :pointer))

(cffi:defcfun ("rmImageSetPixelData" #.(openrm-lispify "rmImageSetPixelData" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (pixelData :pointer)
  (copyEnum #.(openrm-lispify "RMenum" 'enumname))
  (appFreeFunc :pointer))

(cffi:defcfun ("rmImageGetPixelData" #.(openrm-lispify "rmImageGetPixelData" 'function)) :pointer
  (toQuery :pointer))

(cffi:defcfun ("rmImageGetBytesPerScanline" #.(openrm-lispify "rmImageGetBytesPerScanline" 'function)) :unsigned-int
  (toQuery :pointer))

(cffi:defcfun ("rmImageGetCopyFlag" #.(openrm-lispify "rmImageGetCopyFlag" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmImageGetType" #.(openrm-lispify "rmImageGetType" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmImageGetFormat" #.(openrm-lispify "rmImageGetFormat" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmImageGetImageSize" #.(openrm-lispify "rmImageGetImageSize" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnNDims :pointer)
  (returnWidth :pointer)
  (returnHeight :pointer)
  (returnDepth :pointer)
  (returnElements :pointer)
  (returnBytesPerScanline :pointer))

(cffi:defcfun ("rmImageSetPixelZoom" #.(openrm-lispify "rmImageSetPixelZoom" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (xzoom :float)
  (yzoom :float))

(cffi:defcfun ("rmImageGetPixelZoom" #.(openrm-lispify "rmImageGetPixelZoom" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnXZoom float-pointer)
  (returnYZoom float-pointer))

(cffi:defcfun ("rmImageSetVismap" #.(openrm-lispify "rmImageSetVismap" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (vismap :pointer))

(cffi:defcfun ("rmImageGetVismap" #.(openrm-lispify "rmImageGetVismap" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (vismapReturn :pointer))

(cffi:defcfun ("rmImageMirror" #.(openrm-lispify "rmImageMirror" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toMirror :pointer)
  (mirrorAxisEnum #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmImageResize" #.(openrm-lispify "rmImageResize" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (src :pointer)
  (dst :pointer)
  (hardwareEnum #.(openrm-lispify "RMenum" 'enumname))
  (p :pointer))

(cffi:defcfun ("rmImageSetScale" #.(openrm-lispify "rmImageSetScale" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newScale :float))

(cffi:defcfun ("rmImageGetScale" #.(openrm-lispify "rmImageGetScale" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnScale float-pointer))

(cffi:defcfun ("rmImageSetBias" #.(openrm-lispify "rmImageSetBias" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newBias :float))

(cffi:defcfun ("rmImageGetBias" #.(openrm-lispify "rmImageGetBias" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnBias float-pointer))

(cffi:defcfun ("rmLightNew" #.(openrm-lispify "rmLightNew" 'function)) :pointer)

(cffi:defcfun ("rmLightDelete" #.(openrm-lispify "rmLightDelete" 'function)) :void
  (toDelete :pointer))

(cffi:defcfun ("rmLightSetType" #.(openrm-lispify "rmLightSetType" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newType #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmLightGetType" #.(openrm-lispify "rmLightGetType" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmLightSetXYZ" #.(openrm-lispify "rmLightSetXYZ" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newXYZ :pointer))

(cffi:defcfun ("rmLightGetXYZ" #.(openrm-lispify "rmLightGetXYZ" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (retXYZ :pointer))

(cffi:defcfun ("rmLightSetColor" #.(openrm-lispify "rmLightSetColor" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newAmbientColor :pointer)
  (newDiffuseColor :pointer)
  (newSpecularColor :pointer))

(cffi:defcfun ("rmLightGetColor" #.(openrm-lispify "rmLightGetColor" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (retAmbientColor :pointer)
  (retDiffuseColor :pointer)
  (retSpecularColor :pointer))

(cffi:defcfun ("rmLightSetAttenuation" #.(openrm-lispify "rmLightSetAttenuation" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newConstantAttenuation :float)
  (newLinearAttenuation :float)
  (newQuadraticAttenuation :float))

(cffi:defcfun ("rmLightGetAttenuation" #.(openrm-lispify "rmLightGetAttenuation" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (retConstantAttenuation float-pointer)
  (retLinearAttenuation float-pointer)
  (retQuadraticAttenuation float-pointer))

(cffi:defcfun ("rmLightSetEnable" #.(openrm-lispify "rmLightSetEnable" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newValue #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmLightGetEnable" #.(openrm-lispify "rmLightGetEnable" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmLightSetSpotDirection" #.(openrm-lispify "rmLightSetSpotDirection" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newSpotDirection :pointer))

(cffi:defcfun ("rmLightGetSpotDirection" #.(openrm-lispify "rmLightGetSpotDirection" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (retDirection :pointer))

(cffi:defcfun ("rmLightSetSpotCutoff" #.(openrm-lispify "rmLightSetSpotCutoff" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newValue :float))

(cffi:defcfun ("rmLightGetSpotCutoff" #.(openrm-lispify "rmLightGetSpotCutoff" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (retValue float-pointer))

(cffi:defcfun ("rmLightSetSpotExponent" #.(openrm-lispify "rmLightSetSpotExponent" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newValue :float))

(cffi:defcfun ("rmLightGetSpotExponent" #.(openrm-lispify "rmLightGetSpotExponent" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (retValue float-pointer))

(cffi:defcfun ("rmLightModelNew" #.(openrm-lispify "rmLightModelNew" 'function)) :pointer)

(cffi:defcfun ("rmLightModelDelete" #.(openrm-lispify "rmLightModelDelete" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toDelete :pointer))

(cffi:defcfun ("rmLightModelSetAmbient" #.(openrm-lispify "rmLightModelSetAmbient" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newAmbientColor :pointer))

(cffi:defcfun ("rmLightModelGetAmbient" #.(openrm-lispify "rmLightModelGetAmbient" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (retAmbientColor :pointer))

(cffi:defcfun ("rmLightModelSetTwoSided" #.(openrm-lispify "rmLightModelSetTwoSided" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newValue #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmLightModelGetTwoSided" #.(openrm-lispify "rmLightModelGetTwoSided" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmLightModelSetLocalViewer" #.(openrm-lispify "rmLightModelSetLocalViewer" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newValue #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmLightModelGetLocalViewer" #.(openrm-lispify "rmLightModelGetLocalViewer" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmDefaultLighting" #.(openrm-lispify "rmDefaultLighting" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer))

(cffi:defcfun ("rmFloatNew" #.(openrm-lispify "rmFloatNew" 'function)) :pointer
  (num_floats :int))

(cffi:defcfun ("rmFloatDelete" #.(openrm-lispify "rmFloatDelete" 'function)) :void
  (f float-pointer))

(cffi:defcfun ("rmVertex2DNew" #.(openrm-lispify "rmVertex2DNew" 'function)) :pointer
  (n :int))

(cffi:defcfun ("rmVertex2DDelete" #.(openrm-lispify "rmVertex2DDelete" 'function)) :void
  (arg0 :pointer))

(cffi:defcfun ("rmVertex3DNew" #.(openrm-lispify "rmVertex3DNew" 'function)) :pointer
  (n :int))

(cffi:defcfun ("rmVertex3DDelete" #.(openrm-lispify "rmVertex3DDelete" 'function)) :void
  (arg0 :pointer))

(cffi:defcfun ("rmColor4DNew" #.(openrm-lispify "rmColor4DNew" 'function)) :pointer
  (n :int))

(cffi:defcfun ("rmColor4DDelete" #.(openrm-lispify "rmColor4DDelete" 'function)) :void
  (to_delete :pointer))

(cffi:defcfun ("rmInternalMarker2DNew" #.(openrm-lispify "rmInternalMarker2DNew" 'function)) :pointer
  (nverts :int)
  (begin_flag :int)
  (dverts :pointer))

(cffi:defcfun ("rmInternalMarker2DDelete" #.(openrm-lispify "rmInternalMarker2DDelete" 'function)) :void
  (t_arg0 :pointer))

(cffi:defcfun ("rmMalloc2DByteBuffer" #.(openrm-lispify "rmMalloc2DByteBuffer" 'function)) :pointer
  (width :int)
  (height :int))

(cffi:defcfun ("rmFree2DByteBuffer" #.(openrm-lispify "rmFree2DByteBuffer" 'function)) :void
  (c :pointer))

(cffi:defcfun ("rmMalloc2DFloatBuffer" #.(openrm-lispify "rmMalloc2DFloatBuffer" 'function)) :pointer
  (width :int)
  (height :int))

(cffi:defcfun ("rmFree2DFloatBuffer" #.(openrm-lispify "rmFree2DFloatBuffer" 'function)) :void
  (f :pointer))

(cffi:defcfun ("rmMalloc3DByteBuffer" #.(openrm-lispify "rmMalloc3DByteBuffer" 'function)) :pointer
  (width :int)
  (height :int)
  (depth :int))

(cffi:defcfun ("rmFree3DByteBuffer" #.(openrm-lispify "rmFree3DByteBuffer" 'function)) :void
  (c :pointer))

(cffi:defcfun ("rmMalloc3DFloatBuffer" #.(openrm-lispify "rmMalloc3DFloatBuffer" 'function)) :pointer
  (width :int)
  (height :int)
  (depth :int))

(cffi:defcfun ("rmFree3DFloatBuffer" #.(openrm-lispify "rmFree3DFloatBuffer" 'function)) :void
  (c :pointer))

(cffi:defcfun ("rmMatrixNew" #.(openrm-lispify "rmMatrixNew" 'function)) :pointer)

(cffi:defcfun ("rmMatrixCopy" #.(openrm-lispify "rmMatrixCopy" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (dst :pointer)
  (src :pointer))

(cffi:defcfun ("rmMatrixDelete" #.(openrm-lispify "rmMatrixDelete" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toDelete :pointer))

(cffi:defcfun ("rmMatrixIdentity" #.(openrm-lispify "rmMatrixIdentity" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer))

(cffi:defcfun ("rmMatrixSetValue" #.(openrm-lispify "rmMatrixSetValue" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (row :int)
  (col :int)
  (newValue :float))

(cffi:defcfun ("rmMatrixGetValue" #.(openrm-lispify "rmMatrixGetValue" 'function)) :float
  (toQuery :pointer)
  (row :int)
  (col :int))

(cffi:defcfun ("rmMatrixTranspose" #.(openrm-lispify "rmMatrixTranspose" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (src :pointer)
  (dst :pointer))

(cffi:defcfun ("rmMatrixMultiply" #.(openrm-lispify "rmMatrixMultiply" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (srcA :pointer)
  (srcB :pointer)
  (dst :pointer))

(cffi:defcfun ("rmMatrixInverse" #.(openrm-lispify "rmMatrixInverse" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (src :pointer)
  (dst :pointer))

(cffi:defcfun ("rmPoint4MatrixTransform" #.(openrm-lispify "rmPoint4MatrixTransform" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (src float-pointer)
  (matrix :pointer)
  (dst float-pointer))

(cffi:defcfun ("rmPointMatrixTransform" #.(openrm-lispify "rmPointMatrixTransform" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (src :pointer)
  (m :pointer)
  (dst :pointer))

(cffi:defcfun ("rmPrintMatrix" #.(openrm-lispify "rmPrintMatrix" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toPrint :pointer))

(cffi:defcfun ("rmPointMin" #.(openrm-lispify "rmPointMin" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (input float-pointer)
  (count :int)
  (vdims :int)
  (stride :int)
  (minReturn :pointer))

(cffi:defcfun ("rmPointMax" #.(openrm-lispify "rmPointMax" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (input float-pointer)
  (count :int)
  (vdims :int)
  (stride :int)
  (maxReturn :pointer))

(cffi:defcfun ("rmPointMinMax" #.(openrm-lispify "rmPointMinMax" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (input float-pointer)
  (count :int)
  (vdims :int)
  (stride :int)
  (minReturn :pointer)
  (maxReturn :pointer))

(cffi:defcfun ("rmVertex3DMag" #.(openrm-lispify "rmVertex3DMag" 'function)) :double
  (v :pointer))

(cffi:defcfun ("rmVertex3DSum" #.(openrm-lispify "rmVertex3DSum" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (a :pointer)
  (b :pointer)
  (dst :pointer))

(cffi:defcfun ("rmVertex3DDiff" #.(openrm-lispify "rmVertex3DDiff" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (a :pointer)
  (b :pointer)
  (dst :pointer))

(cffi:defcfun ("rmVertex3DDot" #.(openrm-lispify "rmVertex3DDot" 'function)) :double
  (a :pointer)
  (b :pointer))

(cffi:defcfun ("rmVertex3DCross" #.(openrm-lispify "rmVertex3DCross" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer)
  (r :pointer)
  (result :pointer))

(cffi:defcfun ("rmVertex3DNormalize" #.(openrm-lispify "rmVertex3DNormalize" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toNormalize :pointer))

(cffi:defcfun ("rmVertex3DMagNormalize" #.(openrm-lispify "rmVertex3DMagNormalize" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toNormalize :pointer)
  (magReturn :pointer))

(cffi:defcfun ("rmVertex3DMidpoint" #.(openrm-lispify "rmVertex3DMidpoint" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (a :pointer)
  (b :pointer)
  (dst :pointer))

(cffi:defcfun ("rmDCFromWC3" #.(openrm-lispify "rmDCFromWC3" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (src :pointer)
  (dst :pointer)
  (nPoints :int)
  (cam3d :pointer)
  (model :pointer)
  (viewPort :pointer)
  (windowWidth :int)
  (windowHeight :int))

(cffi:defcfun ("rmDCFromWC2" #.(openrm-lispify "rmDCFromWC2" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (src :pointer)
  (dst :pointer)
  (nPoints :int)
  (cam3d :pointer)
  (model :pointer)
  (viewPort :pointer)
  (windowWidth :int)
  (windowHeight :int))

(cffi:defcfun ("rmRootNode" #.(openrm-lispify "rmRootNode" 'function)) :pointer)

(cffi:defcfun ("rmNodeNew" #.(openrm-lispify "rmNodeNew" 'function)) :pointer
  (name :string)
  (renderpassVdims #.(openrm-lispify "RMenum" 'enumname))
  (renderpassOpaque #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmNodeDelete" #.(openrm-lispify "rmNodeDelete" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toDelete :pointer))

(cffi:defcfun ("rmSubTreeDelete" #.(openrm-lispify "rmSubTreeDelete" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toDelete :pointer))

(cffi:defcfun ("rmNodeSetName" #.(openrm-lispify "rmNodeSetName" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (name :string))

(cffi:defcfun ("rmNodeGetName" #.(openrm-lispify "rmNodeGetName" 'function)) :string
  (toQuery :pointer))

(cffi:defcfun ("rmNodeSetPickEnable" #.(openrm-lispify "rmNodeSetPickEnable" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newVal #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmNodeSetTraverseEnable" #.(openrm-lispify "rmNodeSetTraverseEnable" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newval #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmNodeGetTraverseEnable" #.(openrm-lispify "rmNodeGetTraverseEnable" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmNodeGetPickEnable" #.(openrm-lispify "rmNodeGetPickEnable" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmNodeAddChild" #.(openrm-lispify "rmNodeAddChild" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (parent :pointer)
  (child :pointer))

(cffi:defcfun ("rmNodeRemoveChild" #.(openrm-lispify "rmNodeRemoveChild" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (parent :pointer)
  (child :pointer))

(cffi:defcfun ("rmNodeRemoveAllChildren" #.(openrm-lispify "rmNodeRemoveAllChildren" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer))

(cffi:defcfun ("rmNodeRemoveAllPrims" #.(openrm-lispify "rmNodeRemoveAllPrims" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer))

(cffi:defcfun ("rmNodeGetNumChildren" #.(openrm-lispify "rmNodeGetNumChildren" 'function)) :int
  (toQuery :pointer))

(cffi:defcfun ("rmNodeGetNumPrims" #.(openrm-lispify "rmNodeGetNumPrims" 'function)) :int
  (toQuery :pointer))

(cffi:defcfun ("rmNodeGetIthChild" #.(openrm-lispify "rmNodeGetIthChild" 'function)) :pointer
  (toQuery :pointer)
  (indx :int))

(cffi:defcfun ("rmFindNamedNode" #.(openrm-lispify "rmFindNamedNode" 'function)) :pointer
  (start :pointer)
  (name :string))

(cffi:defcfun ("rmSceneGraphWalk" #.(openrm-lispify "rmSceneGraphWalk" 'function)) :void
  (p :pointer)
  (r :pointer)
  (userfunc :pointer)
  (clientData :pointer))

(cffi:defcfun ("rmPrintSceneGraph" #.(openrm-lispify "rmPrintSceneGraph" 'function)) :void
  (root :pointer)
  (printMode #.(openrm-lispify "RMenum" 'enumname))
  (fileName :string))

(cffi:defcfun ("rmNodeMutexInit" #.(openrm-lispify "rmNodeMutexInit" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (lockStatus #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmNodeMutexLock" #.(openrm-lispify "rmNodeMutexLock" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer))

(cffi:defcfun ("rmNodeMutexUnlock" #.(openrm-lispify "rmNodeMutexUnlock" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer))

(cffi:defcfun ("rmNodeMutexTryLock" #.(openrm-lispify "rmNodeMutexTryLock" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmNodeGetMutex" #.(openrm-lispify "rmNodeGetMutex" 'function)) :pointer
  (toQuery :pointer))

(cffi:defcfun ("rmNodeMutexDelete" #.(openrm-lispify "rmNodeMutexDelete" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer))

(cffi:defcfun ("rmNodeSetBoundingBox" #.(openrm-lispify "rmNodeSetBoundingBox" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (vMin :pointer)
  (vMax :pointer))

(cffi:defcfun ("rmNodeGetBoundingBox" #.(openrm-lispify "rmNodeGetBoundingBox" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (vMinReturn :pointer)
  (vMaxReturn :pointer))

(cffi:defcfun ("rmNodeComputeCenterFromBoundingBox" #.(openrm-lispify "rmNodeComputeCenterFromBoundingBox" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer))

(cffi:defcfun ("rmNodeComputeBoundingBox" #.(openrm-lispify "rmNodeComputeBoundingBox" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer))

(cffi:defcfun ("rmNodeUnionAllBoxes" #.(openrm-lispify "rmNodeUnionAllBoxes" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer))

(cffi:defcfun ("rmNodeSetAmbientColor" #.(openrm-lispify "rmNodeSetAmbientColor" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newColor :pointer))

(cffi:defcfun ("rmNodeGetAmbientColor" #.(openrm-lispify "rmNodeGetAmbientColor" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (ambientReturn :pointer))

(cffi:defcfun ("rmNodeSetDiffuseColor" #.(openrm-lispify "rmNodeSetDiffuseColor" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newColor :pointer))

(cffi:defcfun ("rmNodeGetDiffuseColor" #.(openrm-lispify "rmNodeGetDiffuseColor" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (diffuseReturn :pointer))

(cffi:defcfun ("rmNodeSetSpecularColor" #.(openrm-lispify "rmNodeSetSpecularColor" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newColor :pointer))

(cffi:defcfun ("rmNodeGetSpecularColor" #.(openrm-lispify "rmNodeGetSpecularColor" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (diffuseReturn :pointer))

(cffi:defcfun ("rmNodeSetSpecularExponent" #.(openrm-lispify "rmNodeSetSpecularExponent" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newValue :float))

(cffi:defcfun ("rmNodeGetSpecularExponent" #.(openrm-lispify "rmNodeGetSpecularExponent" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (retValue float-pointer))

(cffi:defcfun ("rmNodeSetUnlitColor" #.(openrm-lispify "rmNodeSetUnlitColor" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newColor :pointer))

(cffi:defcfun ("rmNodeGetUnlitColor" #.(openrm-lispify "rmNodeGetUnlitColor" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (retColor :pointer))

(cffi:defcfun ("rmNodeSetOpacity" #.(openrm-lispify "rmNodeSetOpacity" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newValue :float))

(cffi:defcfun ("rmNodeGetOpacity" #.(openrm-lispify "rmNodeGetOpacity" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (retValue float-pointer))

(cffi:defcfun ("rmNodeSetNormalizeNormals" #.(openrm-lispify "rmNodeSetNormalizeNormals" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newValue #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmNodeGetNormalizeNormals" #.(openrm-lispify "rmNodeGetNormalizeNormals" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (retValue :pointer))

(cffi:defcfun ("rmNodeSetLineStyle" #.(openrm-lispify "rmNodeSetLineStyle" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newStyle #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmNodeGetLineStyle" #.(openrm-lispify "rmNodeGetLineStyle" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (retStyle :pointer))

(cffi:defcfun ("rmNodeSetLineWidth" #.(openrm-lispify "rmNodeSetLineWidth" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newWidthEnum #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmNodeGetLineWidth" #.(openrm-lispify "rmNodeGetLineWidth" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (retWidthEnum :pointer))

(cffi:defcfun ("rmNodeSetPointSize" #.(openrm-lispify "rmNodeSetPointSize" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newsize :float))

(cffi:defcfun ("rmNodeGetPointSize" #.(openrm-lispify "rmNodeGetPointSize" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (sizeReturn float-pointer))

(cffi:defcfun ("rmNodeSetShader" #.(openrm-lispify "rmNodeSetShader" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newShaderEnum #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmNodeGetShader" #.(openrm-lispify "rmNodeGetShader" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (retShaderEnum :pointer))

(cffi:defcfun ("rmNodeSetPolygonDrawMode" #.(openrm-lispify "rmNodeSetPolygonDrawMode" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (whichFace #.(openrm-lispify "RMenum" 'enumname))
  (newMode #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmNodeGetPolygonDrawMode" #.(openrm-lispify "rmNodeGetPolygonDrawMode" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnFace :pointer)
  (returnMode :pointer))

(cffi:defcfun ("rmNodeSetPolygonCullMode" #.(openrm-lispify "rmNodeSetPolygonCullMode" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newMode #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmNodeGetPolygonCullMode" #.(openrm-lispify "rmNodeGetPolygonCullMode" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (modeReturn :pointer))

(cffi:defcfun ("rmNodeSetFrontFace" #.(openrm-lispify "rmNodeSetFrontFace" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newMode #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmNodeGetFrontFace" #.(openrm-lispify "rmNodeGetFrontFace" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (modeReturn :pointer))

(cffi:defcfun ("rmNodeSetTraversalMaskOpacity" #.(openrm-lispify "rmNodeSetTraversalMaskOpacity" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (opacityTraversalMask #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmNodeGetTraversalMaskOpacity" #.(openrm-lispify "rmNodeGetTraversalMaskOpacity" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (opacityTraversalMaskReturn :pointer))

(cffi:defcfun ("rmNodeSetTraversalMaskDims" #.(openrm-lispify "rmNodeSetTraversalMaskDims" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (n :pointer)
  (dimsTraversalMask #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmNodeGetTraversalMaskDims" #.(openrm-lispify "rmNodeGetTraversalMaskDims" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (n :pointer)
  (dimsTraversalMaskReturn :pointer))

(cffi:defcfun ("rmNodeSetTraversalMaskChannel" #.(openrm-lispify "rmNodeSetTraversalMaskChannel" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newval #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmNodeGetTraversalMaskChannel" #.(openrm-lispify "rmNodeGetTraversalMaskChannel" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (channelTraversalMaskReturn :pointer))

(cffi:defcfun ("rmNodeSetTransformMode" #.(openrm-lispify "rmNodeSetTransformMode" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newMode #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmNodeGetTransformMode" #.(openrm-lispify "rmNodeGetTransformMode" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmNodeSetPreMatrix" #.(openrm-lispify "rmNodeSetPreMatrix" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newMatrix :pointer))

(cffi:defcfun ("rmNodeGetPreMatrix" #.(openrm-lispify "rmNodeGetPreMatrix" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (matrixReturn :pointer))

(cffi:defcfun ("rmNodeSetCenter" #.(openrm-lispify "rmNodeSetCenter" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newVertex :pointer))

(cffi:defcfun ("rmNodeGetCenter" #.(openrm-lispify "rmNodeGetCenter" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (retVector :pointer))

(cffi:defcfun ("rmNodeSetScaleMatrix" #.(openrm-lispify "rmNodeSetScaleMatrix" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newMatrix :pointer))

(cffi:defcfun ("rmNodeGetScaleMatrix" #.(openrm-lispify "rmNodeGetScaleMatrix" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (matrixReturn :pointer))

(cffi:defcfun ("rmNodeSetRotateMatrix" #.(openrm-lispify "rmNodeSetRotateMatrix" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newMatrix :pointer))

(cffi:defcfun ("rmNodeGetRotateMatrix" #.(openrm-lispify "rmNodeGetRotateMatrix" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (matrixReturn :pointer))

(cffi:defcfun ("rmNodeSetPostRotateScaleMatrix" #.(openrm-lispify "rmNodeSetPostRotateScaleMatrix" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newMatrix :pointer))

(cffi:defcfun ("rmNodeGetPostRotateScaleMatrix" #.(openrm-lispify "rmNodeGetPostRotateScaleMatrix" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (matrixReturn :pointer))

(cffi:defcfun ("rmNodeSetTranslateVector" #.(openrm-lispify "rmNodeSetTranslateVector" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newVector :pointer))

(cffi:defcfun ("rmNodeGetTranslateVector" #.(openrm-lispify "rmNodeGetTranslateVector" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnVector :pointer))

(cffi:defcfun ("rmNodeSetPostMatrix" #.(openrm-lispify "rmNodeSetPostMatrix" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newMatrix :pointer))

(cffi:defcfun ("rmNodeGetPostMatrix" #.(openrm-lispify "rmNodeGetPostMatrix" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (matrixReturn :pointer))

(cffi:defcfun ("rmNodeGetCompositeModelMatrix" #.(openrm-lispify "rmNodeGetCompositeModelMatrix" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (transformReturn :pointer))

(cffi:defcfun ("rmNodeSetSwitchCallback" #.(openrm-lispify "rmNodeSetSwitchCallback" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (appFunc :pointer))

(cffi:defcfun ("rmNodeSetPreTraversalCallback" #.(openrm-lispify "rmNodeSetPreTraversalCallback" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (whichPass #.(openrm-lispify "RMenum" 'enumname))
  (appFunc :pointer))

(cffi:defcfun ("rmNodeSetPostTraversalCallback" #.(openrm-lispify "rmNodeSetPostTraversalCallback" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (whichPass #.(openrm-lispify "RMenum" 'enumname))
  (appFunc :pointer))

(cffi:defcfun ("rmNodeSetRenderOrderCallback" #.(openrm-lispify "rmNodeSetRenderOrderCallback" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (appFunc :pointer))

(cffi:defcfun ("rmNodeFrustumCullCallback" #.(openrm-lispify "rmNodeFrustumCullCallback" 'function)) :int
  (n :pointer)
  (s :pointer))

(cffi:defcfun ("rmInit" #.(openrm-lispify "rmInit" 'function)) :void)

(cffi:defcfun ("rmFinish" #.(openrm-lispify "rmFinish" 'function)) :void)

(cffi:defcfun ("rmFramePick" #.(openrm-lispify "rmFramePick" 'function)) :pointer
  (renderPipe :pointer)
  (subTree :pointer)
  (xpick :int)
  (ypick :int))

(cffi:defcfun ("rmFramePickList" #.(openrm-lispify "rmFramePickList" 'function)) :int
  (renderPipe :pointer)
  (subTree :pointer)
  (xpick :int)
  (ypick :int)
  (listReturn :pointer))

(cffi:defcfun ("rmPickDelete" #.(openrm-lispify "rmPickDelete" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toDelete :pointer))

(cffi:defcfun ("rmPickedNode" #.(openrm-lispify "rmPickedNode" 'function)) :pointer
  (toQuery :pointer))

(cffi:defcfun ("rmPickedPrimitive" #.(openrm-lispify "rmPickedPrimitive" 'function)) :int
  (toQuery :pointer))

(cffi:defcfun ("rmPickedNodeName" #.(openrm-lispify "rmPickedNodeName" 'function)) :string
  (toQuery :pointer))

(cffi:defcfun ("rmPickedPrimitiveZval" #.(openrm-lispify "rmPickedPrimitiveZval" 'function)) :float
  (toQuery :pointer))

(cffi:defcfun ("rmPrimitiveNew" #.(openrm-lispify "rmPrimitiveNew" 'function)) :pointer
  (primType #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmPrimitiveDelete" #.(openrm-lispify "rmPrimitiveDelete" 'function)) :void
  (toDelete :pointer))

(cffi:defcfun ("rmPrimitiveSetRenderFunc" #.(openrm-lispify "rmPrimitiveSetRenderFunc" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (drawFunc :pointer))

(cffi:defcfun ("rmPrimitiveGetRenderFunc" #.(openrm-lispify "rmPrimitiveGetRenderFunc" 'function)) :pointer
  (toQuery :pointer))

(cffi:defcfun ("rmPrimitiveSetModelFlag" #.(openrm-lispify "rmPrimitiveSetModelFlag" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newVal :int))

(cffi:defcfun ("rmPrimitiveGetModelFlag" #.(openrm-lispify "rmPrimitiveGetModelFlag" 'function)) :int
  (toQuery :pointer))

(cffi:defcfun ("rmPrimitiveSetBoundingBox" #.(openrm-lispify "rmPrimitiveSetBoundingBox" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer)
  (bmin :pointer)
  (bmax :pointer))

(cffi:defcfun ("rmPrimitiveGetBoundingBox" #.(openrm-lispify "rmPrimitiveGetBoundingBox" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer)
  (bmin :pointer)
  (bmax :pointer))

(cffi:defcfun ("rmPrimitiveComputeBoundingBox" #.(openrm-lispify "rmPrimitiveComputeBoundingBox" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer))

(cffi:defcfun ("rmPrimitiveGetType" #.(openrm-lispify "rmPrimitiveGetType" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmPrimitiveSetClientData" #.(openrm-lispify "rmPrimitiveSetClientData" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (clientData :pointer)
  (cdFreeFunc :pointer))

(cffi:defcfun ("rmPrimitiveGetClientData" #.(openrm-lispify "rmPrimitiveGetClientData" 'function)) :pointer
  (toQuery :pointer))

(cffi:defcfun ("rmPrimitiveSetAppDisplayList" #.(openrm-lispify "rmPrimitiveSetAppDisplayList" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (displayListID :unsigned-int))

(cffi:defcfun ("rmPrimitiveSetVertex2D" #.(openrm-lispify "rmPrimitiveSetVertex2D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (nVertices :int)
  (vertexData :pointer)
  (copyEnum #.(openrm-lispify "RMenum" 'enumname))
  (appFreeFunc :pointer))

(cffi:defcfun ("rmPrimitiveSetVertex3D" #.(openrm-lispify "rmPrimitiveSetVertex3D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (nVertices :int)
  (vertexData :pointer)
  (copyEnum #.(openrm-lispify "RMenum" 'enumname))
  (appFreeFunc :pointer))

(cffi:defcfun ("rmPrimitiveSetColor3D" #.(openrm-lispify "rmPrimitiveSetColor3D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (nColors :int)
  (colorData :pointer)
  (copyEnum #.(openrm-lispify "RMenum" 'enumname))
  (appFreeFunc :pointer))

(cffi:defcfun ("rmPrimitiveSetColor4D" #.(openrm-lispify "rmPrimitiveSetColor4D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (nColors :int)
  (colorData :pointer)
  (copyEnum #.(openrm-lispify "RMenum" 'enumname))
  (appFreeFunc :pointer))

(cffi:defcfun ("rmPrimitiveSetRadii" #.(openrm-lispify "rmPrimitiveSetRadii" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (nRadii :int)
  (radii float-pointer)
  (copyEnum #.(openrm-lispify "RMenum" 'enumname))
  (freeFunc :pointer))

(cffi:defcfun ("rmPrimitiveSetNormal3D" #.(openrm-lispify "rmPrimitiveSetNormal3D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (nNormals :int)
  (normalsData :pointer)
  (copyEnum #.(openrm-lispify "RMenum" 'enumname))
  (freeFunc :pointer))

(cffi:defcfun ("rmPrimitiveSetTexcoord1D" #.(openrm-lispify "rmPrimitiveSetTexcoord1D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (nTexCoords :int)
  (texCoordData float-pointer)
  (copyEnum #.(openrm-lispify "RMenum" 'enumname))
  (appFreeFunc :pointer))

(cffi:defcfun ("rmPrimitiveSetTexcoord2D" #.(openrm-lispify "rmPrimitiveSetTexcoord2D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (nTexCoords :int)
  (texCoordData :pointer)
  (copyEnum #.(openrm-lispify "RMenum" 'enumname))
  (appFreeFunc :pointer))

(cffi:defcfun ("rmPrimitiveSetTexcoord3D" #.(openrm-lispify "rmPrimitiveSetTexcoord3D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (nTexcoords :int)
  (texCoordData :pointer)
  (copyEnum #.(openrm-lispify "RMenum" 'enumname))
  (appFreeFunc :pointer))

(cffi:defcfun ("rmPrimitiveSetMultiTexcoord1D" #.(openrm-lispify "rmPrimitiveSetMultiTexcoord1D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (nTexCoords :int)
  (texCoordData float-pointer)
  (copyEnum #.(openrm-lispify "RMenum" 'enumname))
  (appFreeFunc :pointer)
  (textureUnitIndx :int))

(cffi:defcfun ("rmPrimitiveSetMultiTexcoord2D" #.(openrm-lispify "rmPrimitiveSetMultiTexcoord2D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (nTexCoords :int)
  (texCoordData :pointer)
  (copyEnum #.(openrm-lispify "RMenum" 'enumname))
  (appFreeFunc :pointer)
  (textureUnitIndx :int))

(cffi:defcfun ("rmPrimitiveSetMultiTexcoord3D" #.(openrm-lispify "rmPrimitiveSetMultiTexcoord3D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (nTexcoords :int)
  (texCoordData :pointer)
  (copyEnum #.(openrm-lispify "RMenum" 'enumname))
  (appFreeFunc :pointer)
  (textureUnitIndx :int))

(cffi:defcfun ("rmPrimitiveSetIndices" #.(openrm-lispify "rmPrimitiveSetIndices" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (numIndices :int)
  (indicesArray :pointer)
  (copyEnum #.(openrm-lispify "RMenum" 'enumname))
  (appFreeFunc :pointer))

(cffi:defcfun ("rmPrimitiveSetSprites" #.(openrm-lispify "rmPrimitiveSetSprites" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (nSprites :int)
  (spriteArray :pointer))

(cffi:defcfun ("rmPrimitiveSetBitmaps" #.(openrm-lispify "rmPrimitiveSetBitmaps" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (nBitmaps :int)
  (bmapArray :pointer))

(cffi:defcfun ("rmPrimitiveSetQmeshDims" #.(openrm-lispify "rmPrimitiveSetQmeshDims" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (uSize :int)
  (vSize :int))

(cffi:defcfun ("rmPrimitiveSetOmeshDims" #.(openrm-lispify "rmPrimitiveSetOmeshDims" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (isize :int)
  (jsize :int)
  (ksize :int))

(cffi:defcfun ("rmPrimitiveSetOmeshMinMaxGrid" #.(openrm-lispify "rmPrimitiveSetOmeshMinMaxGrid" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (gridMin :pointer)
  (gridMax :pointer))

(cffi:defcfun ("rmPrimitiveSetMarkerScale" #.(openrm-lispify "rmPrimitiveSetMarkerScale" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (npts :int)
  (scales float-pointer)
  (copyEnum #.(openrm-lispify "RMenum" 'enumname))
  (appFreeFunc :pointer))

(cffi:defcfun ("rmPrimitiveSetMarkerPrims" #.(openrm-lispify "rmPrimitiveSetMarkerPrims" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (nMarkerPrims :int)
  (mArray :pointer))

(cffi:defcfun ("rmPrimitiveSetEllipse2DRotate" #.(openrm-lispify "rmPrimitiveSetEllipse2DRotate" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (nVals :int)
  (rotationValues float-pointer)
  (copyEnum #.(openrm-lispify "RMenum" 'enumname))
  (appFreeFunc :pointer))

(cffi:defcfun ("rmPrimitiveSetText" #.(openrm-lispify "rmPrimitiveSetText" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (nStrings :int)
  (strings :pointer))

(cffi:defcfun ("rmPrimitiveSetDisplayListEnable" #.(openrm-lispify "rmPrimitiveSetDisplayListEnable" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newMode #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmNodeAddPrimitive" #.(openrm-lispify "rmNodeAddPrimitive" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (addTo :pointer)
  (src :pointer))

(cffi:defcfun ("rmNodeGetPrimitive" #.(openrm-lispify "rmNodeGetPrimitive" 'function)) :pointer
  (toQuery :pointer)
  (indx :int))

(cffi:defcfun ("rmNodeSetClientData" #.(openrm-lispify "rmNodeSetClientData" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (clientData :pointer)
  (cdFreeFunc :pointer))

(cffi:defcfun ("rmNodeGetClientData" #.(openrm-lispify "rmNodeGetClientData" 'function)) :pointer
  (toQuery :pointer))

(cffi:defcfun ("rmStateNew" #.(openrm-lispify "rmStateNew" 'function)) :pointer)

(cffi:defcfun ("rmStateDelete" #.(openrm-lispify "rmStateDelete" 'function)) :void
  (toDelete :pointer))

(cffi:defcfun ("rmStateCopy" #.(openrm-lispify "rmStateCopy" 'function)) :void
  (src :pointer)
  (dst :pointer))

(cffi:defcfun ("rmStateGetModelViewMatrix" #.(openrm-lispify "rmStateGetModelViewMatrix" 'function)) :pointer
  (toQuery :pointer))

(cffi:defcfun ("rmStateGetProjectionMatrix" #.(openrm-lispify "rmStateGetProjectionMatrix" 'function)) :pointer
  (toQuery :pointer))

(cffi:defcfun ("rmStateGetShader" #.(openrm-lispify "rmStateGetShader" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer))

(cffi:defcfun ("rmStateGetPolygonDrawMode" #.(openrm-lispify "rmStateGetPolygonDrawMode" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (whichFaceReturn :pointer)
  (drawModeReturn :pointer))

(cffi:defcfun ("rmStateGetPolygonCullMode" #.(openrm-lispify "rmStateGetPolygonCullMode" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (cullModeReturn :pointer))

(cffi:defcfun ("rmStateGetFrontFace" #.(openrm-lispify "rmStateGetFrontFace" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (frontFaceReturn :pointer))

(cffi:defcfun ("rmStateGetLineWidth" #.(openrm-lispify "rmStateGetLineWidth" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (lineWidthReturn :pointer))

(cffi:defcfun ("rmStateGetLineStyle" #.(openrm-lispify "rmStateGetLineStyle" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (lineStyleReturn :pointer))

(cffi:defcfun ("rmStateGetPointSize" #.(openrm-lispify "rmStateGetPointSize" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (sizeReturn float-pointer))

(cffi:defcfun ("rmStateGetFrameNumber" #.(openrm-lispify "rmStateGetFrameNumber" 'function)) :int
  (toQuery :pointer))

(cffi:defcfun ("rmNodeSetSceneBackgroundColor" #.(openrm-lispify "rmNodeSetSceneBackgroundColor" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newColor :pointer))

(cffi:defcfun ("rmNodeGetSceneBackgroundColor" #.(openrm-lispify "rmNodeGetSceneBackgroundColor" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnColor :pointer))

(cffi:defcfun ("rmNodeSetSceneBackgroundImage" #.(openrm-lispify "rmNodeSetSceneBackgroundImage" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newImageTile :pointer))

(cffi:defcfun ("rmNodeGetSceneBackgroundImage" #.(openrm-lispify "rmNodeGetSceneBackgroundImage" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnImageTile :pointer))

(cffi:defcfun ("rmNodeSetSceneCamera2D" #.(openrm-lispify "rmNodeSetSceneCamera2D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newCamera :pointer))

(cffi:defcfun ("rmNodeGetSceneCamera2D" #.(openrm-lispify "rmNodeGetSceneCamera2D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnCamera :pointer))

(cffi:defcfun ("rmNodeSetSceneCamera3D" #.(openrm-lispify "rmNodeSetSceneCamera3D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newCamera :pointer))

(cffi:defcfun ("rmNodeGetSceneCamera3D" #.(openrm-lispify "rmNodeGetSceneCamera3D" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnCamera :pointer))

(cffi:defcfun ("rmNodeSetSceneClipPlane" #.(openrm-lispify "rmNodeSetSceneClipPlane" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (clipPlaneEnum #.(openrm-lispify "RMenum" 'enumname))
  (newClipPlane :pointer))

(cffi:defcfun ("rmNodeGetSceneClipPlane" #.(openrm-lispify "rmNodeGetSceneClipPlane" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (clipPlaneEnum #.(openrm-lispify "RMenum" 'enumname))
  (returnClipPlaneCopy :pointer))

(cffi:defcfun ("rmNodeSetSceneDepthImage" #.(openrm-lispify "rmNodeSetSceneDepthImage" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newDepthImage :pointer))

(cffi:defcfun ("rmNodeGetSceneDepthImage" #.(openrm-lispify "rmNodeGetSceneDepthImage" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnDepthImage :pointer))

(cffi:defcfun ("rmNodeSetSceneDepthValue" #.(openrm-lispify "rmNodeSetSceneDepthValue" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newDepthValue float-pointer))

(cffi:defcfun ("rmNodeGetSceneDepthValue" #.(openrm-lispify "rmNodeGetSceneDepthValue" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnDepthValue float-pointer))

(cffi:defcfun ("rmNodeSetSceneFog" #.(openrm-lispify "rmNodeSetSceneFog" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newFog :pointer))

(cffi:defcfun ("rmNodeGetSceneFog" #.(openrm-lispify "rmNodeGetSceneFog" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnFog :pointer))

(cffi:defcfun ("rmNodeSetSceneLight" #.(openrm-lispify "rmNodeSetSceneLight" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (whichLightEnum #.(openrm-lispify "RMenum" 'enumname))
  (newLight :pointer))

(cffi:defcfun ("rmNodeGetSceneLight" #.(openrm-lispify "rmNodeGetSceneLight" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (whichLightEnum #.(openrm-lispify "RMenum" 'enumname))
  (returnLightCopy :pointer))

(cffi:defcfun ("rmNodeSetSceneLightModel" #.(openrm-lispify "rmNodeSetSceneLightModel" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (newLightModel :pointer))

(cffi:defcfun ("rmNodeGetSceneLightModel" #.(openrm-lispify "rmNodeGetSceneLightModel" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (retLightModel :pointer))

(cffi:defcfun ("rmNodeGetSceneMultiTexture" #.(openrm-lispify "rmNodeGetSceneMultiTexture" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (n :pointer)
  (textureUnitIndx :int)
  (t_arg2 :pointer))

(cffi:defcfun ("rmNodeSetSceneMultiTexture" #.(openrm-lispify "rmNodeSetSceneMultiTexture" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (n :pointer)
  (t_arg1 :pointer)
  (textureUnit :int))

(cffi:defcfun ("rmTextPropsNew" #.(openrm-lispify "rmTextPropsNew" 'function)) :pointer)

(cffi:defcfun ("rmTextPropsDelete" #.(openrm-lispify "rmTextPropsDelete" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (t_arg0 :pointer))

(cffi:defcfun ("rmNodeSetSceneTextProps" #.(openrm-lispify "rmNodeSetSceneTextProps" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newTextProps :pointer))

(cffi:defcfun ("rmNodeGetSceneTextProps" #.(openrm-lispify "rmNodeGetSceneTextProps" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnTextProps :pointer))

(cffi:defcfun ("rmNodeSetSceneTexture" #.(openrm-lispify "rmNodeSetSceneTexture" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newTexture :pointer))

(cffi:defcfun ("rmNodeGetSceneTexture" #.(openrm-lispify "rmNodeGetSceneTexture" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnTexture :pointer))

(cffi:defcfun ("rmNodeSetSceneViewport" #.(openrm-lispify "rmNodeSetSceneViewport" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (newViewport float-pointer))

(cffi:defcfun ("rmNodeGetSceneViewport" #.(openrm-lispify "rmNodeGetSceneViewport" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnViewport :pointer))

(cffi:defcfun ("rmStatsComputeDemography" #.(openrm-lispify "rmStatsComputeDemography" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (r :pointer))

(cffi:defcfun ("rmStatsPrint" #.(openrm-lispify "rmStatsPrint" 'function)) #.(openrm-lispify "RMenum" 'enumname))

(cffi:defcfun ("rmStatsEndTime" #.(openrm-lispify "rmStatsEndTime" 'function)) #.(openrm-lispify "RMenum" 'enumname))

(cffi:defcfun ("rmStatsStartTime" #.(openrm-lispify "rmStatsStartTime" 'function)) #.(openrm-lispify "RMenum" 'enumname))

(cffi:defcfun ("rmTextPropsSetAttribs" #.(openrm-lispify "rmTextPropsSetAttribs" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (fontEnum :int)
  (sizeEnum :int)
  (boldEnum #.(openrm-lispify "RMenum" 'enumname))
  (italicEnum #.(openrm-lispify "RMenum" 'enumname))
  (hJustifyEnum #.(openrm-lispify "RMenum" 'enumname))
  (vJustifyEnum #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmTextPropsGetAttribs" #.(openrm-lispify "rmTextPropsGetAttribs" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (fontEnumReturn :pointer)
  (sizeEnumReturn :pointer)
  (boldEnumReturn :pointer)
  (italicEnumReturn :pointer)
  (hJustifyReturn :pointer)
  (vJustifyReturn :pointer))

(cffi:defcfun ("rmTextGetExtents" #.(openrm-lispify "rmTextGetExtents" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (string :string)
  (fontEnum :int)
  (sizeEnum :int)
  (boldEnum #.(openrm-lispify "RMenum" 'enumname))
  (italicEnum #.(openrm-lispify "RMenum" 'enumname))
  (widthReturn :pointer)
  (heightReturn :pointer)
  (renderPipe :pointer))

(cffi:defcfun ("rmTimeNew" #.(openrm-lispify "rmTimeNew" 'function)) :pointer)

(cffi:defcfun ("rmTimeDelete" #.(openrm-lispify "rmTimeDelete" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toDelete :pointer))

(cffi:defcfun ("rmTimeCurrent" #.(openrm-lispify "rmTimeCurrent" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (returnVal :pointer))

(cffi:defcfun ("rmTimeDifference" #.(openrm-lispify "rmTimeDifference" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (startTime :pointer)
  (endTime :pointer)
  (diffTime :pointer))

(cffi:defcfun ("rmTimeDifferenceMS" #.(openrm-lispify "rmTimeDifferenceMS" 'function)) :double
  (startTime :pointer)
  (endTime :pointer))

(cffi:defcfun ("rmTimeSet" #.(openrm-lispify "rmTimeSet" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (secs :long)
  (usecs :long))

(cffi:defcfun ("rmTimeGet" #.(openrm-lispify "rmTimeGet" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnSecs :pointer)
  (returnUSecs :pointer))

(cffi:defcfun ("rmTimeSleep" #.(openrm-lispify "rmTimeSleep" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (tSleep :pointer))

(cffi:defcfun ("rmTimeEncodeMS" #.(openrm-lispify "rmTimeEncodeMS" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (result :pointer)
  (ms :double))

(cffi:defcfun ("rmTimeDecodeMS" #.(openrm-lispify "rmTimeDecodeMS" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (src :pointer)
  (resultMS :pointer))

(cffi:defcfun ("rmTextureNew" #.(openrm-lispify "rmTextureNew" 'function)) :pointer
  (ntdims :int))

(cffi:defcfun ("rmTextureDelete" #.(openrm-lispify "rmTextureDelete" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toDelete :pointer)
  (deleteImagesBool #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmTextureSetImages" #.(openrm-lispify "rmTextureSetImages" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (imageArray :pointer)
  (nimages :int)
  (borderWidth :int))

(cffi:defcfun ("rmTextureGetImages" #.(openrm-lispify "rmTextureGetImages" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (imageArray :pointer)
  (nimages :pointer)
  (borderWidth :pointer))

(cffi:defcfun ("rmTextureSetFilterMode" #.(openrm-lispify "rmTextureSetFilterMode" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (minMode :unsigned-int)
  (magMode :unsigned-int))

(cffi:defcfun ("rmTextureGetFilterMode" #.(openrm-lispify "rmTextureGetFilterMode" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (minModeReturn :pointer)
  (magModeReturn :pointer))

(cffi:defcfun ("rmTextureSetWrapMode" #.(openrm-lispify "rmTextureSetWrapMode" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (wrapMode :unsigned-int))

(cffi:defcfun ("rmTextureGetWrapMode" #.(openrm-lispify "rmTextureGetWrapMode" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (wrapModeReturn :pointer))

(cffi:defcfun ("rmTextureSetEnv" #.(openrm-lispify "rmTextureSetEnv" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (envMode :unsigned-int)
  (blendColor :pointer))

(cffi:defcfun ("rmTextureGetEnv" #.(openrm-lispify "rmTextureGetEnv" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (envModeReturn :pointer)
  (blendColorReturn :pointer))

(cffi:defcfun ("rmTextureSetGLTexelFormat" #.(openrm-lispify "rmTextureSetGLTexelFormat" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (internalTexelFormat :unsigned-int))

(cffi:defcfun ("rmTextureGetGLTexelFormat" #.(openrm-lispify "rmTextureGetGLTexelFormat" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnTexelFormat :pointer))

(cffi:defcfun ("rmTextureGetTextureID" #.(openrm-lispify "rmTextureGetTextureID" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toQuery :pointer)
  (returnTextureID :pointer))

(cffi:defcfun ("rmTextureSetTextureID" #.(openrm-lispify "rmTextureSetTextureID" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toModify :pointer)
  (textureID :pointer))

(cffi:defcfun ("rmNearestPowerOfTwo" #.(openrm-lispify "rmNearestPowerOfTwo" 'function)) :int
  (n :int))

(cffi:defcfun ("rmIntMagnitude" #.(openrm-lispify "rmIntMagnitude" 'function)) :int
  (m :int))

(cffi:defcfun ("rmHSVtoRGB" #.(openrm-lispify "rmHSVtoRGB" 'function)) :void
  (hue :float)
  (saturation :float)
  (value :float)
  (redReturn float-pointer)
  (greenReturn float-pointer)
  (blueReturn float-pointer))

(cffi:defcfun ("rmRGBtoHSV" #.(openrm-lispify "rmRGBtoHSV" 'function)) :void
  (red :float)
  (green :float)
  (blue :float)
  (hueReturn float-pointer)
  (saturationReturn float-pointer)
  (valueReturn float-pointer))

(cffi:defcfun ("rmUnionBoundingBoxes" #.(openrm-lispify "rmUnionBoundingBoxes" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (s1min :pointer)
  (s1max :pointer)
  (s2min :pointer)
  (s2max :pointer)
  (dmin :pointer)
  (dmax :pointer))

(cffi:defcfun ("rmGLGetError" #.(openrm-lispify "rmGLGetError" 'function)) :int
  (string :string))

(cffi:defcfun ("rmImageBuildMipmaps" #.(openrm-lispify "rmImageBuildMipmaps" 'function)) :int
  (src :pointer)
  (mapsReturn :pointer)
  (hardwareEnum #.(openrm-lispify "RMenum" 'enumname))
  (hwPipe :pointer))

(cffi:defcfun ("rmSwapBuffers" #.(openrm-lispify "rmSwapBuffers" 'function)) :void
  (p :pointer))

(cl:defconstant #.(openrm-lispify "RM_PS_DEFAULT_PAGE_WIDTH_POINTS" 'constant) 612)

(cl:defconstant #.(openrm-lispify "RM_PS_DEFAULT_PAGE_HEIGHT_POINTS" 'constant) 792)

(cl:defconstant #.(openrm-lispify "RM_PS_DEFAULT_ORIENTATION" 'constant) #x0100)

(cl:defconstant #.(openrm-lispify "RM_PS_DEFAULT_MIN_MARGIN" 'constant) 36)

(cl:defconstant #.(openrm-lispify "RM_DEFAULT_RENDERMODE" 'constant) #x0220)

(cl:defconstant #.(openrm-lispify "RM_DEFAULT_VIEWPORT_XMIN" 'constant) 0.0d0)

(cl:defconstant #.(openrm-lispify "RM_DEFAULT_VIEWPORT_YMIN" 'constant) 0.0d0)

(cl:defconstant #.(openrm-lispify "RM_DEFAULT_VIEWPORT_XMAX" 'constant) 1.0d0)

(cl:defconstant #.(openrm-lispify "RM_DEFAULT_VIEWPORT_YMAX" 'constant) 1.0d0)

(cffi:defcstruct #.(openrm-lispify "RMpsSpec" 'classname)
	(#.(openrm-lispify "psWidthPoints" 'slotname) :int)
	(#.(openrm-lispify "psHeightPoints" 'slotname) :int)
	(#.(openrm-lispify "psMinMargin" 'slotname) :int)
	(#.(openrm-lispify "psOrientation" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "psRasterOrVector" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "psOutputFormat" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "psSortMethod" 'slotname) #.(openrm-lispify "RMenum" 'enumname))
	(#.(openrm-lispify "fName" 'slotname) :string)
	(#.(openrm-lispify "fHandle" 'slotname) :pointer))

(cffi:defcfun ("rmFramePS" #.(openrm-lispify "rmFramePS" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (drawOn :pointer)
  (subTree :pointer)
  (controlStruct :pointer))

(cffi:defcfun ("rmFramePSHeartbeat" #.(openrm-lispify "rmFramePSHeartbeat" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (drawOn :pointer)
  (subTree :pointer)
  (p :pointer)
  (heartBeatFunc :pointer))

(cffi:defcfun ("rmPSSpecNew" #.(openrm-lispify "rmPSSpecNew" 'function)) :pointer)

(cffi:defcfun ("rmPSSpecDelete" #.(openrm-lispify "rmPSSpecDelete" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (toDelete :pointer))

(cffi:defcfun ("rmPSPageSetSize" #.(openrm-lispify "rmPSPageSetSize" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer)
  (pagePointsWidth :int)
  (pagePointsHeight :int))

(cffi:defcfun ("rmPSPageGetSize" #.(openrm-lispify "rmPSPageGetSize" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer)
  (pagePointsWidth :pointer)
  (pagePointsHeight :pointer))

(cffi:defcfun ("rmPSPageSetOrientation" #.(openrm-lispify "rmPSPageSetOrientation" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer)
  (orientation #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmPSPageGetOrientation" #.(openrm-lispify "rmPSPageGetOrientation" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer))

(cffi:defcfun ("rmPSPageSetOutputFormat" #.(openrm-lispify "rmPSPageSetOutputFormat" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer)
  (outputFormat #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmPSPageGetOutputFormat" #.(openrm-lispify "rmPSPageGetOutputFormat" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer))

(cffi:defcfun ("rmPSPageSetVectorFormat" #.(openrm-lispify "rmPSPageSetVectorFormat" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer)
  (trueOrFalse #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmPSPageGetVectorFormat" #.(openrm-lispify "rmPSPageGetVectorFormat" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer))

(cffi:defcfun ("rmPSPageSetMargin" #.(openrm-lispify "rmPSPageSetMargin" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer)
  (minMarginPoints :int))

(cffi:defcfun ("rmPSPageGetMargin" #.(openrm-lispify "rmPSPageGetMargin" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer)
  (minMarginPointsReturn :pointer))

(cffi:defcfun ("rmPSSetOutputFilename" #.(openrm-lispify "rmPSSetOutputFilename" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer)
  (fName :string))

(cffi:defcfun ("rmPSGetOutputFilename" #.(openrm-lispify "rmPSGetOutputFilename" 'function)) :string
  (p :pointer))

(cffi:defcfun ("rmPSSetSortMethod" #.(openrm-lispify "rmPSSetSortMethod" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer)
  (sortMethod #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmPSGetSortMethod" #.(openrm-lispify "rmPSGetSortMethod" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (p :pointer))

(cl:defconstant #.(openrm-lispify "RM_NUM_BUTTONS" 'constant) 5)

(cl:defconstant #.(openrm-lispify "RM_BUTTON1" 'constant) #x1)

(cl:defconstant #.(openrm-lispify "RM_BUTTON2" 'constant) #x2)

(cl:defconstant #.(openrm-lispify "RM_BUTTON3" 'constant) #x3)

(cl:defconstant #.(openrm-lispify "RM_BUTTON4" 'constant) #x4)

(cl:defconstant #.(openrm-lispify "RM_BUTTON5" 'constant) #x5)

(cl:defconstant #.(openrm-lispify "Button1" 'constant) #x1)

(cl:defconstant #.(openrm-lispify "Button2" 'constant) #x2)

(cl:defconstant #.(openrm-lispify "Button3" 'constant) #x3)

(cl:defconstant #.(openrm-lispify "Button4" 'constant) #x4)

(cl:defconstant #.(openrm-lispify "Button5" 'constant) #x5)

(cl:defconstant #.(openrm-lispify "RM_NUM_BUTTON_MODIFIERS" 'constant) 3)

(cl:defconstant #.(openrm-lispify "RM_NONE_MODMASK" 'constant) 0)

(cl:defconstant #.(openrm-lispify "RM_NO_MODIFIER" 'constant) 0)

(cl:defconstant #.(openrm-lispify "RM_SHIFT_MODIFIER" 'constant) 1)

(cl:defconstant #.(openrm-lispify "RM_CONTROL_MODIFIER" 'constant) 2)

(cl:defconstant #.(openrm-lispify "RM_SHIFT_MODMASK" 'constant) 1)

(cl:defconstant #.(openrm-lispify "RM_CONTROL_MODMASK" 'constant) 2)

(cl:defconstant #.(openrm-lispify "RMAUX_DEFAULT_SPIN_THRESHOLD" 'constant) 3.0d0)

(cffi:defcfun ("rmauxEventLoop" #.(openrm-lispify "rmauxEventLoop" 'function)) :void
  (drawToPipe :pointer)
  (subTreeToDraw :pointer)
  (msg :pointer))

(cffi:defcfun ("rmauxSetGeomTransform" #.(openrm-lispify "rmauxSetGeomTransform" 'function)) :void
  (target :pointer)
  (pipe :pointer))

(cffi:defcfun ("rmauxSetSpinEnable" #.(openrm-lispify "rmauxSetSpinEnable" 'function)) :void
  (spinEnableBool #.(openrm-lispify "RMenum" 'enumname)))

(cffi:defcfun ("rmauxGetSpinEnable" #.(openrm-lispify "rmauxGetSpinEnable" 'function)) #.(openrm-lispify "RMenum" 'enumname))

(cffi:defcfun ("rmauxSetCamera3DTransform" #.(openrm-lispify "rmauxSetCamera3DTransform" 'function)) :void
  (target :pointer)
  (pipe :pointer))

(cffi:defcfun ("rmauxUI" #.(openrm-lispify "rmauxUI" 'function)) :void
  (target :pointer)
  (pipe :pointer))

(cffi:defcfun ("rmauxSetInitFunc" #.(openrm-lispify "rmauxSetInitFunc" 'function)) :void
  (userinitfunc :pointer))

(cffi:defcfun ("rmauxSetIdleFunc" #.(openrm-lispify "rmauxSetIdleFunc" 'function)) :void
  (arg0 :pointer)
  (userfunc :pointer))

(cffi:defcfun ("rmauxSetKeyFunc" #.(openrm-lispify "rmauxSetKeyFunc" 'function)) :void
  (arg0 :pointer)
  (userfunc :pointer))

(cffi:defcfun ("rmauxDefaultKeyFunc" #.(openrm-lispify "rmauxDefaultKeyFunc" 'function)) :int
  (currentPipe :pointer)
  (key :char)
  (code :int))

(cffi:defcfun ("rmauxSetResizeFunc" #.(openrm-lispify "rmauxSetResizeFunc" 'function)) :void
  (arg0 :pointer)
  (cameraNode :pointer)
  (userfunc :pointer))

(cffi:defcfun ("rmauxDefaultResizeFunc" #.(openrm-lispify "rmauxDefaultResizeFunc" 'function)) :int
  (currentPipe :pointer)
  (cameraNode :pointer)
  (pointerX :int)
  (pointerY :int))

(cffi:defcfun ("rmauxSetRenderFunc" #.(openrm-lispify "rmauxSetRenderFunc" 'function)) :void
  (userfunc :pointer))

(cffi:defcfun ("rmauxSetButtonDownFunc" #.(openrm-lispify "rmauxSetButtonDownFunc" 'function)) :void
  (buttonnum :unsigned-int)
  (modmask :unsigned-int)
  (userfunc :pointer))

(cffi:defcfun ("rmauxSetButtonUpFunc" #.(openrm-lispify "rmauxSetButtonUpFunc" 'function)) :void
  (whichbutton :unsigned-int)
  (modmask :unsigned-int)
  (userfunc :pointer))

(cffi:defcfun ("rmauxSetButtonMotionFunc" #.(openrm-lispify "rmauxSetButtonMotionFunc" 'function)) :void
  (whichbutton :unsigned-int)
  (modmask :unsigned-int)
  (userfunc :pointer))

(cffi:defcfun ("rmauxWndProc" #.(openrm-lispify "rmauxWndProc" 'function)) :pointer
  (hWnd :pointer)
  (msg :pointer)
  (wParam :pointer)
  (lParam :pointer))

(cffi:defcfun ("rmauxCreateW32Window" #.(openrm-lispify "rmauxCreateW32Window" 'function)) :pointer
  (useMe :pointer)
  (parent :pointer)
  (xpos :int)
  (ypos :int)
  (width :int)
  (height :int)
  (wtitle :string)
  (hInstance :pointer)
  (event_loop_fptr :pointer))

(cffi:defcfun ("rmauxCreateOffscreenDrawable" #.(openrm-lispify "rmauxCreateOffscreenDrawable" 'function)) :pointer
  (pipe :pointer)
  (width :int)
  (height :int)
  (depth :int)
  (hCurrentInst :pointer)
  (eventLoopFuncPtr :pointer))

(cffi:defcfun ("rmauxFlyUI" #.(openrm-lispify "rmauxFlyUI" 'function)) :void
  (cameraNode :pointer)
  (drawNode :pointer)
  (pipe :pointer)
  (orientationScale :float)
  (translateScale :float))

(cffi:defcfun ("rmauxFlyResetCamera" #.(openrm-lispify "rmauxFlyResetCamera" 'function)) :int
  (p :pointer)
  (xbutton :int)
  (ybutton :int))

(cffi:defcfun ("rmauxFlyToggleMotion" #.(openrm-lispify "rmauxFlyToggleMotion" 'function)) :int
  (p :pointer)
  (xbutton :int)
  (ybutton :int))

(cffi:defcfun ("rmauxFlyMotionFunc" #.(openrm-lispify "rmauxFlyMotionFunc" 'function)) :int
  (p :pointer)
  (xbutton :int)
  (ybutton :int))

(cffi:defcfun ("rmauxArcBall" #.(openrm-lispify "rmauxArcBall" 'function)) :void
  (x1 float-pointer)
  (y1 float-pointer)
  (x2 float-pointer)
  (y2 float-pointer)
  (result :pointer))

(cffi:defcfun ("rmauxDolly" #.(openrm-lispify "rmauxDolly" 'function)) :void
  (toModify :pointer)
  (x1 float-pointer)
  (y1 float-pointer)
  (x2 float-pointer)
  (y2 float-pointer))

(cffi:defcfun ("rmauxTranslate" #.(openrm-lispify "rmauxTranslate" 'function)) :void
  (toModify :pointer)
  (x1 float-pointer)
  (y1 float-pointer)
  (x2 float-pointer)
  (y2 float-pointer))

(cl:defconstant #.(openrm-lispify "RMI_PPM_P2" 'constant) 2)

(cl:defconstant #.(openrm-lispify "RMI_PPM_P3" 'constant) 3)

(cl:defconstant #.(openrm-lispify "RMI_PPM_P5" 'constant) 5)

(cl:defconstant #.(openrm-lispify "RMI_PPM_P6" 'constant) 6)

(cl:defconstant #.(openrm-lispify "RMI_PPM_ASCII" 'constant) 0)

(cl:defconstant #.(openrm-lispify "RMI_PPM_BINARY" 'constant) 1)

(cffi:defcfun ("rmiReadPPM" #.(openrm-lispify "rmiReadPPM" 'function)) :pointer
  (filename :string)
  (format #.(openrm-lispify "RMenum" 'enumname))
  (alpha :unsigned-char))

(cffi:defcfun ("rmiWritePPM" #.(openrm-lispify "rmiWritePPM" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (filename :string)
  (type :int)
  (image :pointer))

(cffi:defcfun ("rmiReadJPEG" #.(openrm-lispify "rmiReadJPEG" 'function)) :pointer
  (filename :string))

(cffi:defcfun ("rmiWriteJPEG" #.(openrm-lispify "rmiWriteJPEG" 'function)) #.(openrm-lispify "RMenum" 'enumname)
  (filename :string)
  (quality :int)
  (image :pointer))


