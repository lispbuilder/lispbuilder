%insert("lisphead") 
%{
;;;; OpenRM CFFI lisp wrapper
;;;; Part of the CL-Gardeners project
;;;; http://wiki.alu.org/Application_Builder
;;;; (C)2006 Luke J Crook
;;;; See COPYING for license
;;;; 
;;;; This .i file has been tested with SDL version 1.2.11
;;;;
;;;; NOTE: Remove the defcenum RMenum from the generated CFFI bindings.
;;;; This is defined below.
;;;;
;;;; SWIG command line:
;;;; C:\swig>swig -cffi -I<path_to_includes> -Ilib -Ilib\cffi <path_to_openrmswig.i>

(in-package :lispbuilder-openrm-cffi)

;;; Equivalent to a C macro where 1 is shifted left n times
(defun 1<<(x) (ash 1 x))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'openrm-lispify)
(defun openrm-lispify (name flag &optional (package *package*))
  (labels ((find-sub (src lst)
	     (when (>= (length lst)
		       (length src))
	       (if (and (equal src (subseq lst 0 (length src)))
			(not (equal (nth (length src)
					 lst) #\_))
		        (not (null (nth (length src)
			     	        lst))))
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
		    ((constant variable) "+")
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


;;;; These defctypes are used by the CFFI translation functions
;;;; see the typemap definition below.
(defctype float-pointer :pointer)
(defctype rmnode-pointer :pointer)
(defctype rmprimitive-pointer :pointer)
(defctype rmcamera3d-pointer :pointer)
(defctype rmcamera2d-pointer :pointer)
(defctype rmvertex3d-pointer :pointer)
(defctype rmvertex2d-pointer :pointer)
(defctype rmimage-pointer :pointer)
(defctype rmmatrix-pointer :pointer)
(defctype rmlightmodel-pointer :pointer)
(defctype rmlight-pointer :pointer)
(defctype rmfog-pointer :pointer)
(defctype rmcolor3d-pointer :pointer)
(defctype rmcolor4d-pointer :pointer)
(defctype rmclipplane-pointer :pointer)
(defctype rmbitmap-pointer :pointer)

;; CFFI does not currently support translation functions for ENUMs. Because we want
;; the CFFI layer to automatically convert NIL->RM_FALSE, and T->RM_TRUE, we need to 
;; create an rm-enum ctype that contains the necessary translation functions to/from
;; RMenums.
(defctype rm-enum (:wrapper :int 
			    :to-c to-rm-enum
			    :from-c from-rm-enum))

;; See "rmtypes.h" below.

(cffi:defcenum rm-enum-wrapper ;;#.(openrm-lispify "RMenum 'enumname)
	(#.(openrm-lispify "RM_WHACKED" 'enumvalue :keyword) -1)
	(#.(openrm-lispify "RM_FALSE" 'enumvalue :keyword) 0)
	(#.(openrm-lispify "RM_TRUE" 'enumvalue :keyword) 1)
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


(cffi:defcstruct #.(openrm-lispify "RMmatrix" 'classname)
	(#.(openrm-lispify "m" 'slotname) :float :count 16))

(cffi:defcstruct _object_info
	(name :char :count 64)
	(posted :int)
	(rpass_vdims :int)
	(rpass_opacity :int)
	(channel :int)
	(pickEnable :int))



;; See "rmtypes.h" below.
(cffi:defcstruct RM-pipe
	(offscreen :int)
	(processing-Mode :int)
	(channel-format :int)
	(xwindow-width :int)
	(xwindow-height :int)
	(xflags :int)
	(opaque-3D-Enable :int)
	(transparent-3D-Enable :int)
	(opaque-2D-Enable :int)
	(init-Matrix-Stack :int)
	(frame-Number :int)
	(display-List-Enable-Bool :int)
	(context-Cache :pointer)
	(display-Lists :pointer)
	(mt-Control :pointer)
	(hdc :pointer)
	(hwnd :pointer)
	(h-RC :pointer)
	(my-Rank :int)
	(global-NPE :int)
	(target-Platform :int)
	(channel-render-func :pointer)
	(post-Render-Barrier-Func :pointer)
	(postrenderfunc :pointer)
	(postrender-depthbufferfunc :pointer)
	(swap-Buffers-Func :pointer)
	(shutdown-Func :pointer)
	(post-FB-Clear-Barrier-Func :pointer)
	(create-Context-Func :pointer)
	(target-Frame-Rate :int)
	(time-Per-Frame :pointer)
	(time-Per-Frame-MS :double)
	(last-Time-Start :pointer)
	(last-Render-Time :pointer)
	(time-Sync-Func :pointer)
	(caps :pointer)
	(fb-Clear-Node :pointer)
	(local-Mask-Stack :int :count 65)
	(local-Mask-Stack-Top :int))
%}

%module openrm

%feature("intern_function","openrm-lispify");
%typemap(cin) float* "float-pointer";
%typemap(cin) RMnode* "rmnode-pointer";
%typemap(cin) RMcamera3D* "rmcamera3d-pointer";
%typemap(cin) RMcamera2D* "rmcamera2d-pointer";
%typemap(cin) RMvertex3D* "rmvertex3d-pointer";
%typemap(cin) RMvertex2D* "rmvertex2d-pointer";
%typemap(cin) RMcolor3D* "rmcolor3d-pointer";
%typemap(cin) RMcolor4D* "rmcolor4d-pointer";
%typemap(cin) RMimage* "rmimage-pointer";
%typemap(cin) RMmatrix* "rmmatrix-pointer";
%typemap(cin) RMlightModel* "rmlightmodel-pointer";
%typemap(cin) RMlight* "rmlight-pointer";
%typemap(cin) RMfog* "rmfog-pointer";
%typemap(cin) RMclipPlane* "rmclipplane-pointer";
%typemap(cin) RMbitmap* "rmbitmap-pointer";

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
%ignore RMmatrix;		// Defines above as this is an array of floats.


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

// Need to fix OGLPRIMPARMLIST()
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
%ignore rmColor3DNew;			// Defined in Lisp above.
%ignore rmColor3DDelete; 		// Defined in Lisp above.
%ignore rmSwapBuffers;
%include "rmpublic.h"

// "rmdeflts.h"
%ignore RM_PS_PORTRAIT;			// This is an enum, not a #define. SWIG can't assign an enum to a define.
%ignore RM_SHADER_SMOOTH;		// This is an enum, not a #define. SWIG can't assign an enum to a define.
#define RM_PS_PORTRAIT		0x0100;     
#define RM_SHADER_SMOOTH  	0x0220;
%include "rmdeflts.h"

%include "rmps.h"

#define WINAPI
%ignore rmauxWndProc;
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