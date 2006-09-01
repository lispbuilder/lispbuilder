
;;;; OpenRM CFFI lisp wrapper
;;;; Version 1.6.0-2
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




;;;SWIG wrapper code starts here

(defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(progn ,@(loop for value in enums
                  for index = 0 then (1+ index)
                  when (listp value) do (setf index (second value)
                                              value (first value))
                  collect `(defconstant ,value ,index))))

;;;SWIG wrapper code ends here


(defconstant RM_VERSION_MAJOR 1)

(defconstant RM_VERSION_MINOR 5)

(defconstant RM_VERSION_REV 2)

(defconstant RM_PI 3.1415926535897931)

(defconstant RM_TWO_PI 6.2831853071795862)

(defconstant RM_SQRT2 1.4142135623730951)

(defconstant RM_SQRT1_2 0.7071067811865476)

(defconstant RM_MAX_STRING_LENGTH 64)

(defconstant RM_SINGLEBUFFERED 0)

(defconstant RM_DOUBLEBUFFERED 1)

(defconstant RM_MAX_MIPMAPS 16)

(defconstant RM_MAX_LIGHTS 8)

(defconstant RM_MAX_MULTITEXTURES 8)

(defconstant RM_FEEDBACK_MIN_BUFFER_SIZE 65536)

(defconstant RM_NUM_FONT_FACES 5)

(defconstant RM_FONT_SERIF 0)

(defconstant RM_FONT_SANS 1)

(defconstant RM_FONT_MONO 2)

(defconstant RM_FONT_SYMBOL 3)

(defconstant RM_FONT_DINGBATS 4)

(defconstant RM_NUM_FONT_SIZES 7)

(defconstant RM_FONT_XXS 0)

(defconstant RM_FONT_XS 1)

(defconstant RM_FONT_S 2)

(defconstant RM_FONT_M 3)

(defconstant RM_FONT_L 4)

(defconstant RM_FONT_XL 5)

(defconstant RM_FONT_XXL 6)

(defconstant RM_NUM_FONT_STYLES 4)

(defconstant RM_MAX_LINESTYLES 5)

(defconstant RM_MAX_LINEWEIGHTS 8)

(defconstant RM_SPHERES_8 1)

(defconstant RM_SPHERES_32 2)

(defconstant RM_SPHERES_128 3)

(defconstant RM_SPHERES_512 4)

(defconstant RM_CONES_4 4)

(defconstant RM_CONES_8 8)

(defconstant RM_CONES_12 12)

(defconstant RM_CONES_16 16)

(defconstant RM_CONES_32 32)

(defconstant RM_CONES_64 64)

(defconstant RM_CONES_128 128)

(defconstant RM_CYLINDERS_4 4)

(defconstant RM_CYLINDERS_8 8)

(defconstant RM_CYLINDERS_12 12)

(defconstant RM_CYLINDERS_16 16)

(defconstant RM_CYLINDERS_32 32)

(defconstant RM_CYLINDERS_64 64)

(defconstant RM_CYLINDERS_128 128)

(defconstant RM_OCTMESH_1 #x0001)

(defconstant RM_OCTMESH_2 #x0002)

(defconstant RM_OCTMESH_4 #x0004)

(defconstant RM_OCTMESH_8 #x0008)

(defconstant RM_OCTMESH_16 #x0010)

(defconstant RM_OCTMESH_DIVISOR_MASK #x001F)

(defconstant RM_OCTMESH_2DTEXTURES_MIN_MEMORY #x0040)

(defconstant RM_OCTMESH_2DTEXTURES_MAX_PERFORMANCE #x0080)

(defcfun ("rmMutexNew" rmMutexNew) :pointer
  (initLockState :int))

(defcfun ("rmMutexDelete" rmMutexDelete) :int
  (arg0 :pointer))

(defcfun ("rmMutexLock" rmMutexLock) :int
  (arg0 :pointer))

(defcfun ("rmMutexUnlock" rmMutexUnlock) :int
  (arg0 :pointer))

(defcfun ("rmMutexTryLock" rmMutexTryLock) :int
  (toQuery :pointer))

(defcfun ("rmThreadCreate" rmThreadCreate) :int
  (threadID :pointer)
  (threadFunc :pointer)
  (args :pointer))

(defcfun ("rmThreadJoin" rmThreadJoin) :int
  (threadID :pointer)
  (threadReturn :pointer))

(defconstant MAX_MASK_STACK_DEPTH 64)

(defconstant MAX_FRAME_SAMPLES 6)

(defcstruct HINSTANCE__
	(unused :int))

(defcstruct HWND__
	(unused :int))

(defcstruct HGLRC__
	(unused :int))

(defcstruct HDC__
	(unused :int))

(defconstant MAX_VIS_CMAP_ENTRIES 256)

(defconstant RMV_DEFAULT_MAP_SIZE 256)

(defconstant OBJECT_NAME_LENGTH 64)

(defcstruct RMcolor1D
	(lum :float))

(defcstruct RMcolor2D
	(lum :float)
	(alpha :float))

(defcstruct RMcolor3D
	(r :float)
	(g :float)
	(b :float))

(defcstruct RMcolor4D
	(r :float)
	(g :float)
	(b :float)
	(a :float))

(defcstruct RMvertex2D
	(x :float)
	(y :float))

(defcstruct RMvertex3D
	(x :float)
	(y :float)
	(z :float))

(defcstruct RMvertex4D
	(x :float)
	(y :float)
	(z :float)
	(w :float))

(defcstruct RMarray
	(nItems :int)
	(currentArraySize :int)
	(elementSize :int)
	(chunkSize :int)
	(data :pointer))

(defcstruct RMinternalMarker2D
	(npts :int)
	(gl_begin_flag :int)
	(vlist :pointer))

(defcstruct RMvisMap
	(nentries :int)
	(r :pointer)
	(g :pointer)
	(b :pointer)
	(a :pointer)
	(transfer_min :float)
	(transfer_max :float))

(defcstruct RMbitmap
	(w :int)
	(h :int)
	(bytes_per_scanline :int)
	(pbsize :int)
	(pixeldata :pointer))

(defcstruct RMimage
	(ndims :int)
	(w :int)
	(h :int)
	(d :int)
	(image_format :int)
	(image_type :int)
	(xzoom :float)
	(yzoom :float)
	(copy_flag :int)
	(pixeldata :pointer)
	(appfreefunc :pointer)
	(vismap :pointer)
	(scale :float)
	(bias :float)
	(bytes_per_component :int)
	(bytes_per_scanline :int)
	(elements :int)
	(pbsize :unsigned-int)
	(compListIndx :int)
	(cacheKey :unsigned-int))

(defcstruct RMcamera2D
	(xmin :float)
	(ymin :float)
	(xmax :float)
	(ymax :float)
	(aspect_ratio :float))

(defcstruct _RMStereoCameraControls
	(preframe_leftchannel_func :pointer)
	(preframe_rightchannel_func :pointer))

(defcstruct RMcamera3D
	(eye :pointer)
	(at :pointer)
	(up :pointer)
	(hither :float)
	(yon :float)
	(fov :float)
	(aspect :float)
	(projection :int)
	(isStereo :int)
	(degrees_eye_separation :float)
	(focalDistance :float))

(defcstruct RMtextProps
	(fontEnum :int)
	(italicEnum :int)
	(boldEnum :int)
	(sizeEnum :int)
	(hJustify :int)
	(vJustify :int)
	(listbase :int)
	(listoffset :int)
	(fontinfo :pointer)
	(compListIndx :int))

(defcstruct _bounding_box
	(min :pointer)
	(max :pointer))

(defcstruct RMclipPlane
	(enabled :int)
	(point :pointer)
	(normal :pointer)
	(a :float)
	(b :float)
	(c :float)
	(d :float))

(defcstruct RMlight
	(ltype :int)
	(ambientLightColor :pointer)
	(diffuseLightColor :pointer)
	(specularLightColor :pointer)
	(lightXYZ :pointer)
	(spotCutoff :float)
	(spotExponent :float)
	(spotDirection :pointer)
	(constantAttenuation :float)
	(linearAttenuation :float)
	(quadraticAttenuation :float)
	(enabled :int))

(defcstruct RMlightModel
	(globalAmbient :pointer)
	(twoSideEnable :unsigned-int)
	(localViewerEnable :unsigned-int))

(defcstruct RMtexture
	(images :pointer)
	(appTextureID :pointer)
	(dims :int)
	(nmipmaps :int)
	(mag_filter_mode :unsigned-int)
	(min_filter_mode :unsigned-int)
	(wrap_mode :unsigned-int)
	(oglTexelFormat :pointer)
	(envMode :unsigned-int)
	(blendColor :pointer)
	(borderWidth :int)
	(residency_status :int)
	(compListIndx :int)
	(cacheKeyID :unsigned-int)
	(cacheKeyData :unsigned-int)
	(refCount :int))

(defcstruct RMfog
	(fogMode :unsigned-int)
	(fogDensity :float)
	(fogStart :float)
	(fogEnd :float)
	(fogColor :pointer))

(defcstruct RMstate
	(model :pointer)
	(view :pointer)
	(modelView :pointer)
	(projection :pointer)
	(composite :pointer)
	(pick :pointer)
	(textureMatrix :pointer)
	(projection_inverse :pointer)
	(vpm :pointer)
	(vp :pointer)
	(aspect_ratio :float)
	(focal_length :float)
	(w :int)
	(h :int)
	(rendermode :int)
	(renderpass :int)
	(renderPassDims :int)
	(which_channel :int)
	(texture :pointer)
	(texture_mode :int)
	(attrib_stack_depth :int)
	(ambient :pointer)
	(diffuse :pointer)
	(specular :pointer)
	(emissive :pointer)
	(unlit_color :pointer)
	(specular_exponent :float)
	(opacity :float)
	(pointsize :float)
	(linewidth :int)
	(linestyle :int)
	(shademodel :int)
	(poly_mode_face :int)
	(poly_mode_drawstyle :int)
	(cull_mode :int)
	(front_face :int)
	(cp0 :pointer)
	(cp1 :pointer)
	(cp2 :pointer)
	(cp3 :pointer)
	(cp4 :pointer)
	(cp5 :pointer)
	(fogActive :int)
	(fog :pointer)
	(lmodel :pointer)
	(lightSources :pointer)
	(textProps :pointer)
	(frameNumber :int)
	(colorMaterialActive :int)
	(lightingActive :int))

(defcstruct RMstateCache
	(colorMaterialActive :int)
	(lightingActive :int)
	(texturingActive :int))

(defcstruct _surface_properties
	(ambient_color :pointer)
	(diffuse_color :pointer)
	(specular_color :pointer)
	(unlit_color :pointer)
	(specular_exponent :pointer)
	(opacity :pointer))

(defcstruct _rendermode_properties
	(shademodel :pointer)
	(poly_mode_face :pointer)
	(poly_mode_drawstyle :pointer)
	(cull_mode :pointer)
	(front_face :pointer)
	(pointsize :pointer)
	(linewidth :pointer)
	(linestyle :pointer)
	(normalizeNormals :pointer))

(defcstruct internals_RMsceneParms
	(viewport :pointer)
	(camera3d :pointer)
	(camera2d :pointer)
	(textures :pointer)
	(haveAnyTextures :int)
	(cp0 :pointer)
	(cp1 :pointer)
	(cp2 :pointer)
	(cp3 :pointer)
	(cp4 :pointer)
	(cp5 :pointer)
	(lmodel :pointer)
	(lightSources :pointer)
	(textProps :pointer)
	(fog :pointer))

(defcstruct internals_RMfbClear
	(bgColor :pointer)
	(bgImageTile :pointer)
	(depthValue :pointer)
	(depthImage :pointer))

(defcstruct internals_RMtransformationStruct
	(pre :pointer)
	(s :pointer)
	(r :pointer)
	(s2 :pointer)
	(translate :pointer)
	(post :pointer)
	(transform_mode :int))

(defcstruct RMnode
	(parent :pointer)
	(nchildren :int)
	(children :pointer)
	(nprims :int)
	(prims :pointer)
	(sprops :pointer)
	(rprops :pointer)
	(scene_parms :pointer)
	(fbClear :pointer)
	(bbox :pointer)
	(center :pointer)
	(transforms :pointer)
	(object_info :pointer)
	(viewPretraverseCallback :pointer)
	(viewPosttraverseCallback :pointer)
	(renderPretraverseCallback :pointer)
	(renderPosttraverseCallback :pointer)
	(viewSwitchCallback :pointer)
	(viewRenderOrderCallback :pointer)
	(clientData :pointer)
	(clientDataFreeFunc :pointer)
	(refcount :int)
	(attribMask :unsigned-int)
	(nodeMutex :pointer)
	(compListIndx :int))

(defcstruct RMfontRegistry
	(initialized :int)
	(refcount :int)
	(listbase :int)
	(listoffset :int)
	(listCount :int)
	(fontinfo :pointer))

(defcstruct RMcontextCache
	(primDisplayListIDs :pointer)
	(primCacheKeys :pointer)
	(numPrimDisplayListIDs :int)
	(numPrimCacheKeys :int)
	(imgDisplayListIDs :pointer)
	(imgCacheKeys :pointer)
	(numImgDisplayListIDs :int)
	(numImgCacheKeys :int)
	(textureIDs :pointer)
	(textureIDCacheKeys :pointer)
	(textureDataCacheKeys :pointer)
	(numTextureIDs :int)
	(numTextureIDCacheKeys :int)
	(numTextureDataCacheKeys :int)
	(sphereIDs :pointer)
	(coneIDs :pointer)
	(flipConeIDs :pointer)
	(cylinderIDs :pointer)
	(flipCylinderIDs :pointer)
	(pipeFontRegistry :pointer))

(defcstruct RMtime
	(sec :long)
	(usec :long))

(defcstruct RMpipeOGLCapabilities
	(haveMultiTexturing :int)
	(numTextureUnits :int)
	(activeTextureARB :pointer)
	(multiTexCoord1fvARB :pointer)
	(multiTexCoord2fvARB :pointer)
	(multiTexCoord3fvARB :pointer)
	(have3DTextures :int)
	(rm_glTexImage3D :pointer)
	(rm_glTexSubImage3D :pointer))

(defcstruct RMprimitiveDataBlob
	(ptr :pointer)
	(copyflag :int)
	(nthings :int)
	(blobtype :int)
	(stride :int)
	(veclen :int)
	(appfreefunc :pointer))

(defcstruct RMprimitive
	(type :int)
	(blobs :pointer)
	(multiTextureCoordBlobs :pointer)
	(multiTextureCoordBlobsMask :int)
	(numMultiTextureCoordBlobs :int)
	(renderfunc :pointer)
	(clientData :pointer)
	(clientDataFreeFunc :pointer)
	(p1 :pointer)
	(flags1 :unsigned-int)
	(model_flag :unsigned-int)
	(display_list_enable :int)
	(utility :int)
	(compListIndx :int)
	(cacheKey :unsigned-int)
	(utilCacheKey :unsigned-int)
	(bmin :pointer)
	(bmax :pointer)
	(primitiveComputeBoundingBoxFunc :pointer))

(defcstruct RMpick
	(node :pointer)
	(zval :float)
	(index :unsigned-int)
	(prim_index :unsigned-int))

(defcfun ("rmVismapNew" rmVismapNew) :pointer
  (size :int))

(defcfun ("rmVismapDup" rmVismapDup) :pointer
  (toDuplicate :pointer))

(defcfun ("rmVismapDelete" rmVismapDelete) :int
  (toDelete :pointer))

(defcfun ("rmVismapSetColor3D" rmVismapSetColor3D) :int
  (toModify :pointer)
  (indx :int)
  (newColor :pointer))

(defcfun ("rmVismapGetColor3D" rmVismapGetColor3D) :int
  (toQuery :pointer)
  (indx :int)
  (retColor :pointer))

(defcfun ("rmVismapSetColor4D" rmVismapSetColor4D) :int
  (toModify :pointer)
  (indx :int)
  (newColor :pointer))

(defcfun ("rmVismapGetColor4D" rmVismapGetColor4D) :int
  (toQuery :pointer)
  (indx :int)
  (retColor :pointer))

(defcfun ("rmVismapSetTfMin" rmVismapSetTfMin) :int
  (toModify :pointer)
  (newTFMin :float))

(defcfun ("rmVismapGetTfMin" rmVismapGetTfMin) :float
  (toQuery :pointer))

(defcfun ("rmVismapSetTfMax" rmVismapSetTfMax) :int
  (toModify :pointer)
  (newTFMax :float))

(defcfun ("rmVismapGetTfMax" rmVismapGetTfMax) :float
  (toQuery :pointer))

(defcfun ("rmVismapIndexFromData" rmVismapIndexFromData) :int
  (map :pointer)
  (val :float))

(defcfun ("rmVismapSetSize" rmVismapSetSize) :int
  (toModify :pointer)
  (newSize :int))

(defcfun ("rmVismapGetSize" rmVismapGetSize) :int
  (toQuery :pointer))

(defcfun ("rmVismapSetAlpha" rmVismapSetAlpha) :int
  (toModify :pointer)
  (indx :int)
  (newAlpha :float))

(defcfun ("rmVismapGetAlpha" rmVismapGetAlpha) :float
  (toQuery :pointer)
  (indx :int))

(defcfun ("rmDefaultVismap" rmDefaultVismap) :pointer)

(defcfun ("rmPipeSetWindow" rmPipeSetWindow) :int
  (p :pointer)
  (hWnd :pointer)
  (width :int)
  (height :int))

(defcfun ("rmPipeGetWindow" rmPipeGetWindow) :pointer
  (p :pointer))

(defcfun ("rmPipeSwapBuffersWin32" rmPipeSwapBuffersWin32) :int
  (p :pointer))

(defcfun ("rmwPipeCreateContext" rmwPipeCreateContext) :int
  (p :pointer))

(defcfun ("rmPipeGetContext" rmPipeGetContext) :pointer
  (p :pointer))

(defcfun ("rmPipeSetContext" rmPipeSetContext) :int
  (p :pointer)
  (theContext :pointer))

(defcfun ("rmPipeNew" rmPipeNew) :pointer
  (targetPlatform :int))

(defcfun ("rmPipeCreateContext" rmPipeCreateContext) :int
  (p :pointer))

(defcfun ("rmPipeDelete" rmPipeDelete) :int
  (toDelete :pointer))

(defcfun ("rmPipeMakeCurrent" rmPipeMakeCurrent) :int
  (pipe :pointer))

(defcfun ("rmPipeClose" rmPipeClose) :int
  (toClose :pointer))

(defcfun ("rmPipeSwapBuffers" rmPipeSwapBuffers) :int
  (p :pointer))

(defcfun ("rmPipeSetChannelFormat" rmPipeSetChannelFormat) :int
  (toModify :pointer)
  (newChannelFormat :int))

(defcfun ("rmPipeGetChannelFormat" rmPipeGetChannelFormat) :int
  (toQuery :pointer))

(defcfun ("rmPipeSetInitMatrixStackMode" rmPipeSetInitMatrixStackMode) :int
  (toModify :pointer)
  (newMode :int))

(defcfun ("rmPipeGetInitMatrixStackMode" rmPipeGetInitMatrixStackMode) :int
  (toQuery :pointer))

(defcfun ("rmPipeSetRenderPassEnable" rmPipeSetRenderPassEnable) :int
  (t_arg0 :pointer)
  (opaque3DEnable :int)
  (transparent3DEnable :int)
  (opaque2DEnable :int))

(defcfun ("rmPipeGetRenderPassEnable" rmPipeGetRenderPassEnable) :int
  (t_arg0 :pointer)
  (opaque3DEnableReturn :pointer)
  (transparent3DEnableReturn :pointer)
  (opaque2DEnableReturn :pointer))

(defcfun ("rmPipeSetWindowSize" rmPipeSetWindowSize) :int
  (toModify :pointer)
  (newWidth :int)
  (newHeight :int))

(defcfun ("rmPipeGetWindowSize" rmPipeGetWindowSize) :int
  (toQuery :pointer)
  (widthReturn :pointer)
  (heightReturn :pointer))

(defcfun ("rmPipeSetSwapBuffersFunc" rmPipeSetSwapBuffersFunc) :int
  (toModify :pointer)
  (newFunc :pointer))

(defcfun ("rmPipeSetPostRenderFunc" rmPipeSetPostRenderFunc) :int
  (toModify :pointer)
  (postRenderFunc :pointer))

(defcfun ("rmPipeSetPostRenderDepthFunc" rmPipeSetPostRenderDepthFunc) :int
  (toModify :pointer)
  (postRenderDepthFunc :pointer))

(defcfun ("rmPipeSetPostRenderBarrierFunc" rmPipeSetPostRenderBarrierFunc) :int
  (toModify :pointer)
  (barrierFunc :pointer))

(defcfun ("rmPipeGetFrameNumber" rmPipeGetFrameNumber) :int
  (toQuery :pointer))

(defcfun ("rmPipeGetProcessingMode" rmPipeGetProcessingMode) :int
  (toQuery :pointer))

(defcfun ("rmPipeSetProcessingMode" rmPipeSetProcessingMode) :int
  (toModify :pointer)
  (newMode :int))

(defcfun ("rmPipeProcessingModeIsMultithreaded" rmPipeProcessingModeIsMultithreaded) :int
  (toQuery :pointer))

(defcfun ("rmPipeSetCommSize" rmPipeSetCommSize) :int
  (toModify :pointer)
  (globalNPE :int))

(defcfun ("rmPipeGetCommSize" rmPipeGetCommSize) :int
  (toQuery :pointer))

(defcfun ("rmPipeSetRank" rmPipeSetRank) :int
  (toModify :pointer)
  (myRank :int))

(defcfun ("rmPipeGetRank" rmPipeGetRank) :int
  (toQuery :pointer))

(defcfun ("rmPipeGetDisplayListEnable" rmPipeGetDisplayListEnable) :int
  (toQuery :pointer))

(defcfun ("rmPipeSetDisplayListEnable" rmPipeSetDisplayListEnable) :int
  (toModify :pointer)
  (newMode :int))

(defcfun ("rmPipeSetFrameRate" rmPipeSetFrameRate) :int
  (toModify :pointer)
  (newFramesPerSecond :int))

(defcfun ("rmPipeGetFrameRate" rmPipeGetFrameRate) :int
  (toQuery :pointer))

(defcfun ("rmPipeGetNumMultitextureUnits" rmPipeGetNumMultitextureUnits) :int
  (toQuery :pointer))

(defcfun ("rmPipeSetSceneBackgroundColor" rmPipeSetSceneBackgroundColor) :int
  (toModify :pointer)
  (newColor :pointer))

(defcfun ("rmPipeGetSceneBackgroundColor" rmPipeGetSceneBackgroundColor) :int
  (toQuery :pointer)
  (returnColor :pointer))

(defcfun ("rmPipeSetSceneBackgroundImage" rmPipeSetSceneBackgroundImage) :int
  (toModify :pointer)
  (newImageTile :pointer))

(defcfun ("rmPipeGetSceneBackgroundImage" rmPipeGetSceneBackgroundImage) :int
  (toQuery :pointer)
  (returnImageTile :pointer))

(defcfun ("rmPipeSetSceneDepthImage" rmPipeSetSceneDepthImage) :int
  (toModify :pointer)
  (newDepthImage :pointer))

(defcfun ("rmPipeGetSceneDepthImage" rmPipeGetSceneDepthImage) :int
  (toQuery :pointer)
  (returnDepthImage :pointer))

(defcfun ("rmPipeSetSceneDepthValue" rmPipeSetSceneDepthValue) :int
  (toModify :pointer)
  (newDepthValue :pointer))

(defcfun ("rmPipeGetSceneDepthValue" rmPipeGetSceneDepthValue) :int
  (toQuery :pointer)
  (returnDepthValue :pointer))

(defcfun ("rmArrayAdd" rmArrayAdd) :int
  (toModify :pointer)
  (newData :pointer))

(defcfun ("rmArrayDelete" rmArrayDelete) :int
  (a :pointer))

(defcfun ("rmArrayGet" rmArrayGet) :pointer
  (toQuery :pointer)
  (indx :int))

(defcfun ("rmArrayNew" rmArrayNew) :pointer
  (initSize :int)
  (chunkSize :int)
  (elementSize :int))

(defcfun ("rmArrayNumItems" rmArrayNumItems) :int
  (toQuery :pointer))

(defcfun ("rmArraySort" rmArraySort) :void
  (arg0 :pointer)
  (compareFunc :pointer))

(defcfun ("rmBitmapNew" rmBitmapNew) :pointer
  (width :int)
  (height :int))

(defcfun ("rmBitmapCopy" rmBitmapCopy) :int
  (dst :pointer)
  (src :pointer))

(defcfun ("rmBitmapDup" rmBitmapDup) :pointer
  (src :pointer))

(defcfun ("rmBitmapDelete" rmBitmapDelete) :void
  (toDelete :pointer))

(defcfun ("rmBitmapSetPixelData" rmBitmapSetPixelData) :int
  (toModify :pointer)
  (pixeldata :pointer))

(defcfun ("rmBitmapGetPixelData" rmBitmapGetPixelData) :pointer
  (toQuery :pointer))

(defcfun ("rmBitmapGetSize" rmBitmapGetSize) :int
  (toQuery :pointer)
  (widthReturn :pointer)
  (heightReturn :pointer)
  (bytesWidthReturn :pointer))

(defcfun ("rmBitmapSetBit" rmBitmapSetBit) :int
  (toModify :pointer)
  (columnIndex :int)
  (rowIndex :int))

(defcfun ("rmCamera2DNew" rmCamera2DNew) :pointer)

(defcfun ("rmCamera2DCopy" rmCamera2DCopy) :int
  (dst :pointer)
  (src :pointer))

(defcfun ("rmCamera2DDelete" rmCamera2DDelete) :void
  (toDelete :pointer))

(defcfun ("rmDefaultCamera2D" rmDefaultCamera2D) :int
  (c :pointer))

(defcfun ("rmCamera2DSetAspectRatio" rmCamera2DSetAspectRatio) :int
  (toModify :pointer)
  (newAspect :float))

(defcfun ("rmCamera2DResetAspectRatio" rmCamera2DResetAspectRatio) :int
  (toModify :pointer)
  (vp :pointer)
  (windowWidth :int)
  (windowWeight :int))

(defcfun ("rmCamera2DGetAspectRatio" rmCamera2DGetAspectRatio) :int
  (toQuery :pointer)
  (retValue :pointer))

(defcfun ("rmCamera2DSetExtents" rmCamera2DSetExtents) :int
  (toModify :pointer)
  (xmin :float)
  (ymin :float)
  (xmax :float)
  (ymax :float))

(defcfun ("rmCamera2DGetExtents" rmCamera2DGetExtents) :int
  (toQuery :pointer)
  (xmin :pointer)
  (ymin :pointer)
  (xmax :pointer)
  (ymax :pointer))

(defcfun ("rmCamera2DComputeViewMatrix" rmCamera2DComputeViewMatrix) :int
  (c :pointer)
  (m :pointer))

(defcfun ("rmCamera2DComputeViewFromGeometry" rmCamera2DComputeViewFromGeometry) :int
  (toModify :pointer)
  (source :pointer))

(defcfun ("rmCamera3DNew" rmCamera3DNew) :pointer)

(defcfun ("rmCamera3DCopy" rmCamera3DCopy) :int
  (dst :pointer)
  (src :pointer))

(defcfun ("rmCamera3DDelete" rmCamera3DDelete) :void
  (toDelete :pointer))

(defcfun ("rmDefaultCamera3D" rmDefaultCamera3D) :int
  (c :pointer))

(defcfun ("rmCamera3DSetAspectRatio" rmCamera3DSetAspectRatio) :int
  (toModify :pointer)
  (newAspect :float))

(defcfun ("rmCamera3DResetAspectRatio" rmCamera3DResetAspectRatio) :int
  (toModify :pointer)
  (vp :pointer)
  (windowWidth :int)
  (windowHeight :int))

(defcfun ("rmCamera3DGetAspectRatio" rmCamera3DGetAspectRatio) :float
  (toQuery :pointer))

(defcfun ("rmCamera3DSetProjection" rmCamera3DSetProjection) :int
  (toQuery :pointer)
  (newVal :int))

(defcfun ("rmCamera3DGetProjection" rmCamera3DGetProjection) :int
  (toQuery :pointer))

(defcfun ("rmCamera3DSetEye" rmCamera3DSetEye) :int
  (toModify :pointer)
  (newEye :pointer))

(defcfun ("rmCamera3DGetEye" rmCamera3DGetEye) :int
  (toQuery :pointer)
  (returnEye :pointer))

(defcfun ("rmCamera3DSetAt" rmCamera3DSetAt) :int
  (toModify :pointer)
  (newAt :pointer))

(defcfun ("rmCamera3DGetAt" rmCamera3DGetAt) :int
  (toQuery :pointer)
  (returnAt :pointer))

(defcfun ("rmCamera3DSetUpVector" rmCamera3DSetUpVector) :int
  (toModify :pointer)
  (newUpVector :pointer))

(defcfun ("rmCamera3DGetUpVector" rmCamera3DGetUpVector) :int
  (toQuery :pointer)
  (returnUpVector :pointer))

(defcfun ("rmCamera3DSetFOV" rmCamera3DSetFOV) :int
  (toQuery :pointer)
  (newFOV :float))

(defcfun ("rmCamera3DGetFOV" rmCamera3DGetFOV) :float
  (toQuery :pointer))

(defcfun ("rmCamera3DSetHither" rmCamera3DSetHither) :int
  (toModify :pointer)
  (newHither :float))

(defcfun ("rmCamera3DGetHither" rmCamera3DGetHither) :float
  (toQuery :pointer))

(defcfun ("rmCamera3DSetYon" rmCamera3DSetYon) :int
  (toModify :pointer)
  (newYon :float))

(defcfun ("rmCamera3DGetYon" rmCamera3DGetYon) :float
  (toQuery :pointer))

(defcfun ("rmCamera3DSetStereo" rmCamera3DSetStereo) :int
  (toModify :pointer)
  (newBoolValue :int))

(defcfun ("rmCamera3DGetStereo" rmCamera3DGetStereo) :int
  (toQuery :pointer))

(defcfun ("rmCamera3DSetEyeSeparation" rmCamera3DSetEyeSeparation) :int
  (toModify :pointer)
  (newval :float))

(defcfun ("rmCamera3DGetEyeSeparation" rmCamera3DGetEyeSeparation) :float
  (toQuery :pointer))

(defcfun ("rmCamera3DSetFocalDistance" rmCamera3DSetFocalDistance) :int
  (toModify :pointer)
  (newval :float))

(defcfun ("rmCamera3DGetFocalDistance" rmCamera3DGetFocalDistance) :float
  (toQuery :pointer))

(defcfun ("rmCamera3DComputeViewMatrix" rmCamera3DComputeViewMatrix) :int
  (source :pointer)
  (viewReturn :pointer)
  (projectionReturn :pointer))

(defcfun ("rmCamera3DComputeViewFromGeometry" rmCamera3DComputeViewFromGeometry) :int
  (toModify :pointer)
  (source :pointer)
  (windowWidth :int)
  (windowHeight :int))

(defcfun ("rmClipPlaneNew" rmClipPlaneNew) :pointer)

(defcfun ("rmClipPlaneDelete" rmClipPlaneDelete) :int
  (toDelete :pointer))

(defcfun ("rmClipPlaneSetPointNormal" rmClipPlaneSetPointNormal) :int
  (toModify :pointer)
  (point :pointer)
  (normal :pointer))

(defcfun ("rmClipPlaneGetPointNormal" rmClipPlaneGetPointNormal) :int
  (toQuery :pointer)
  (point :pointer)
  (normal :pointer))

(defcfun ("rmClipPlaneEnable" rmClipPlaneEnable) :int
  (toModify :pointer))

(defcfun ("rmClipPlaneDisable" rmClipPlaneDisable) :int
  (toModify :pointer))

(defcfun ("rmClipPlaneIsEnabled" rmClipPlaneIsEnabled) :int
  (toQuery :pointer))

(defcfun ("rmComponentManagerPrintStatus" rmComponentManagerPrintStatus) :void)

(defcfun ("rmError" rmError) :void
  (msg :string))

(defcfun ("rmNotice" rmNotice) :void
  (msg :string))

(defcfun ("rmNotifyLevel" rmNotifyLevel) :int
  (level :int))

(defcfun ("rmSplash" rmSplash) :void
  (msg :string))

(defcfun ("rmWarning" rmWarning) :void
  (msg :string))

(defcfun ("rmGetEnum" rmGetEnum) :int
  (queryTag :int)
  (returnValue :pointer))

(defcfun ("rmSetEnum" rmSetEnum) :int
  (setTag :int)
  (newValue :int))

(defcfun ("rmFogNew" rmFogNew) :pointer)

(defcfun ("rmFogDup" rmFogDup) :pointer
  (toDuplicate :pointer))

(defcfun ("rmFogDelete" rmFogDelete) :int
  (toDelete :pointer))

(defcfun ("rmFogSetMode" rmFogSetMode) :int
  (toModify :pointer)
  (newMode :unsigned-int))

(defcfun ("rmFogGetMode" rmFogGetMode) :unsigned-int
  (toQuery :pointer))

(defcfun ("rmFogSetColor" rmFogSetColor) :int
  (toModify :pointer)
  (newColor :pointer))

(defcfun ("rmFogGetColor" rmFogGetColor) :int
  (toQuery :pointer)
  (returnColor :pointer))

(defcfun ("rmFogSetDensity" rmFogSetDensity) :int
  (toModify :pointer)
  (newDensity :float))

(defcfun ("rmFogGetDensity" rmFogGetDensity) :float
  (toQuery :pointer))

(defcfun ("rmFogSetStartEnd" rmFogSetStartEnd) :int
  (toModify :pointer)
  (newStart :float)
  (newEnd :float))

(defcfun ("rmFogGetStartEnd" rmFogGetStartEnd) :int
  (toModify :pointer)
  (startReturn :pointer)
  (endReturn :pointer))

(defcfun ("rmFrame" rmFrame) :void
  (drawToPipe :pointer)
  (rootedTree :pointer))

(defcfun ("rmImageNew" rmImageNew) :pointer
  (ndims :int)
  (width :int)
  (height :int)
  (depth :int)
  (formatEnum :int)
  (typeEnum :int)
  (copyFlag :int))

(defcfun ("rmImageDup" rmImageDup) :pointer
  (toDuplicate :pointer))

(defcfun ("rmImageDelete" rmImageDelete) :int
  (toDelete :pointer))

(defcfun ("rmImageSetPixelData" rmImageSetPixelData) :int
  (toModify :pointer)
  (pixelData :pointer)
  (copyEnum :int)
  (appFreeFunc :pointer))

(defcfun ("rmImageGetPixelData" rmImageGetPixelData) :pointer
  (toQuery :pointer))

(defcfun ("rmImageGetBytesPerScanline" rmImageGetBytesPerScanline) :unsigned-int
  (toQuery :pointer))

(defcfun ("rmImageGetCopyFlag" rmImageGetCopyFlag) :int
  (toQuery :pointer))

(defcfun ("rmImageGetType" rmImageGetType) :int
  (toQuery :pointer))

(defcfun ("rmImageGetFormat" rmImageGetFormat) :int
  (toQuery :pointer))

(defcfun ("rmImageGetImageSize" rmImageGetImageSize) :int
  (toQuery :pointer)
  (returnNDims :pointer)
  (returnWidth :pointer)
  (returnHeight :pointer)
  (returnDepth :pointer)
  (returnElements :pointer)
  (returnBytesPerScanline :pointer))

(defcfun ("rmImageSetPixelZoom" rmImageSetPixelZoom) :int
  (toModify :pointer)
  (xzoom :float)
  (yzoom :float))

(defcfun ("rmImageGetPixelZoom" rmImageGetPixelZoom) :int
  (toQuery :pointer)
  (returnXZoom :pointer)
  (returnYZoom :pointer))

(defcfun ("rmImageSetVismap" rmImageSetVismap) :int
  (toModify :pointer)
  (vismap :pointer))

(defcfun ("rmImageGetVismap" rmImageGetVismap) :int
  (toQuery :pointer)
  (vismapReturn :pointer))

(defcfun ("rmImageMirror" rmImageMirror) :int
  (toMirror :pointer)
  (mirrorAxisEnum :int))

(defcfun ("rmImageResize" rmImageResize) :int
  (src :pointer)
  (dst :pointer)
  (hardwareEnum :int)
  (p :pointer))

(defcfun ("rmImageSetScale" rmImageSetScale) :int
  (toModify :pointer)
  (newScale :float))

(defcfun ("rmImageGetScale" rmImageGetScale) :int
  (toQuery :pointer)
  (returnScale :pointer))

(defcfun ("rmImageSetBias" rmImageSetBias) :int
  (toModify :pointer)
  (newBias :float))

(defcfun ("rmImageGetBias" rmImageGetBias) :int
  (toQuery :pointer)
  (returnBias :pointer))

(defcfun ("rmLightNew" rmLightNew) :pointer)

(defcfun ("rmLightDelete" rmLightDelete) :void
  (toDelete :pointer))

(defcfun ("rmLightSetType" rmLightSetType) :int
  (toModify :pointer)
  (newType :int))

(defcfun ("rmLightGetType" rmLightGetType) :int
  (toQuery :pointer))

(defcfun ("rmLightSetXYZ" rmLightSetXYZ) :int
  (toModify :pointer)
  (newXYZ :pointer))

(defcfun ("rmLightGetXYZ" rmLightGetXYZ) :int
  (toQuery :pointer)
  (retXYZ :pointer))

(defcfun ("rmLightSetColor" rmLightSetColor) :int
  (toModify :pointer)
  (newAmbientColor :pointer)
  (newDiffuseColor :pointer)
  (newSpecularColor :pointer))

(defcfun ("rmLightGetColor" rmLightGetColor) :int
  (toQuery :pointer)
  (retAmbientColor :pointer)
  (retDiffuseColor :pointer)
  (retSpecularColor :pointer))

(defcfun ("rmLightSetAttenuation" rmLightSetAttenuation) :int
  (toModify :pointer)
  (newConstantAttenuation :float)
  (newLinearAttenuation :float)
  (newQuadraticAttenuation :float))

(defcfun ("rmLightGetAttenuation" rmLightGetAttenuation) :int
  (toQuery :pointer)
  (retConstantAttenuation :pointer)
  (retLinearAttenuation :pointer)
  (retQuadraticAttenuation :pointer))

(defcfun ("rmLightSetEnable" rmLightSetEnable) :int
  (toModify :pointer)
  (newValue :int))

(defcfun ("rmLightGetEnable" rmLightGetEnable) :int
  (toQuery :pointer))

(defcfun ("rmLightSetSpotDirection" rmLightSetSpotDirection) :int
  (toModify :pointer)
  (newSpotDirection :pointer))

(defcfun ("rmLightGetSpotDirection" rmLightGetSpotDirection) :int
  (toModify :pointer)
  (retDirection :pointer))

(defcfun ("rmLightSetSpotCutoff" rmLightSetSpotCutoff) :int
  (toModify :pointer)
  (newValue :float))

(defcfun ("rmLightGetSpotCutoff" rmLightGetSpotCutoff) :int
  (toQuery :pointer)
  (retValue :pointer))

(defcfun ("rmLightSetSpotExponent" rmLightSetSpotExponent) :int
  (toModify :pointer)
  (newValue :float))

(defcfun ("rmLightGetSpotExponent" rmLightGetSpotExponent) :int
  (toQuery :pointer)
  (retValue :pointer))

(defcfun ("rmLightModelNew" rmLightModelNew) :pointer)

(defcfun ("rmLightModelDelete" rmLightModelDelete) :int
  (toDelete :pointer))

(defcfun ("rmLightModelSetAmbient" rmLightModelSetAmbient) :int
  (toModify :pointer)
  (newAmbientColor :pointer))

(defcfun ("rmLightModelGetAmbient" rmLightModelGetAmbient) :int
  (toQuery :pointer)
  (retAmbientColor :pointer))

(defcfun ("rmLightModelSetTwoSided" rmLightModelSetTwoSided) :int
  (toModify :pointer)
  (newValue :int))

(defcfun ("rmLightModelGetTwoSided" rmLightModelGetTwoSided) :int
  (toQuery :pointer))

(defcfun ("rmLightModelSetLocalViewer" rmLightModelSetLocalViewer) :int
  (toModify :pointer)
  (newValue :int))

(defcfun ("rmLightModelGetLocalViewer" rmLightModelGetLocalViewer) :int
  (toQuery :pointer))

(defcfun ("rmDefaultLighting" rmDefaultLighting) :int
  (toModify :pointer))

(defcfun ("rmFloatNew" rmFloatNew) :pointer
  (num_floats :int))

(defcfun ("rmFloatDelete" rmFloatDelete) :void
  (f :pointer))

(defcfun ("rmVertex2DNew" rmVertex2DNew) :pointer
  (n :int))

(defcfun ("rmVertex2DDelete" rmVertex2DDelete) :void
  (arg0 :pointer))

(defcfun ("rmVertex3DNew" rmVertex3DNew) :pointer
  (n :int))

(defcfun ("rmVertex3DDelete" rmVertex3DDelete) :void
  (arg0 :pointer))

(defcfun ("rmColor4DNew" rmColor4DNew) :pointer
  (n :int))

(defcfun ("rmColor4DDelete" rmColor4DDelete) :void
  (to_delete :pointer))

(defcfun ("rmInternalMarker2DNew" rmInternalMarker2DNew) :pointer
  (nverts :int)
  (begin_flag :int)
  (dverts :pointer))

(defcfun ("rmInternalMarker2DDelete" rmInternalMarker2DDelete) :void
  (t_arg0 :pointer))

(defcfun ("rmMalloc2DByteBuffer" rmMalloc2DByteBuffer) :pointer
  (width :int)
  (height :int))

(defcfun ("rmFree2DByteBuffer" rmFree2DByteBuffer) :void
  (c :pointer))

(defcfun ("rmMalloc2DFloatBuffer" rmMalloc2DFloatBuffer) :pointer
  (width :int)
  (height :int))

(defcfun ("rmFree2DFloatBuffer" rmFree2DFloatBuffer) :void
  (f :pointer))

(defcfun ("rmMalloc3DByteBuffer" rmMalloc3DByteBuffer) :pointer
  (width :int)
  (height :int)
  (depth :int))

(defcfun ("rmFree3DByteBuffer" rmFree3DByteBuffer) :void
  (c :pointer))

(defcfun ("rmMalloc3DFloatBuffer" rmMalloc3DFloatBuffer) :pointer
  (width :int)
  (height :int)
  (depth :int))

(defcfun ("rmFree3DFloatBuffer" rmFree3DFloatBuffer) :void
  (c :pointer))

(defcfun ("rmMatrixNew" rmMatrixNew) :pointer)

(defcfun ("rmMatrixCopy" rmMatrixCopy) :int
  (dst :pointer)
  (src :pointer))

(defcfun ("rmMatrixDelete" rmMatrixDelete) :int
  (toDelete :pointer))

(defcfun ("rmMatrixIdentity" rmMatrixIdentity) :int
  (toModify :pointer))

(defcfun ("rmMatrixSetValue" rmMatrixSetValue) :int
  (toModify :pointer)
  (row :int)
  (col :int)
  (newValue :float))

(defcfun ("rmMatrixGetValue" rmMatrixGetValue) :float
  (toQuery :pointer)
  (row :int)
  (col :int))

(defcfun ("rmMatrixTranspose" rmMatrixTranspose) :int
  (src :pointer)
  (dst :pointer))

(defcfun ("rmMatrixMultiply" rmMatrixMultiply) :int
  (srcA :pointer)
  (srcB :pointer)
  (dst :pointer))

(defcfun ("rmMatrixInverse" rmMatrixInverse) :int
  (src :pointer)
  (dst :pointer))

(defcfun ("rmPoint4MatrixTransform" rmPoint4MatrixTransform) :int
  (src :pointer)
  (matrix :pointer)
  (dst :pointer))

(defcfun ("rmPointMatrixTransform" rmPointMatrixTransform) :int
  (src :pointer)
  (m :pointer)
  (dst :pointer))

(defcfun ("rmPrintMatrix" rmPrintMatrix) :int
  (toPrint :pointer))

(defcfun ("rmPointMin" rmPointMin) :int
  (input :pointer)
  (count :int)
  (vdims :int)
  (stride :int)
  (minReturn :pointer))

(defcfun ("rmPointMax" rmPointMax) :int
  (input :pointer)
  (count :int)
  (vdims :int)
  (stride :int)
  (maxReturn :pointer))

(defcfun ("rmPointMinMax" rmPointMinMax) :int
  (input :pointer)
  (count :int)
  (vdims :int)
  (stride :int)
  (minReturn :pointer)
  (maxReturn :pointer))

(defcfun ("rmVertex3DMag" rmVertex3DMag) :double
  (v :pointer))

(defcfun ("rmVertex3DSum" rmVertex3DSum) :int
  (a :pointer)
  (b :pointer)
  (dst :pointer))

(defcfun ("rmVertex3DDiff" rmVertex3DDiff) :int
  (a :pointer)
  (b :pointer)
  (dst :pointer))

(defcfun ("rmVertex3DDot" rmVertex3DDot) :double
  (a :pointer)
  (b :pointer))

(defcfun ("rmVertex3DCross" rmVertex3DCross) :int
  (p :pointer)
  (r :pointer)
  (result :pointer))

(defcfun ("rmVertex3DNormalize" rmVertex3DNormalize) :int
  (toNormalize :pointer))

(defcfun ("rmVertex3DMagNormalize" rmVertex3DMagNormalize) :int
  (toNormalize :pointer)
  (magReturn :pointer))

(defcfun ("rmVertex3DMidpoint" rmVertex3DMidpoint) :int
  (a :pointer)
  (b :pointer)
  (dst :pointer))

(defcfun ("rmDCFromWC3" rmDCFromWC3) :int
  (src :pointer)
  (dst :pointer)
  (nPoints :int)
  (cam3d :pointer)
  (model :pointer)
  (viewPort :pointer)
  (windowWidth :int)
  (windowHeight :int))

(defcfun ("rmDCFromWC2" rmDCFromWC2) :int
  (src :pointer)
  (dst :pointer)
  (nPoints :int)
  (cam3d :pointer)
  (model :pointer)
  (viewPort :pointer)
  (windowWidth :int)
  (windowHeight :int))

(defcfun ("rmRootNode" rmRootNode) :pointer)

(defcfun ("rmNodeNew" rmNodeNew) :pointer
  (name :string)
  (renderpassVdims :int)
  (renderpassOpaque :int))

(defcfun ("rmNodeDelete" rmNodeDelete) :int
  (toDelete :pointer))

(defcfun ("rmSubTreeDelete" rmSubTreeDelete) :int
  (toDelete :pointer))

(defcfun ("rmNodeSetName" rmNodeSetName) :int
  (toModify :pointer)
  (name :string))

(defcfun ("rmNodeGetName" rmNodeGetName) :string
  (toQuery :pointer))

(defcfun ("rmNodeSetPickEnable" rmNodeSetPickEnable) :int
  (toModify :pointer)
  (newVal :int))

(defcfun ("rmNodeSetTraverseEnable" rmNodeSetTraverseEnable) :int
  (toModify :pointer)
  (newval :int))

(defcfun ("rmNodeGetTraverseEnable" rmNodeGetTraverseEnable) :int
  (toQuery :pointer))

(defcfun ("rmNodeGetPickEnable" rmNodeGetPickEnable) :int
  (toQuery :pointer))

(defcfun ("rmNodeAddChild" rmNodeAddChild) :int
  (parent :pointer)
  (child :pointer))

(defcfun ("rmNodeRemoveChild" rmNodeRemoveChild) :int
  (parent :pointer)
  (child :pointer))

(defcfun ("rmNodeRemoveAllChildren" rmNodeRemoveAllChildren) :int
  (toModify :pointer))

(defcfun ("rmNodeRemoveAllPrims" rmNodeRemoveAllPrims) :int
  (toModify :pointer))

(defcfun ("rmNodeGetNumChildren" rmNodeGetNumChildren) :int
  (toQuery :pointer))

(defcfun ("rmNodeGetNumPrims" rmNodeGetNumPrims) :int
  (toQuery :pointer))

(defcfun ("rmNodeGetIthChild" rmNodeGetIthChild) :pointer
  (toQuery :pointer)
  (indx :int))

(defcfun ("rmFindNamedNode" rmFindNamedNode) :pointer
  (start :pointer)
  (name :string))

(defcfun ("rmSceneGraphWalk" rmSceneGraphWalk) :void
  (p :pointer)
  (r :pointer)
  (userfunc :pointer)
  (clientData :pointer))

(defcfun ("rmPrintSceneGraph" rmPrintSceneGraph) :void
  (root :pointer)
  (printMode :int)
  (fileName :string))

(defcfun ("rmNodeMutexInit" rmNodeMutexInit) :int
  (toModify :pointer)
  (lockStatus :int))

(defcfun ("rmNodeMutexLock" rmNodeMutexLock) :int
  (toModify :pointer))

(defcfun ("rmNodeMutexUnlock" rmNodeMutexUnlock) :int
  (toModify :pointer))

(defcfun ("rmNodeMutexTryLock" rmNodeMutexTryLock) :int
  (toQuery :pointer))

(defcfun ("rmNodeGetMutex" rmNodeGetMutex) :pointer
  (toQuery :pointer))

(defcfun ("rmNodeMutexDelete" rmNodeMutexDelete) :int
  (toModify :pointer))

(defcfun ("rmNodeSetBoundingBox" rmNodeSetBoundingBox) :int
  (toModify :pointer)
  (vMin :pointer)
  (vMax :pointer))

(defcfun ("rmNodeGetBoundingBox" rmNodeGetBoundingBox) :int
  (toQuery :pointer)
  (vMinReturn :pointer)
  (vMaxReturn :pointer))

(defcfun ("rmNodeComputeCenterFromBoundingBox" rmNodeComputeCenterFromBoundingBox) :int
  (toModify :pointer))

(defcfun ("rmNodeComputeBoundingBox" rmNodeComputeBoundingBox) :int
  (toModify :pointer))

(defcfun ("rmNodeUnionAllBoxes" rmNodeUnionAllBoxes) :int
  (toModify :pointer))

(defcfun ("rmNodeSetAmbientColor" rmNodeSetAmbientColor) :int
  (toModify :pointer)
  (newColor :pointer))

(defcfun ("rmNodeGetAmbientColor" rmNodeGetAmbientColor) :int
  (toQuery :pointer)
  (ambientReturn :pointer))

(defcfun ("rmNodeSetDiffuseColor" rmNodeSetDiffuseColor) :int
  (toModify :pointer)
  (newColor :pointer))

(defcfun ("rmNodeGetDiffuseColor" rmNodeGetDiffuseColor) :int
  (toQuery :pointer)
  (diffuseReturn :pointer))

(defcfun ("rmNodeSetSpecularColor" rmNodeSetSpecularColor) :int
  (toModify :pointer)
  (newColor :pointer))

(defcfun ("rmNodeGetSpecularColor" rmNodeGetSpecularColor) :int
  (toQuery :pointer)
  (diffuseReturn :pointer))

(defcfun ("rmNodeSetSpecularExponent" rmNodeSetSpecularExponent) :int
  (toModify :pointer)
  (newValue :float))

(defcfun ("rmNodeGetSpecularExponent" rmNodeGetSpecularExponent) :int
  (toQuery :pointer)
  (retValue :pointer))

(defcfun ("rmNodeSetUnlitColor" rmNodeSetUnlitColor) :int
  (toModify :pointer)
  (newColor :pointer))

(defcfun ("rmNodeGetUnlitColor" rmNodeGetUnlitColor) :int
  (toQuery :pointer)
  (retColor :pointer))

(defcfun ("rmNodeSetOpacity" rmNodeSetOpacity) :int
  (toModify :pointer)
  (newValue :float))

(defcfun ("rmNodeGetOpacity" rmNodeGetOpacity) :int
  (toQuery :pointer)
  (retValue :pointer))

(defcfun ("rmNodeSetNormalizeNormals" rmNodeSetNormalizeNormals) :int
  (toModify :pointer)
  (newValue :int))

(defcfun ("rmNodeGetNormalizeNormals" rmNodeGetNormalizeNormals) :int
  (toQuery :pointer)
  (retValue :pointer))

(defcfun ("rmNodeSetLineStyle" rmNodeSetLineStyle) :int
  (toModify :pointer)
  (newStyle :int))

(defcfun ("rmNodeGetLineStyle" rmNodeGetLineStyle) :int
  (toQuery :pointer)
  (retStyle :pointer))

(defcfun ("rmNodeSetLineWidth" rmNodeSetLineWidth) :int
  (toModify :pointer)
  (newWidthEnum :int))

(defcfun ("rmNodeGetLineWidth" rmNodeGetLineWidth) :int
  (toQuery :pointer)
  (retWidthEnum :pointer))

(defcfun ("rmNodeSetPointSize" rmNodeSetPointSize) :int
  (toModify :pointer)
  (newsize :float))

(defcfun ("rmNodeGetPointSize" rmNodeGetPointSize) :int
  (toQuery :pointer)
  (sizeReturn :pointer))

(defcfun ("rmNodeSetShader" rmNodeSetShader) :int
  (toModify :pointer)
  (newShaderEnum :int))

(defcfun ("rmNodeGetShader" rmNodeGetShader) :int
  (toQuery :pointer)
  (retShaderEnum :pointer))

(defcfun ("rmNodeSetPolygonDrawMode" rmNodeSetPolygonDrawMode) :int
  (toModify :pointer)
  (whichFace :int)
  (newMode :int))

(defcfun ("rmNodeGetPolygonDrawMode" rmNodeGetPolygonDrawMode) :int
  (toQuery :pointer)
  (returnFace :pointer)
  (returnMode :pointer))

(defcfun ("rmNodeSetPolygonCullMode" rmNodeSetPolygonCullMode) :int
  (toModify :pointer)
  (newMode :int))

(defcfun ("rmNodeGetPolygonCullMode" rmNodeGetPolygonCullMode) :int
  (toQuery :pointer)
  (modeReturn :pointer))

(defcfun ("rmNodeSetFrontFace" rmNodeSetFrontFace) :int
  (toModify :pointer)
  (newMode :int))

(defcfun ("rmNodeGetFrontFace" rmNodeGetFrontFace) :int
  (toQuery :pointer)
  (modeReturn :pointer))

(defcfun ("rmNodeSetTraversalMaskOpacity" rmNodeSetTraversalMaskOpacity) :int
  (toModify :pointer)
  (opacityTraversalMask :int))

(defcfun ("rmNodeGetTraversalMaskOpacity" rmNodeGetTraversalMaskOpacity) :int
  (toQuery :pointer)
  (opacityTraversalMaskReturn :pointer))

(defcfun ("rmNodeSetTraversalMaskDims" rmNodeSetTraversalMaskDims) :int
  (n :pointer)
  (dimsTraversalMask :int))

(defcfun ("rmNodeGetTraversalMaskDims" rmNodeGetTraversalMaskDims) :int
  (n :pointer)
  (dimsTraversalMaskReturn :pointer))

(defcfun ("rmNodeSetTraversalMaskChannel" rmNodeSetTraversalMaskChannel) :int
  (toModify :pointer)
  (newval :int))

(defcfun ("rmNodeGetTraversalMaskChannel" rmNodeGetTraversalMaskChannel) :int
  (toQuery :pointer)
  (channelTraversalMaskReturn :pointer))

(defcfun ("rmNodeSetTransformMode" rmNodeSetTransformMode) :int
  (toModify :pointer)
  (newMode :int))

(defcfun ("rmNodeGetTransformMode" rmNodeGetTransformMode) :int
  (toQuery :pointer))

(defcfun ("rmNodeSetPreMatrix" rmNodeSetPreMatrix) :int
  (toModify :pointer)
  (newMatrix :pointer))

(defcfun ("rmNodeGetPreMatrix" rmNodeGetPreMatrix) :int
  (toQuery :pointer)
  (matrixReturn :pointer))

(defcfun ("rmNodeSetCenter" rmNodeSetCenter) :int
  (toModify :pointer)
  (newVertex :pointer))

(defcfun ("rmNodeGetCenter" rmNodeGetCenter) :int
  (toQuery :pointer)
  (retVector :pointer))

(defcfun ("rmNodeSetScaleMatrix" rmNodeSetScaleMatrix) :int
  (toModify :pointer)
  (newMatrix :pointer))

(defcfun ("rmNodeGetScaleMatrix" rmNodeGetScaleMatrix) :int
  (toQuery :pointer)
  (matrixReturn :pointer))

(defcfun ("rmNodeSetRotateMatrix" rmNodeSetRotateMatrix) :int
  (toModify :pointer)
  (newMatrix :pointer))

(defcfun ("rmNodeGetRotateMatrix" rmNodeGetRotateMatrix) :int
  (toQuery :pointer)
  (matrixReturn :pointer))

(defcfun ("rmNodeSetPostRotateScaleMatrix" rmNodeSetPostRotateScaleMatrix) :int
  (toModify :pointer)
  (newMatrix :pointer))

(defcfun ("rmNodeGetPostRotateScaleMatrix" rmNodeGetPostRotateScaleMatrix) :int
  (toQuery :pointer)
  (matrixReturn :pointer))

(defcfun ("rmNodeSetTranslateVector" rmNodeSetTranslateVector) :int
  (toModify :pointer)
  (newVector :pointer))

(defcfun ("rmNodeGetTranslateVector" rmNodeGetTranslateVector) :int
  (toQuery :pointer)
  (returnVector :pointer))

(defcfun ("rmNodeSetPostMatrix" rmNodeSetPostMatrix) :int
  (toModify :pointer)
  (newMatrix :pointer))

(defcfun ("rmNodeGetPostMatrix" rmNodeGetPostMatrix) :int
  (toQuery :pointer)
  (matrixReturn :pointer))

(defcfun ("rmNodeGetCompositeModelMatrix" rmNodeGetCompositeModelMatrix) :int
  (toQuery :pointer)
  (transformReturn :pointer))

(defcfun ("rmNodeSetSwitchCallback" rmNodeSetSwitchCallback) :int
  (toModify :pointer)
  (appFunc :pointer))

(defcfun ("rmNodeSetPreTraversalCallback" rmNodeSetPreTraversalCallback) :int
  (toModify :pointer)
  (whichPass :int)
  (appFunc :pointer))

(defcfun ("rmNodeSetPostTraversalCallback" rmNodeSetPostTraversalCallback) :int
  (toModify :pointer)
  (whichPass :int)
  (appFunc :pointer))

(defcfun ("rmNodeSetRenderOrderCallback" rmNodeSetRenderOrderCallback) :int
  (toModify :pointer)
  (appFunc :pointer))

(defcfun ("rmNodeFrustumCullCallback" rmNodeFrustumCullCallback) :int
  (n :pointer)
  (s :pointer))

(defcfun ("rmInit" rmInit) :void)

(defcfun ("rmFinish" rmFinish) :void)

(defcfun ("rmFramePick" rmFramePick) :pointer
  (renderPipe :pointer)
  (subTree :pointer)
  (xpick :int)
  (ypick :int))

(defcfun ("rmFramePickList" rmFramePickList) :int
  (renderPipe :pointer)
  (subTree :pointer)
  (xpick :int)
  (ypick :int)
  (listReturn :pointer))

(defcfun ("rmPickDelete" rmPickDelete) :int
  (toDelete :pointer))

(defcfun ("rmPickedNode" rmPickedNode) :pointer
  (toQuery :pointer))

(defcfun ("rmPickedPrimitive" rmPickedPrimitive) :int
  (toQuery :pointer))

(defcfun ("rmPickedNodeName" rmPickedNodeName) :string
  (toQuery :pointer))

(defcfun ("rmPickedPrimitiveZval" rmPickedPrimitiveZval) :float
  (toQuery :pointer))

(defcfun ("rmPrimitiveNew" rmPrimitiveNew) :pointer
  (primType :int))

(defcfun ("rmPrimitiveDelete" rmPrimitiveDelete) :void
  (toDelete :pointer))

(defcfun ("rmPrimitiveSetRenderFunc" rmPrimitiveSetRenderFunc) :int
  (toModify :pointer)
  (drawFunc :pointer))

(defcfun ("rmPrimitiveGetRenderFunc" rmPrimitiveGetRenderFunc) :pointer
  (toQuery :pointer))

(defcfun ("rmPrimitiveSetModelFlag" rmPrimitiveSetModelFlag) :int
  (toModify :pointer)
  (newVal :int))

(defcfun ("rmPrimitiveGetModelFlag" rmPrimitiveGetModelFlag) :int
  (toQuery :pointer))

(defcfun ("rmPrimitiveSetBoundingBox" rmPrimitiveSetBoundingBox) :int
  (p :pointer)
  (bmin :pointer)
  (bmax :pointer))

(defcfun ("rmPrimitiveGetBoundingBox" rmPrimitiveGetBoundingBox) :int
  (p :pointer)
  (bmin :pointer)
  (bmax :pointer))

(defcfun ("rmPrimitiveComputeBoundingBox" rmPrimitiveComputeBoundingBox) :int
  (p :pointer))

(defcfun ("rmPrimitiveGetType" rmPrimitiveGetType) :int
  (toQuery :pointer))

(defcfun ("rmPrimitiveSetClientData" rmPrimitiveSetClientData) :int
  (toModify :pointer)
  (clientData :pointer)
  (cdFreeFunc :pointer))

(defcfun ("rmPrimitiveGetClientData" rmPrimitiveGetClientData) :pointer
  (toQuery :pointer))

(defcfun ("rmPrimitiveSetAppDisplayList" rmPrimitiveSetAppDisplayList) :int
  (toModify :pointer)
  (displayListID :unsigned-int))

(defcfun ("rmPrimitiveSetVertex2D" rmPrimitiveSetVertex2D) :int
  (toModify :pointer)
  (nVertices :int)
  (vertexData :pointer)
  (copyEnum :int)
  (appFreeFunc :pointer))

(defcfun ("rmPrimitiveSetVertex3D" rmPrimitiveSetVertex3D) :int
  (toModify :pointer)
  (nVertices :int)
  (vertexData :pointer)
  (copyEnum :int)
  (appFreeFunc :pointer))

(defcfun ("rmPrimitiveSetColor3D" rmPrimitiveSetColor3D) :int
  (toModify :pointer)
  (nColors :int)
  (colorData :pointer)
  (copyEnum :int)
  (appFreeFunc :pointer))

(defcfun ("rmPrimitiveSetColor4D" rmPrimitiveSetColor4D) :int
  (toModify :pointer)
  (nColors :int)
  (colorData :pointer)
  (copyEnum :int)
  (appFreeFunc :pointer))

(defcfun ("rmPrimitiveSetRadii" rmPrimitiveSetRadii) :int
  (toModify :pointer)
  (nRadii :int)
  (radii :pointer)
  (copyEnum :int)
  (freeFunc :pointer))

(defcfun ("rmPrimitiveSetNormal3D" rmPrimitiveSetNormal3D) :int
  (toModify :pointer)
  (nNormals :int)
  (normalsData :pointer)
  (copyEnum :int)
  (freeFunc :pointer))

(defcfun ("rmPrimitiveSetTexcoord1D" rmPrimitiveSetTexcoord1D) :int
  (toModify :pointer)
  (nTexCoords :int)
  (texCoordData :pointer)
  (copyEnum :int)
  (appFreeFunc :pointer))

(defcfun ("rmPrimitiveSetTexcoord2D" rmPrimitiveSetTexcoord2D) :int
  (toModify :pointer)
  (nTexCoords :int)
  (texCoordData :pointer)
  (copyEnum :int)
  (appFreeFunc :pointer))

(defcfun ("rmPrimitiveSetTexcoord3D" rmPrimitiveSetTexcoord3D) :int
  (toModify :pointer)
  (nTexcoords :int)
  (texCoordData :pointer)
  (copyEnum :int)
  (appFreeFunc :pointer))

(defcfun ("rmPrimitiveSetMultiTexcoord1D" rmPrimitiveSetMultiTexcoord1D) :int
  (toModify :pointer)
  (nTexCoords :int)
  (texCoordData :pointer)
  (copyEnum :int)
  (appFreeFunc :pointer)
  (textureUnitIndx :int))

(defcfun ("rmPrimitiveSetMultiTexcoord2D" rmPrimitiveSetMultiTexcoord2D) :int
  (toModify :pointer)
  (nTexCoords :int)
  (texCoordData :pointer)
  (copyEnum :int)
  (appFreeFunc :pointer)
  (textureUnitIndx :int))

(defcfun ("rmPrimitiveSetMultiTexcoord3D" rmPrimitiveSetMultiTexcoord3D) :int
  (toModify :pointer)
  (nTexcoords :int)
  (texCoordData :pointer)
  (copyEnum :int)
  (appFreeFunc :pointer)
  (textureUnitIndx :int))

(defcfun ("rmPrimitiveSetIndices" rmPrimitiveSetIndices) :int
  (toModify :pointer)
  (numIndices :int)
  (indicesArray :pointer)
  (copyEnum :int)
  (appFreeFunc :pointer))

(defcfun ("rmPrimitiveSetSprites" rmPrimitiveSetSprites) :int
  (toModify :pointer)
  (nSprites :int)
  (spriteArray :pointer))

(defcfun ("rmPrimitiveSetBitmaps" rmPrimitiveSetBitmaps) :int
  (toModify :pointer)
  (nBitmaps :int)
  (bmapArray :pointer))

(defcfun ("rmPrimitiveSetQmeshDims" rmPrimitiveSetQmeshDims) :int
  (toModify :pointer)
  (uSize :int)
  (vSize :int))

(defcfun ("rmPrimitiveSetOmeshDims" rmPrimitiveSetOmeshDims) :int
  (toModify :pointer)
  (isize :int)
  (jsize :int)
  (ksize :int))

(defcfun ("rmPrimitiveSetOmeshMinMaxGrid" rmPrimitiveSetOmeshMinMaxGrid) :int
  (toModify :pointer)
  (gridMin :pointer)
  (gridMax :pointer))

(defcfun ("rmPrimitiveSetMarkerScale" rmPrimitiveSetMarkerScale) :int
  (toModify :pointer)
  (npts :int)
  (scales :pointer)
  (copyEnum :int)
  (appFreeFunc :pointer))

(defcfun ("rmPrimitiveSetMarkerPrims" rmPrimitiveSetMarkerPrims) :int
  (toModify :pointer)
  (nMarkerPrims :int)
  (mArray :pointer))

(defcfun ("rmPrimitiveSetEllipse2DRotate" rmPrimitiveSetEllipse2DRotate) :int
  (toModify :pointer)
  (nVals :int)
  (rotationValues :pointer)
  (copyEnum :int)
  (appFreeFunc :pointer))

(defcfun ("rmPrimitiveSetText" rmPrimitiveSetText) :int
  (toModify :pointer)
  (nStrings :int)
  (strings :pointer))

(defcfun ("rmPrimitiveSetDisplayListEnable" rmPrimitiveSetDisplayListEnable) :int
  (toModify :pointer)
  (newMode :int))

(defcfun ("rmNodeAddPrimitive" rmNodeAddPrimitive) :int
  (addTo :pointer)
  (src :pointer))

(defcfun ("rmNodeGetPrimitive" rmNodeGetPrimitive) :pointer
  (toQuery :pointer)
  (indx :int))

(defcfun ("rmNodeSetClientData" rmNodeSetClientData) :int
  (toModify :pointer)
  (clientData :pointer)
  (cdFreeFunc :pointer))

(defcfun ("rmNodeGetClientData" rmNodeGetClientData) :pointer
  (toQuery :pointer))

(defcfun ("rmStateNew" rmStateNew) :pointer)

(defcfun ("rmStateDelete" rmStateDelete) :void
  (toDelete :pointer))

(defcfun ("rmStateCopy" rmStateCopy) :void
  (src :pointer)
  (dst :pointer))

(defcfun ("rmStateGetModelViewMatrix" rmStateGetModelViewMatrix) :pointer
  (toQuery :pointer))

(defcfun ("rmStateGetProjectionMatrix" rmStateGetProjectionMatrix) :pointer
  (toQuery :pointer))

(defcfun ("rmStateGetShader" rmStateGetShader) :int
  (toQuery :pointer))

(defcfun ("rmStateGetPolygonDrawMode" rmStateGetPolygonDrawMode) :int
  (toQuery :pointer)
  (whichFaceReturn :pointer)
  (drawModeReturn :pointer))

(defcfun ("rmStateGetPolygonCullMode" rmStateGetPolygonCullMode) :int
  (toQuery :pointer)
  (cullModeReturn :pointer))

(defcfun ("rmStateGetFrontFace" rmStateGetFrontFace) :int
  (toQuery :pointer)
  (frontFaceReturn :pointer))

(defcfun ("rmStateGetLineWidth" rmStateGetLineWidth) :int
  (toQuery :pointer)
  (lineWidthReturn :pointer))

(defcfun ("rmStateGetLineStyle" rmStateGetLineStyle) :int
  (toQuery :pointer)
  (lineStyleReturn :pointer))

(defcfun ("rmStateGetPointSize" rmStateGetPointSize) :int
  (toQuery :pointer)
  (sizeReturn :pointer))

(defcfun ("rmStateGetFrameNumber" rmStateGetFrameNumber) :int
  (toQuery :pointer))

(defcfun ("rmNodeSetSceneBackgroundColor" rmNodeSetSceneBackgroundColor) :int
  (toModify :pointer)
  (newColor :pointer))

(defcfun ("rmNodeGetSceneBackgroundColor" rmNodeGetSceneBackgroundColor) :int
  (toQuery :pointer)
  (returnColor :pointer))

(defcfun ("rmNodeSetSceneBackgroundImage" rmNodeSetSceneBackgroundImage) :int
  (toModify :pointer)
  (newImageTile :pointer))

(defcfun ("rmNodeGetSceneBackgroundImage" rmNodeGetSceneBackgroundImage) :int
  (toQuery :pointer)
  (returnImageTile :pointer))

(defcfun ("rmNodeSetSceneCamera2D" rmNodeSetSceneCamera2D) :int
  (toModify :pointer)
  (newCamera :pointer))

(defcfun ("rmNodeGetSceneCamera2D" rmNodeGetSceneCamera2D) :int
  (toQuery :pointer)
  (returnCamera :pointer))

(defcfun ("rmNodeSetSceneCamera3D" rmNodeSetSceneCamera3D) :int
  (toModify :pointer)
  (newCamera :pointer))

(defcfun ("rmNodeGetSceneCamera3D" rmNodeGetSceneCamera3D) :int
  (toQuery :pointer)
  (returnCamera :pointer))

(defcfun ("rmNodeSetSceneClipPlane" rmNodeSetSceneClipPlane) :int
  (toModify :pointer)
  (clipPlaneEnum :int)
  (newClipPlane :pointer))

(defcfun ("rmNodeGetSceneClipPlane" rmNodeGetSceneClipPlane) :int
  (toQuery :pointer)
  (clipPlaneEnum :int)
  (returnClipPlaneCopy :pointer))

(defcfun ("rmNodeSetSceneDepthImage" rmNodeSetSceneDepthImage) :int
  (toModify :pointer)
  (newDepthImage :pointer))

(defcfun ("rmNodeGetSceneDepthImage" rmNodeGetSceneDepthImage) :int
  (toQuery :pointer)
  (returnDepthImage :pointer))

(defcfun ("rmNodeSetSceneDepthValue" rmNodeSetSceneDepthValue) :int
  (toModify :pointer)
  (newDepthValue :pointer))

(defcfun ("rmNodeGetSceneDepthValue" rmNodeGetSceneDepthValue) :int
  (toQuery :pointer)
  (returnDepthValue :pointer))

(defcfun ("rmNodeSetSceneFog" rmNodeSetSceneFog) :int
  (toModify :pointer)
  (newFog :pointer))

(defcfun ("rmNodeGetSceneFog" rmNodeGetSceneFog) :int
  (toQuery :pointer)
  (returnFog :pointer))

(defcfun ("rmNodeSetSceneLight" rmNodeSetSceneLight) :int
  (toModify :pointer)
  (whichLightEnum :int)
  (newLight :pointer))

(defcfun ("rmNodeGetSceneLight" rmNodeGetSceneLight) :int
  (toQuery :pointer)
  (whichLightEnum :int)
  (returnLightCopy :pointer))

(defcfun ("rmNodeSetSceneLightModel" rmNodeSetSceneLightModel) :int
  (toQuery :pointer)
  (newLightModel :pointer))

(defcfun ("rmNodeGetSceneLightModel" rmNodeGetSceneLightModel) :int
  (toQuery :pointer)
  (retLightModel :pointer))

(defcfun ("rmNodeGetSceneMultiTexture" rmNodeGetSceneMultiTexture) :int
  (n :pointer)
  (textureUnitIndx :int)
  (t_arg2 :pointer))

(defcfun ("rmNodeSetSceneMultiTexture" rmNodeSetSceneMultiTexture) :int
  (n :pointer)
  (t_arg1 :pointer)
  (textureUnit :int))

(defcfun ("rmTextPropsNew" rmTextPropsNew) :pointer)

(defcfun ("rmTextPropsDelete" rmTextPropsDelete) :int
  (t_arg0 :pointer))

(defcfun ("rmNodeSetSceneTextProps" rmNodeSetSceneTextProps) :int
  (toModify :pointer)
  (newTextProps :pointer))

(defcfun ("rmNodeGetSceneTextProps" rmNodeGetSceneTextProps) :int
  (toQuery :pointer)
  (returnTextProps :pointer))

(defcfun ("rmNodeSetSceneTexture" rmNodeSetSceneTexture) :int
  (toModify :pointer)
  (newTexture :pointer))

(defcfun ("rmNodeGetSceneTexture" rmNodeGetSceneTexture) :int
  (toQuery :pointer)
  (returnTexture :pointer))

(defcfun ("rmNodeSetSceneViewport" rmNodeSetSceneViewport) :int
  (toModify :pointer)
  (newViewport :pointer))

(defcfun ("rmNodeGetSceneViewport" rmNodeGetSceneViewport) :int
  (toQuery :pointer)
  (returnViewport :pointer))

(defcfun ("rmStatsComputeDemography" rmStatsComputeDemography) :int
  (r :pointer))

(defcfun ("rmStatsPrint" rmStatsPrint) :int)

(defcfun ("rmStatsEndTime" rmStatsEndTime) :int)

(defcfun ("rmStatsStartTime" rmStatsStartTime) :int)

(defcfun ("rmTextPropsSetAttribs" rmTextPropsSetAttribs) :int
  (toModify :pointer)
  (fontEnum :int)
  (sizeEnum :int)
  (boldEnum :int)
  (italicEnum :int)
  (hJustifyEnum :int)
  (vJustifyEnum :int))

(defcfun ("rmTextPropsGetAttribs" rmTextPropsGetAttribs) :int
  (toQuery :pointer)
  (fontEnumReturn :pointer)
  (sizeEnumReturn :pointer)
  (boldEnumReturn :pointer)
  (italicEnumReturn :pointer)
  (hJustifyReturn :pointer)
  (vJustifyReturn :pointer))

(defcfun ("rmTextGetExtents" rmTextGetExtents) :int
  (string :string)
  (fontEnum :int)
  (sizeEnum :int)
  (boldEnum :int)
  (italicEnum :int)
  (widthReturn :pointer)
  (heightReturn :pointer)
  (renderPipe :pointer))

(defcfun ("rmTimeNew" rmTimeNew) :pointer)

(defcfun ("rmTimeDelete" rmTimeDelete) :int
  (toDelete :pointer))

(defcfun ("rmTimeCurrent" rmTimeCurrent) :int
  (returnVal :pointer))

(defcfun ("rmTimeDifference" rmTimeDifference) :int
  (startTime :pointer)
  (endTime :pointer)
  (diffTime :pointer))

(defcfun ("rmTimeDifferenceMS" rmTimeDifferenceMS) :double
  (startTime :pointer)
  (endTime :pointer))

(defcfun ("rmTimeSet" rmTimeSet) :int
  (toModify :pointer)
  (secs :long)
  (usecs :long))

(defcfun ("rmTimeGet" rmTimeGet) :int
  (toQuery :pointer)
  (returnSecs :pointer)
  (returnUSecs :pointer))

(defcfun ("rmTimeSleep" rmTimeSleep) :int
  (tSleep :pointer))

(defcfun ("rmTimeEncodeMS" rmTimeEncodeMS) :int
  (result :pointer)
  (ms :double))

(defcfun ("rmTimeDecodeMS" rmTimeDecodeMS) :int
  (src :pointer)
  (resultMS :pointer))

(defcfun ("rmTextureNew" rmTextureNew) :pointer
  (ntdims :int))

(defcfun ("rmTextureDelete" rmTextureDelete) :int
  (toDelete :pointer)
  (deleteImagesBool :int))

(defcfun ("rmTextureSetImages" rmTextureSetImages) :int
  (toModify :pointer)
  (imageArray :pointer)
  (nimages :int)
  (borderWidth :int))

(defcfun ("rmTextureGetImages" rmTextureGetImages) :int
  (toQuery :pointer)
  (imageArray :pointer)
  (nimages :pointer)
  (borderWidth :pointer))

(defcfun ("rmTextureSetFilterMode" rmTextureSetFilterMode) :int
  (toModify :pointer)
  (minMode :unsigned-int)
  (magMode :unsigned-int))

(defcfun ("rmTextureGetFilterMode" rmTextureGetFilterMode) :int
  (toQuery :pointer)
  (minModeReturn :pointer)
  (magModeReturn :pointer))

(defcfun ("rmTextureSetWrapMode" rmTextureSetWrapMode) :int
  (toModify :pointer)
  (wrapMode :unsigned-int))

(defcfun ("rmTextureGetWrapMode" rmTextureGetWrapMode) :int
  (toQuery :pointer)
  (wrapModeReturn :pointer))

(defcfun ("rmTextureSetEnv" rmTextureSetEnv) :int
  (toModify :pointer)
  (envMode :unsigned-int)
  (blendColor :pointer))

(defcfun ("rmTextureGetEnv" rmTextureGetEnv) :int
  (toQuery :pointer)
  (envModeReturn :pointer)
  (blendColorReturn :pointer))

(defcfun ("rmTextureSetGLTexelFormat" rmTextureSetGLTexelFormat) :int
  (toModify :pointer)
  (internalTexelFormat :unsigned-int))

(defcfun ("rmTextureGetGLTexelFormat" rmTextureGetGLTexelFormat) :int
  (toQuery :pointer)
  (returnTexelFormat :pointer))

(defcfun ("rmTextureGetTextureID" rmTextureGetTextureID) :int
  (toQuery :pointer)
  (returnTextureID :pointer))

(defcfun ("rmTextureSetTextureID" rmTextureSetTextureID) :int
  (toModify :pointer)
  (textureID :pointer))

(defcfun ("rmNearestPowerOfTwo" rmNearestPowerOfTwo) :int
  (n :int))

(defcfun ("rmIntMagnitude" rmIntMagnitude) :int
  (m :int))

(defcfun ("rmHSVtoRGB" rmHSVtoRGB) :void
  (hue :float)
  (saturation :float)
  (value :float)
  (redReturn :pointer)
  (greenReturn :pointer)
  (blueReturn :pointer))

(defcfun ("rmRGBtoHSV" rmRGBtoHSV) :void
  (red :float)
  (green :float)
  (blue :float)
  (hueReturn :pointer)
  (saturationReturn :pointer)
  (valueReturn :pointer))

(defcfun ("rmUnionBoundingBoxes" rmUnionBoundingBoxes) :int
  (s1min :pointer)
  (s1max :pointer)
  (s2min :pointer)
  (s2max :pointer)
  (dmin :pointer)
  (dmax :pointer))

(defcfun ("rmGLGetError" rmGLGetError) :int
  (string :string))

(defcfun ("rmImageBuildMipmaps" rmImageBuildMipmaps) :int
  (src :pointer)
  (mapsReturn :pointer)
  (hardwareEnum :int)
  (hwPipe :pointer))

(defconstant RM_PS_DEFAULT_PAGE_WIDTH_POINTS 612)

(defconstant RM_PS_DEFAULT_PAGE_HEIGHT_POINTS 792)

(defconstant RM_PS_DEFAULT_ORIENTATION #x0100)

(defconstant RM_PS_DEFAULT_MIN_MARGIN 36)

(defconstant RM_DEFAULT_RENDERMODE #x0220)

(defconstant RM_DEFAULT_VIEWPORT_XMIN 0.0)

(defconstant RM_DEFAULT_VIEWPORT_YMIN 0.0)

(defconstant RM_DEFAULT_VIEWPORT_XMAX 1.0)

(defconstant RM_DEFAULT_VIEWPORT_YMAX 1.0)

(defcstruct RMpsSpec
	(psWidthPoints :int)
	(psHeightPoints :int)
	(psMinMargin :int)
	(psOrientation :int)
	(psRasterOrVector :int)
	(psOutputFormat :int)
	(psSortMethod :int)
	(fName :string)
	(fHandle :pointer))

(defcfun ("rmFramePS" rmFramePS) :int
  (drawOn :pointer)
  (subTree :pointer)
  (controlStruct :pointer))

(defcfun ("rmFramePSHeartbeat" rmFramePSHeartbeat) :int
  (drawOn :pointer)
  (subTree :pointer)
  (p :pointer)
  (heartBeatFunc :pointer))

(defcfun ("rmPSSpecNew" rmPSSpecNew) :pointer)

(defcfun ("rmPSSpecDelete" rmPSSpecDelete) :int
  (toDelete :pointer))

(defcfun ("rmPSPageSetSize" rmPSPageSetSize) :int
  (p :pointer)
  (pagePointsWidth :int)
  (pagePointsHeight :int))

(defcfun ("rmPSPageGetSize" rmPSPageGetSize) :int
  (p :pointer)
  (pagePointsWidth :pointer)
  (pagePointsHeight :pointer))

(defcfun ("rmPSPageSetOrientation" rmPSPageSetOrientation) :int
  (p :pointer)
  (orientation :int))

(defcfun ("rmPSPageGetOrientation" rmPSPageGetOrientation) :int
  (p :pointer))

(defcfun ("rmPSPageSetOutputFormat" rmPSPageSetOutputFormat) :int
  (p :pointer)
  (outputFormat :int))

(defcfun ("rmPSPageGetOutputFormat" rmPSPageGetOutputFormat) :int
  (p :pointer))

(defcfun ("rmPSPageSetVectorFormat" rmPSPageSetVectorFormat) :int
  (p :pointer)
  (trueOrFalse :int))

(defcfun ("rmPSPageGetVectorFormat" rmPSPageGetVectorFormat) :int
  (p :pointer))

(defcfun ("rmPSPageSetMargin" rmPSPageSetMargin) :int
  (p :pointer)
  (minMarginPoints :int))

(defcfun ("rmPSPageGetMargin" rmPSPageGetMargin) :int
  (p :pointer)
  (minMarginPointsReturn :pointer))

(defcfun ("rmPSSetOutputFilename" rmPSSetOutputFilename) :int
  (p :pointer)
  (fName :string))

(defcfun ("rmPSGetOutputFilename" rmPSGetOutputFilename) :string
  (p :pointer))

(defcfun ("rmPSSetSortMethod" rmPSSetSortMethod) :int
  (p :pointer)
  (sortMethod :int))

(defcfun ("rmPSGetSortMethod" rmPSGetSortMethod) :int
  (p :pointer))

(defconstant RM_NUM_BUTTONS 5)

(defconstant RM_BUTTON1 #x1)

(defconstant RM_BUTTON2 #x2)

(defconstant RM_BUTTON3 #x3)

(defconstant RM_BUTTON4 #x4)

(defconstant RM_BUTTON5 #x5)

(defconstant Button1 #x1)

(defconstant Button2 #x2)

(defconstant Button3 #x3)

(defconstant Button4 #x4)

(defconstant Button5 #x5)

(defconstant RM_NUM_BUTTON_MODIFIERS 3)

(defconstant RM_NONE_MODMASK 0)

(defconstant RM_NO_MODIFIER 0)

(defconstant RM_SHIFT_MODIFIER 1)

(defconstant RM_CONTROL_MODIFIER 2)

(defconstant RM_SHIFT_MODMASK 1)

(defconstant RM_CONTROL_MODMASK 2)

(defcfun ("rmauxEventLoop" rmauxEventLoop) :void
  (drawToPipe :pointer)
  (subTreeToDraw :pointer)
  (msg :pointer))

(defcfun ("rmauxSetGeomTransform" rmauxSetGeomTransform) :void
  (target :pointer)
  (pipe :pointer))

(defcfun ("rmauxSetSpinEnable" rmauxSetSpinEnable) :void
  (spinEnableBool :int))

(defcfun ("rmauxGetSpinEnable" rmauxGetSpinEnable) :int)

(defcfun ("rmauxSetCamera3DTransform" rmauxSetCamera3DTransform) :void
  (target :pointer)
  (pipe :pointer))

(defcfun ("rmauxUI" rmauxUI) :void
  (target :pointer)
  (pipe :pointer))

(defcfun ("rmauxSetInitFunc" rmauxSetInitFunc) :void
  (userinitfunc :pointer))

(defcfun ("rmauxSetIdleFunc" rmauxSetIdleFunc) :void
  (arg0 :pointer)
  (userfunc :pointer))

(defcfun ("rmauxSetKeyFunc" rmauxSetKeyFunc) :void
  (arg0 :pointer)
  (userfunc :pointer))

(defcfun ("rmauxDefaultKeyFunc" rmauxDefaultKeyFunc) :int
  (currentPipe :pointer)
  (key :char)
  (code :int))

(defcfun ("rmauxSetResizeFunc" rmauxSetResizeFunc) :void
  (arg0 :pointer)
  (cameraNode :pointer)
  (userfunc :pointer))

(defcfun ("rmauxDefaultResizeFunc" rmauxDefaultResizeFunc) :int
  (currentPipe :pointer)
  (cameraNode :pointer)
  (pointerX :int)
  (pointerY :int))

(defcfun ("rmauxSetRenderFunc" rmauxSetRenderFunc) :void
  (userfunc :pointer))

(defcfun ("rmauxSetButtonDownFunc" rmauxSetButtonDownFunc) :void
  (buttonnum :unsigned-int)
  (modmask :unsigned-int)
  (userfunc :pointer))

(defcfun ("rmauxSetButtonUpFunc" rmauxSetButtonUpFunc) :void
  (whichbutton :unsigned-int)
  (modmask :unsigned-int)
  (userfunc :pointer))

(defcfun ("rmauxSetButtonMotionFunc" rmauxSetButtonMotionFunc) :void
  (whichbutton :unsigned-int)
  (modmask :unsigned-int)
  (userfunc :pointer))

(defcfun ("rmauxCreateW32Window" rmauxCreateW32Window) :pointer
  (useMe :pointer)
  (parent :pointer)
  (xpos :int)
  (ypos :int)
  (width :int)
  (height :int)
  (wtitle :string)
  (hInstance :pointer)
  (event_loop_fptr :pointer))

(defcfun ("rmauxCreateOffscreenDrawable" rmauxCreateOffscreenDrawable) :pointer
  (pipe :pointer)
  (width :int)
  (height :int)
  (depth :int)
  (hCurrentInst :pointer)
  (eventLoopFuncPtr :pointer))

(defcfun ("rmauxFlyUI" rmauxFlyUI) :void
  (cameraNode :pointer)
  (drawNode :pointer)
  (pipe :pointer)
  (orientationScale :float)
  (translateScale :float))

(defcfun ("rmauxFlyResetCamera" rmauxFlyResetCamera) :int
  (p :pointer)
  (xbutton :int)
  (ybutton :int))

(defcfun ("rmauxFlyToggleMotion" rmauxFlyToggleMotion) :int
  (p :pointer)
  (xbutton :int)
  (ybutton :int))

(defcfun ("rmauxFlyMotionFunc" rmauxFlyMotionFunc) :int
  (p :pointer)
  (xbutton :int)
  (ybutton :int))

(defcfun ("rmauxArcBall" rmauxArcBall) :void
  (x1 :pointer)
  (y1 :pointer)
  (x2 :pointer)
  (y2 :pointer)
  (result :pointer))

(defcfun ("rmauxDolly" rmauxDolly) :void
  (toModify :pointer)
  (x1 :pointer)
  (y1 :pointer)
  (x2 :pointer)
  (y2 :pointer))

(defcfun ("rmauxTranslate" rmauxTranslate) :void
  (toModify :pointer)
  (x1 :pointer)
  (y1 :pointer)
  (x2 :pointer)
  (y2 :pointer))


