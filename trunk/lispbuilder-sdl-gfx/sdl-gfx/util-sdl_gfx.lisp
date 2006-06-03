;; SDL_gfx v2.0.13 library. Uses CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>, Justin Heyes-Jones <justinhj@gmail.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL_gfx from Common lisp
;; using sdl_gfx.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl-gfx)
  
;;; d

(defun draw-pixel (surface point color)
  (if (= 3 (length color))
      (pixelcolor surface (sdl::point-x point) (sdl::point-y point)
		  (map-color color))
      (pixelrgba surface (sdl::point-x point) (sdl::point-y point)
		 (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-hline (surface x1 x2 y color)
  (if (= 3 (length color))
      (hlinecolor surface x1 x2 y
		  (map-color color))
      (hlineRGBA surface x1 x2 y
		 (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-vline (surface x y1 y2 color)
  (if (= 3 (length color))
      (vlinecolor surface x y1 y2
		  (map-color color))
      (vlineRGBA surface x y1 y2
		 (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-rectangle (surface rect color)
  (if (= 3 (length color))
      (rectanglecolor surface (sdl::rect-x rect) (sdl::rect-y rect) (sdl::rect-x2 rect) (sdl::rect-y2 rect)
		      (map-color color))
      (rectangleRGBA surface (sdl::rect-x rect) (sdl::rect-y rect) (sdl::rect-x2 rect) (sdl::rect-y2 rect)
		     (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-box (surface rect color)
  (if (= 3 (length color))
      (boxcolor surface (sdl::rect-x rect) (sdl::rect-y rect) (sdl::rect-x2 rect) (sdl::rect-y2 rect)
		(map-color color))
      (boxRGBA surface (sdl::rect-x rect) (sdl::rect-y rect) (sdl::rect-x2 rect) (sdl::rect-y2 rect)
	       (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-line (surface point1 point2 color)
  (if (= 3 (length color))
      (linecolor surface (sdl::point-x point1) (sdl::point-y point1) (sdl::point-x point2) (sdl::point-y point2)
		 (map-color color))
      (lineRGBA surface (sdl::point-x point1) (sdl::point-y point1) (sdl::point-x point2) (sdl::point-y point2)
		(sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-aaline (surface point1 point2 color)
  (if (= 3 (length color))
      (aalinecolor surface (sdl::point-x point1) (sdl::point-y point1) (sdl::point-x point2) (sdl::point-y point2)
		   (map-color color))
      (aalineRGBA surface (sdl::point-x point1) (sdl::point-y point1) (sdl::point-x point2) (sdl::point-y point2)
		  (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-circle (surface point r color)
  (if (= 3 (length color))
      (circlecolor surface (sdl::point-x point) (sdl::point-y point) r
		   (map-color color))
      (circleRGBA surface (sdl::point-x point) (sdl::point-y point) r
		  (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-aacircle (surface point r color)
  (if (= 3 (length color))
      (aacirclecolor surface (sdl::point-x point) (sdl::point-y point) r
		     (map-color color))
      (aacircleRGBA surface (sdl::point-x point) (sdl::point-y point) r
		    (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-filledcircle (surface point r color)
  (if (= 3 (length color))
      (filledcirclecolor surface (sdl::point-x point) (sdl::point-y point) r
		     (map-color color))
      (filledcircleRGBA surface (sdl::point-x point) (sdl::point-y point) r
		    (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-ellipse (surface point rx ry color)
  (if (= 3 (length color))
      (ellipsecolor surface (sdl::point-x point) (sdl::point-y point) rx ry
		    (map-color color))
      (ellipseRGBA surface (sdl::point-x point) (sdl::point-y point) rx ry
		   (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-aaellipse (surface point rx ry color)
  (if (= 3 (length color))
      (aaellipsecolor surface (sdl::point-x point) (sdl::point-y point) rx ry
		      (map-color color))
      (aaellipseRGBA surface (sdl::point-x point) (sdl::point-y point) rx ry
		     (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-filledellipse (surface point rx ry color)
  (if (= 3 (length color))
      (filledellipsecolor surface (sdl::point-x point) (sdl::point-y point) rx ry
			  (map-color color))
      (filledellipseRGBA surface (sdl::point-x point) (sdl::point-y point) rx ry
			 (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-pie (surface point rad start end color)
  (if (= 3 (length color))
      (piecolor surface (sdl::point-x point) (sdl::point-y point) rad start end
		(map-color color))
      (pieRGBA surface (sdl::point-x point) (sdl::point-y point) rad start end
	       (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-filledpie (surface point rad start end color)
  (if (= 3 (length color))
      (filledpiecolor surface (sdl::point-x point) (sdl::point-y point) rad start end
		      (map-color color))
      (filledpieRGBA surface (sdl::point-x point) (sdl::point-y point) rad start end
		     (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-trigon (surface point1 point2 point3 color)
  (if (= 3 (length color))
      (trigoncolor surface (sdl::point-x point1) (sdl::point-y point1)
		   (sdl::point-x point2) (sdl::point-y point2)
		   (sdl::point-x point3) (sdl::point-y point3)
		   (map-color color))
      (trigonRGBA surface (sdl::point-x point1) (sdl::point-y point1)
		   (sdl::point-x point2) (sdl::point-y point2)
		   (sdl::point-x point3) (sdl::point-y point3)
		  (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-aatrigon (surface point1 point2 point3 color)
  (if (= 3 (length color))
      (aatrigoncolor surface (sdl::point-x point1) (sdl::point-y point1)
		     (sdl::point-x point2) (sdl::point-y point2)
		     (sdl::point-x point3) (sdl::point-y point3)
		     (map-color color))
      (aatrigonRGBA surface (sdl::point-x point1) (sdl::point-y point1)
		    (sdl::point-x point2) (sdl::point-y point2)
		    (sdl::point-x point3) (sdl::point-y point3)
		    (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-filledtrigon (surface point1 point2 point3 color)
  (if (= 3 (length color))
      (filledtrigoncolor surface (sdl::point-x point1) (sdl::point-y point1)
			 (sdl::point-x point2) (sdl::point-y point2)
			 (sdl::point-x point3) (sdl::point-y point3)
			 (map-color color))
      (filledtrigonRGBA surface (sdl::point-x point1) (sdl::point-y point1)
			(sdl::point-x point2) (sdl::point-y point2)
			(sdl::point-x point3) (sdl::point-y point3)
			(sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-polygon (surface point n color)
  (if (= 3 (length color))
      (polygoncolor surface (sdl::point-x point) (sdl::point-y point) n
		    (map-color color))
      (polygonRGBA surface (sdl::point-x point) (sdl::point-y point) n
		   (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-aapolygon (surface point n color)
  (if (= 3 (length color))
      (aapolygoncolor surface (sdl::point-x point) (sdl::point-y point) n
		      (map-color color))
      (aapolygonRGBA surface (sdl::point-x point) (sdl::point-y point) n
		     (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-filledpolygon (surface point n color)
  (if (= 3 (length color))
      (filledpolygoncolor surface (sdl::point-x point) (sdl::point-y point) n
			  (map-color color))
      (filledpolygonRGBA surface (sdl::point-x point) (sdl::point-y point) n
			 (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-bezier (surface point n s color)
  (if (= 3 (length color))
      (beziercolor surface (sdl::point-x point) (sdl::point-y point) n s
		   (map-color color))
      (bezierRGBA surface (sdl::point-x point) (sdl::point-y point) n s
		  (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-character (surface point c color)
  (if (= 3 (length color))
      (charactercolor surface (sdl::point-x point) (sdl::point-y point) c
		      (map-color color))
      (characterRGBA surface (sdl::point-x point) (sdl::point-y point) c
		     (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

(defun draw-string (surface point c color)
  (if (= 3 (length color))
      (stringcolor surface (sdl::point-x point) (sdl::point-y point) c
		   (map-color color))
      (stringRGBA surface (sdl::point-x point) (sdl::point-y point) c
		  (sdl::color-r color) (sdl::color-g color) (sdl::color-b color) (sdl::color-a color))))

;;; m

(defun map-color (color)
  (let ((col #x00000000))
    (setf col (logior (ash (sdl::color-r color) 24)
		      (ash (sdl::color-g color) 16)
		      (ash (sdl::color-b color) 8)
		      (if (= 4 (length color))
			  (sdl::color-a color)
			  #xFF)))
    col))

;;; r

(defun rotozoom (surface angle zoom smooth)
  (rotozoomSurface surface angle zoom smooth))

(defun rotozoom-xy (surface angle zoomx zoomy smooth)
  (rotozoomSurfacexy surface angle zoomx zoomy smooth))

(defun rotozoom-size (width height angle zoom)
  (cffi:with-foreign-objects ((dstwidth :int) (dstheight :int))
    (rotozoomSurfaceSize width height angle zoom dstwidth dstheight)
    (vector (cffi:mem-ref dstwidth :int) (cffi:mem-ref dstheight :int))))

(defun rotozoom-size-xy (width height angle zoomx zoomy)
  (cffi:with-foreign-objects ((dstwidth :int) (dstheight :int))
    (rotozoomSurfaceSizeXY width height angle zoomx zoomy dstwidth dstheight)
    (vector (cffi:mem-ref dstwidth :int) (cffi:mem-ref dstheight :int))))

;;; z

(defun zoom (surface zoomx zoomy smooth)
  (zoomSurface surface zoomx zoomy smooth))

(defun zoom-size (width height zoomx zoomy)
  (cffi:with-foreign-objects ((dstwidth :int) (dstheight :int))
    (zoomSurfaceSize width height zoomx zoomy dstwidth dstheight)
    (vector (cffi:mem-ref dstwidth :int) (cffi:mem-ref dstheight :int))))

