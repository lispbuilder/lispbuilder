;; lispbuilder-sdl sample program
;; (C)2006 Frank Buss
;; see COPYING for license

;; From "http://www.frank-buss.de/lisp/canvas.html"

;; To run this sample you need asdf, cffi and lispbuild-sdl installed, 
;; (asdf:operate 'asdf:load-op :cffi)
;; (asdf:operate 'asdf:load-op :lispbuilder-sdl-examples)

;; (sdl-examples:mouse-painter)

(in-package #:sdl-examples)

(defun mouse-painter ()
  (sdl:with-init ()
    (sdl:with-display (640 480 :title-caption "Mouse Painter" :icon-caption "Mouse Painter")
      (sdl:clear-screen #(255 255 255))
      (sdl:with-events
	(:quit t)
	(:videoexpose (sdl:update-screen))
	(:keydown (state scancode key mod unicode)
		  (when (sdl:is-key key :SDLK_SPACE)
		    (sdl:clear-screen #(255 255 255))
		    (sdl:update-screen)))
	(:mousemotion (state x y xrel yrel)
		      (when (= 1 state)
			(sdl:draw-line x y (- x xrel) (- y yrel) :color #(0 0 0))
			(sdl:update-screen)))))))
