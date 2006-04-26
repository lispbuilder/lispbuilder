;;;;; A simple example to verify the correctness of the SDL, OpenRM and ODE FLI definitions for Lispworks.
;;;;; Author: Luke J Crook, luke@balooga.com
;;;;; 

;; We shadow all exported symbols in :engine
;; (shadowing-import (let ((lst ()))
;;                     (do-external-symbols (s (find-package 'ENGINE) lst) (push s lst))
;;                     lst))

(in-package #:rm-examples)


(defun aux-handle-motion (node button x y width height)
  (cond
    ((equal button sdl::sdl_button_left)
     (rm::arc node width height x y))
    ((equal button sdl::sdl_button_right)
     (rm::dolly node width height y))
    ((equal button sdl::sdl_button_middle)
     (rm::translate node width height x y))))

(defun aux-handle-buttons (node button x y width height)
  (cond
    ((equal button sdl::sdl_button_left)
     (rm::reset-arc node width height x y)
     sdl::sdl_button_left)
    ((equal button sdl::sdl_button_right)
     (rm::reset-dolly width height y)
     sdl::sdl_button_right)
    ((equal button sdl::sdl_button_middle)
     (rm::reset-translate width height x y)
     sdl::sdl_button_middle)))

(defun jballs ()
  (let ((width 640) (height 480)
	(button-state nil)
	(bounds-display nil))
    (sdl::with-init ()
      (sdl::set-window width height :flags sdl::SDL_OPENGL)
      (sdl::set-framerate 60)
      (rm::with-init ()
	(rm::with-rmpipe ((sdl::get-hwnd) width height) a-pipe
	  (let ((root-node (rm::rmrootnode))
		(my-root (rm::new-node :opacity :opaque))
		(sloth (rm::new-node :opacity :opaque))
		(zippy (rm::new-node :opacity :opaque))
		(floor (rm::new-node :opacity :opaque))
		(moving-group (rm::new-node :opacity :opaque))
		(stationary-group (rm::new-node :opacity :opaque)))
	    (rm::node-add-primitive sloth
				    (rm::new-sphere-primitive :color #(0.5 0.1 0.0 1.0)
							      :position #(0.0 0.0 0.0)
							      :tesselate 512
							      :radius 1.0))
	    (rm::node-add-primitive zippy
				    (rm::new-sphere-primitive :color #(1.0 0.0 0.0 1.0)
							      :position #(1.5 -1.0 0.0)
							      :radius 0.25))
	    (rm::node-add-primitive floor
				    (rm::new-plane-primitive :color #(0.3 0.9 0.5 1.0)
							     :subdivisions 20
							     :vertices (list #(-3.0 -2.0 -3.0)
									     #(3.0 -2.0 3.0))))

	    (rm::nodesetcenter moving-group #(0.0 0.0 0.0))
	    
	    (rm::add-node stationary-group floor)
	    (rm::add-node stationary-group sloth)
	    (rm::add-node moving-group zippy)

	    (rm::with-light red-light
		(rm::set-light-position #(1.5 -1.0 0.0))
		(rm::set-diffuse-color #(1.0 0.0 0.0 1.0))
		(rm::set-specular-color #(1.0 0.0 0.0 1.0))
		(rm::set-type :point)
		(rm::set-scene moving-group :light3))

	    (rm::add-node my-root moving-group)
	    (rm::add-node moving-group stationary-group)
	    (rm::add-node root-node
			  my-root :union t :compute-center t)

	    (rm::NodeSetSceneBackgroundColor my-root #(0.2 0.2 0.3 1.0))
	    
	    (rm::set-default-scene my-root width height)

	    (sdl::with-events
	      (:quit t)
	      (:keydown (state scancode key mod unicode)
			(cond
			  ((sdl::is-key key :SDLK_ESCAPE)
			   (sdl::push-quitevent))
			  ((sdl::is-key key :SDLK_A)
			   (unless bounds-display
			     ;;Add a bounding box for the nodes.
			     (rm::node-add-primitive sloth (rm::create-bounds-from-node sloth))
			     (rm::node-add-primitive zippy (rm::create-bounds-from-node zippy))
			     (rm::node-add-primitive floor (rm::create-bounds-from-node floor))
			     (setf bounds-display t)))))
	      (:mousemotion (state x y xrel yrel)
			    (aux-handle-motion my-root button-state x y width height))
	      (:mousebuttondown (button state x y)
				(setf button-state (aux-handle-buttons my-root button x y width height)))
	      (:mousebuttonup (button state x y)
			      (setf button-state nil))
	      (:idle
 	       (rm::rotate-node moving-group 0.0 1.0 0.0 :only-this-node t)
	       (rm::RMFRAME a-pipe root-node)
	       (sdl::SDL_GL_SwapBuffers)))))))))

(defun rm-example-1 ()
  (let ((width 320) (height 200))
    (sdl::with-init ()
      (sdl::set-window width height :flags sdl::SDL_OPENGL)
      (rm::with-init ()
	(rm::with-rmpipe ((sdl::get-hwnd) width height) a-pipe
	  (let ((root-node (rm::rmrootnode))
		(a-node (rm::new-node :opacity :opaque)))
	    (rm::node-add-primitive a-node 
				    (rm::new-sphere-primitive :color #(0.5 0.0 0.1)
							      :position #(0.0 0.0 0.0)
							      :radius 1.0
							      :tesselate 8)
				    (rm::new-sphere-primitive :color #(0.5 0.0 0.1)
							      :position #(2.0 0.0 0.0)
							      :radius 1.0
							      :tesselate 32)
				    (rm::new-sphere-primitive :color #(0.5 0.0 0.1)
							      :position #(4.0 0.0 0.0)
							      :radius 1.0
							      :tesselate 128)
				    (rm::new-sphere-primitive :color #(0.5 0.0 0.1)
							      :position #(6.0 0.0 0.0)
							      :radius 1.0
							      :tesselate 512))
	    (rm::node-add-primitive a-node
				    ; Create a cube by specifying the vertices
				    (rm::new-cube-primitive :color #(0.5 0.0 0.1)
							    :vertices  (list #(-1.0 1.0 -1.0)
									     #(1.0 3.0 1.0)))
				    ; Create a cube by specifying width, height and depth
				    (rm::new-cube-primitive :color #(0.5 0.0 0.1)
							    :dimensions #(2.0 2.0 2.0)
							    :position #(-1.0 -3.0 -1.0)))

	    (rm::node-add-primitive a-node
				    (rm::new-plane-primitive :color #(0.3 0.9 0.5 1.0)
							     :subdivisions 100
							     :vertices (list #(-5.0 -8.0 -5.0)
									     #(5.0 -8.0 5.0))))
	    
	    (rm::add-node root-node
			  a-node :union t :compute-center t)

	    (rm::set-node-position a-node #(0.0 0.0 0.0))

	    (rm::set-default-scene root-node width height)

	    (sdl::with-events
	      (:quit t)
	      (:keydown (state scancode key mod unicode)
			(if (sdl::is-key key :SDLK_ESCAPE)
			    (sdl::push-quitevent)))
	      (:idle
 	       (rm::rotate-node a-node 0.0 1.0 0.0)
	       (rm::RMFRAME a-pipe root-node)
	       (sdl::SDL_GL_SwapBuffers)))))))))

(defun rm-example-2 ()
  (let ((width 320) (height 200))
    (sdl::with-init ()
      (sdl::set-window width height :flags sdl::SDL_OPENGL)
      (rm::with-init ()
	(rm::with-rmpipe ((sdl::get-hwnd) width height) a-pipe
	  (let ((root-node (rm::rmrootnode))
		(a-node (rm::new-node :opacity :opaque)))
	    (dotimes (i 5)
	      (dotimes (j 5)
		(rm::node-add-primitive a-node 
					(rm::new-sphere-primitive :color (vector (random 1.0) (random 1.0) (random 1.0)
										 1.0)
								  :position (vector (+ 0.0 i) (+ 0.0 j) 0.0)
								  :radius 0.5))))

	    (rm::add-node root-node
			  a-node :union t :compute-center t)

	    (rm::set-node-position a-node #(0.0 0.0 0.0))

	    (rm::set-default-scene root-node width height)

	    (sdl::with-events
	      (:quit t)
	      (:keydown (state scancode key mod unicode)
			(if (sdl::is-key key :SDLK_ESCAPE)
			    (sdl::push-quitevent)))
	      (:idle
 	       (rm::rotate-node a-node 0.0 1.0 0.0)
	       (rm::RMFRAME a-pipe root-node)
	       (sdl::SDL_GL_SwapBuffers)))))))))

#|
(defun rm-sin-wave ()
  (let ((width 320) (height 200)
	(button-state nil))
    (init-objects)

    (sdl::with-init ()
      (sdl::set-window width height :flags sdl::sdl_opengl)
      (rm::with-engine ((sdl::get-hwnd) width height) root-node

	(let* ((rotate (new-actor :script (new-rotate :dx 0.0 :dy 1.0 :dz 0.0)))
	       (action-delta 0)
	       (actors (loop for i from 0 to 50 append
			   (loop for j from 0 to 5
				 collect (new-actor :primitives (rm::new-sphere (* i 1.0) 0.0 (* j 1.0) 0.5)
						    :script (new-sinus :active-frame (incf action-delta 0.5)
								       :sinus-scale 5
								       :sinus-speed 5
								       :axis :y))))))
	  (add-actor root-node
		     (add-actor rotate
				actors :union t :compute-center t)))
	
	(rm::set-default-scene root-node width height)
	
	(sdl::with-events
	  (:quit t)
	  (:keydown (state scancode key mod unicode)
		    (if (sdl::is-key key :SDLK_ESCAPE)
			(sdl::push-quitevent)))
;; 	  (:mousemotion (state x y xrel yrel)
;; 			(rm::aux-handle-motion root-node button-state x y width height))
;; 	  (:mousebuttondown (button state x y)
;; 			    (setf button-state (rm::aux-handle-buttons root-node button x y width height)))
;; 	  (:mousebuttonup (button state x y)
;; 			  (setf button-state nil))
	  (:idle
	   (rm::update-all)))))))



(defun rm-rotating-cubes ()
  (let ((width 320) (height 200)
	(button-state nil))
    (init-objects)

    (sdl::with-init ()
      (sdl::set-window width height :flags sdl::sdl_opengl)
      (rm::with-engine ((sdl::get-hwnd) width height) root-node

	(let ((cubes nil))
	  (setf cubes (loop for i from 0 to 10
			    append (loop for j from 0 to 10
					 collect (new-actor :script (new-rotate :dx 1.0 :dy 1.0 :dz 1.0)
							  :primitives (rm::new-box -1.0 -1.0 -1.0
										   1.0 1.0 1.0
										   (random 1.0) (random 1.0) (random 1.0)
										   :cx (* i 3.5) :cy (* j 3.5) :cz 0)))))
	  (add-actor root-node cubes :union t :compute-center t))
	(rm::set-default-scene root-node width height)

	(sdl::with-events
	  (:quit t)
	  (:keydown (state scancode key mod unicode)
		    (when (equal key 'sdl::SDLK-ESCAPE)
		      (sdl::quitevent)))
	  (:mousemotion (state x y xrel yrel)
			(rm::aux-handle-motion root-node button-state x y width height))
	  (:mousebuttondown (button state x y)
			    (setf button-state (rm::aux-handle-buttons root-node button x y width height)))
	  (:mousebuttonup (button state x y)
			  (setf button-state nil))
	  (:idle
	   (update-all)))))))

(defun rm-spheres ()
  (let ((width 320) (height 200)
	(button-state nil))
    (init-objects)

    (sdl::with-init ()
      (sdl::set-window width height :flags sdl::sdl_opengl)
      (rm::with-engine ((sdl::get-hwnd) width height) root-node

	(add-actor root-node
		   (new-actor :script (defscript ()
					(rotate-actor actor 0.0 1.0 0.0))
			    :primitives (loop for i from 0 to 10 
					      append (loop for j from 0 to 10
							   collect (rm::define-sphere (rm::translate
										       (rm::rmvertex-3d (* i 2.0)
													(* j 2.0)
													0.0))
										      (rm::radius 1.0)
										      (rm::tesselate 128)
										      (rm::color 0.0 1.0 1.0)))))
		   :union t
		   :compute-center t)
		  
	(rm::set-default-scene root-node width height)

	(sdl::with-events
	  (:quit t)
	  (:keydown (state scancode key mod unicode)
		    (when (equal key 'sdl::SDLK-ESCAPE)
		      (sdl::quitevent)))
	  (:mousemotion (state x y xrel yrel)
			(rm::aux-handle-motion root-node button-state x y width height))
	  (:mousebuttondown (button state x y)
			    (setf button-state (rm::aux-handle-buttons root-node button x y width height)))
	  (:mousebuttonup (button state x y)
			  (setf button-state nil))
	  (:idle
	   (update-all)))))))

(defun rm-cube ()
  (let ((width 320) (height 200)
	(button-state nil))
    (init-objects)

    (sdl::with-init ()
      (sdl::set-window width height :flags sdl::sdl_opengl)
      (rm::with-engine ((sdl::get-hwnd) width height) root-node

	(add-actor root-node
		   (new-actor :script (new-rotate :dx 1.0 :dy 1.0 :dz 1.0)
			      :primitives (rm::define-cube
					    (rm::color (random 1.0) (random 1.0) (random 1.0) 1.0)
					    (rm::vmin-vmax (rm::rmvertex-3d -1.0 -1.0 -1.0)
							   (rm::rmvertex-3d  1.0  1.0  1.0))))
		   :union t
		   :compute-center t)
		  
	(rm::set-default-scene root-node width height)

	(sdl::with-events
	  (:quit t)
	  (:keydown (state scancode key mod unicode)
		    (when (equal key 'sdl::SDLK-ESCAPE)
		      (sdl::quitevent)))
	  (:mousemotion (state x y xrel yrel)
			(rm::aux-handle-motion root-node button-state x y width height))
	  (:mousebuttondown (button state x y)
			    (setf button-state (rm::aux-handle-buttons root-node button x y width height)))
	  (:mousebuttonup (button state x y)
			  (setf button-state nil))
	  (:idle
	   (update-all)))))))

(defun rm-cube-2 ()
  (let ((width 320) (height 200)
	(button-state nil))
    (init-objects)

    (sdl::with-init ()
      (sdl::set-window width height :flags sdl::sdl_opengl)
      (rm::with-engine ((sdl::get-hwnd) width height) root-node

	(add-actor root-node
		   (actor 
		    (primitive (rm::cube
				(rm::color (random 1.0) (random 1.0) (random 1.0) 1.0)
				(rm::vertices (rm::rmvertex-3d -1.0 -1.0 -1.0)
					      (rm::rmvertex-3d  1.0  1.0  1.0))))
		    (script (new-rotate :dx 1.0 :dy 1.0 :dz 1.0)))
		   :union t
		   :compute-center t)
		  
	(rm::set-default-scene root-node width height)

	(sdl::with-events
	  (:quit t)
	  (:keydown (state scancode key mod unicode)
		    (when (equal key 'sdl::SDLK-ESCAPE)
		      (sdl::quitevent)))
	  (:mousemotion (state x y xrel yrel)
			(rm::aux-handle-motion root-node button-state x y width height))
	  (:mousebuttondown (button state x y)
			    (setf button-state (rm::aux-handle-buttons root-node button x y width height)))
	  (:mousebuttonup (button state x y)
			  (setf button-state nil))
	  (:idle
	   (update-all)))))))


|#

