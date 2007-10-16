;;;;; A simple example to verify the correctness of the SDL, OpenRM and ODE FLI definitions for Lispworks.
;;;;; Author: Luke J Crook, luke@balooga.com
;;;;; 

(in-package #:rm-examples)

(defun aux-handle-motion-sdl (node button-state x y width height)
  (cond
    ((equal (logand (sdl::sdl_button sdl::SDL_BUTTON_LEFT) button-state) 1)
     (rm::arc node width height x y))
    ((equal (logand (sdl::sdl_button sdl::SDL_BUTTON_RIGHT) button-state) 4)
     (rm::dolly node width height y))
    ((equal (logand (sdl::sdl_button sdl::SDL_BUTTON_MIDDLE) button-state) 2)
     (rm::translate node width height x y))))
  
(defun aux-handle-buttons-sdl (node button x y width height)
  (cond
    ((equal button sdl::sdl_button_left)
     (rm::reset-arc node width height x y))
    ((equal button sdl::sdl_button_right)
     (rm::reset-dolly width height y))
    ((equal button sdl::sdl_button_middle)
     (rm::reset-translate width height x y))))

(defun jballs ()
  (let ((width 640) (height 480)
	(button-state nil)
	(bounds-display nil))
    (sdl::with-init ()
      (sdl::set-window width height :flags sdl::SDL_OPENGL)
      (sdl::set-framerate 30)
      (rm::with-init ()
	(rm::with-rmpipe ((sdl::get-native-window) width height) a-pipe
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

	    (rm::with-light ((rm::rmLightNew))
		(rm::set-light-position #(1.5 -1.0 0.0))
		(rm::set-light-diffuse-color #(1.0 0.0 0.0 1.0))
		(rm::set-light-specular-color #(1.0 0.0 0.0 1.0))
		(rm::set-light-type :point)
		(rm::set-scene-light moving-group :light3))

	    (rm::add-node my-root moving-group)
	    (rm::add-node moving-group stationary-group)
	    (rm::add-node root-node
			  my-root :union t :compute-center t)

	    (rm::NodeSetSceneBackgroundColor my-root #(0.2 0.2 0.3 1.0))
	    
	    (rm::set-default-scene my-root width height)

	    (sdl::with-events ()
	      (:quit () t)
	      (:keydown (:key key)
			(cond
			  ((sdl::key= key :SDLK_ESCAPE)
			   (sdl::push-quitevent))
			  ((sdl::key= key :SDLK_A)
			   (unless bounds-display
			     ;;Add a bounding box for the nodes.
			     (rm::node-add-primitive sloth (rm::create-bounds-from-node sloth))
			     (rm::node-add-primitive zippy (rm::create-bounds-from-node zippy))
			     (rm::node-add-primitive floor (rm::create-bounds-from-node floor))
			     (setf bounds-display t)))))
	      (:mousemotion (:state state :x x :y y)
			    (aux-handle-motion-sdl my-root state x y width height))
	      (:mousebuttondown (:button button :x x :y y)
				(aux-handle-buttons-sdl my-root button x y width height))
	      (:mousebuttonup ()
			      (setf button-state nil))
	      (:idle ()
 	       (rm::rotate-node moving-group :direction #(0.0 1.0 0.0) :only-this-node t)
	       (rm::RMFRAME a-pipe root-node)
	       (sdl::SDL_GL_SwapBuffers)))))))))

