;;;;; A simple example to verify the correctness of the SDL, OpenRM and ODE FLI definitions for Lispworks.
;;;;; Author: Luke J Crook, luke@balooga.com
;;;;; 

(in-package #:rm-examples)

(defun set-spotlight (node &key (direction #(0.0 -1.0 0.0)) (position #(5.0 5.0 5.0)) (color #(0.5 0.5 0.5 1.0))
		      (exponent 4.0) (cutoff 45) (light :light1))
  (rm::with-light ((rm::rmLightNew))
    (rm::set-light-type :spot)
    (rm::set-light-diffuse-color color)
    (rm::set-light-specular-color color)
    (rm::set-light-position position)
    (rm::set-spot-direction direction)
    (rm::set-spot-cutoff cutoff)
    (rm::set-spot-exponent exponent)
    (rm::set-scene-light node light)))

(defun new-spotlight (&key (direction #(0.0 -1.0 0.0)) (position #(5.0 5.0 5.0)) (color #(0.5 0.5 0.5 1.0)) (cutoff 45))
  (let* ((node (rm::new-node :name "spotlight" :opacity :opaque))
	 (scale (sin (rm::RM_DEGREES_TO_RADIANS CUTOFF)))
	 (primitive (rm::new-cone-primitive :radius scale :color color
					    :vertices (list (vector (rm::vertex-x direction)
								    (rm::vertex-y direction)
								    (rm::vertex-z direction))
							    (vector 0.0 0.0 0.0))
					    :tesselate 32)))

    (rm::node-add-primitive node primitive)
    (rm::set-node-position node position)
    
    (rm::NodeSetDiffuseColor node color)
    (rm::NodeSetSpecularColor node color)
    (rm::NodeSetAmbientColor node color)
    node))

(defun spotlight ()
  (let ((width 320) (height 240)
	(spot-position #(5.0 5.0 5.0))
	(spot-direction #(0.0 -1.0 0.0))
	(spot-color #(0.9 0.5 0.5 1.0))
	(spot-exponent 4.0)
	(spot-cutoff 45.0))
    (sdl:with-init ()
      (sdl:with-display (width height :flags sdl:SDL_OPENGL)
	(sdl:set-framerate 30)
	(rm::with-init ()
	  (rm::with-rmpipe ((sdl::get-native-window) width height) a-pipe
			   (let ((root-node (rm::rmrootnode))
				 (obj-root (rm::new-node :opacity :opaque))
				 (cube-root (rm::new-node :opacity :opaque))
				 (light-root (rm::new-node :opacity :opaque))
				 (spot-root nil)
				 (dummy (rm::new-node :opacity :opaque))
				 (origin #(0.0 0.0 0.0))
				 (xzmax #(10.0 0.0 10.0))
				 (yzmax1 #(0.0 10.0 10.0))
				 (origin2 #(10.0 0.0 0.0))
				 (yzmax2 #(10.0 10.0 10.0))
				 (origin3 #(0.0 10.0 0.0))
				 (xzmax2 #(10.0 10.0 10.0))
				 (xymax #(10.0 10.0 10.0)))

			     (set-spotlight light-root :color spot-color :exponent spot-exponent
					    :position spot-position :direction spot-direction :cutoff spot-cutoff)
			     (setf spot-root (new-spotlight :direction spot-direction :color spot-color :cutoff spot-cutoff
							    :position spot-position))
	  
			     (rm::node-add-primitive cube-root
						     (rm::new-plane-primitive :orientation :xz
									      :subdivisions 30
									      :vertices (list origin xzmax)
									      :sign 1
									      :color nil)

						     (rm::new-plane-primitive :orientation :xz
									      :subdivisions 30
									      :vertices (list origin3 xzmax2)
									      :sign -1
									      :color nil)

						     (rm::new-plane-primitive :orientation :yz
									      :subdivisions 30
									      :vertices (list origin yzmax1)
									      :sign 1
									      :color nil)

						     (rm::new-plane-primitive :orientation :yz
									      :subdivisions 30
									      :vertices (list origin2 yzmax2)
									      :sign -1
									      :color nil)

						     (rm::new-plane-primitive :orientation :xy
									      :subdivisions 30
									      :vertices (list origin xymax)
									      :sign 1
									      :color nil))

			     (rm::add-node light-root cube-root)
			     (rm::add-node light-root spot-root)
			     (rm::add-node obj-root light-root)

			     (rm::add-node root-node
					   obj-root :union t :compute-center t)
    
			     (rm::NodeSetSceneBackgroundColor obj-root #(0.2 0.2 0.3 1.0))
	    
			     (rm::set-default-scene obj-root width height)

			     (sdl::with-events ()
			       (:quit () t)
			       (:keydown (:key key)
					 (if (sdl:key= key :SDLK_ESCAPE)
					     (sdl:push-quitevent)))
			       (:mousemotion (:state state :x x :y y)
					     (cond
					       ((equal (logand (sdl::sdl_button sdl::SDL_BUTTON_LEFT) state) 1)
						(rm::arc dummy width height x y)
						(set-spotlight light-root
							       :direction (rm::point-matrix-transform dummy #(0.0 -1.0 0.0))
							       :color spot-color)
						(rm::rotate-node spot-root :match-node dummy))
					       ((equal (logand (sdl::sdl_button sdl::SDL_BUTTON_RIGHT) state) 4)
						(rm::dolly obj-root width height y))
					       ((equal (logand (sdl::sdl_button sdl::SDL_BUTTON_MIDDLE) state) 2)
						(rm::translate obj-root width height x y))))
			       (:mousebuttondown (:button button :x x :y y)
						 (cond
						   ((equal button sdl::sdl_button_left)
						    (rm::reset-arc dummy width height x y))
						   ((equal button sdl::sdl_button_right)
						    (rm::reset-dolly width height y))
						   ((equal button sdl::sdl_button_middle)
						    (rm::reset-translate width height x y))))
			       (:idle ()
				      (rm::RMFRAME a-pipe root-node)
				      (sdl::SDL_GL_SwapBuffers))))))))))

