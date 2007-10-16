;;;;; A simple example to verify the correctness of the SDL, OpenRM and ODE FLI definitions for Lispworks.
;;;;; Author: Luke J Crook, luke@balooga.com
;;;;; 

(in-package #:cal3d-examples)

(defvar *path* (or *load-truename* *default-pathname-defaults*))

(defun cal3d-demo ()
  (let ((width 320) (height 240))
    (sdl::with-init ()
      (sdl::set-window width height :flags sdl::SDL_OPENGL)
      (sdl::set-framerate 60)
      (rm::with-init ()
	(rm::with-rmpipe ((sdl::get-native-window) width height) a-pipe
	  (let ((root-node (rm::rmrootnode))
		(obj-root (rm::new-node :opacity :opaque))
		(obj-mesh (rm::new-node :opacity :opaque))
		(obj-bounds (rm::new-node :opacity :opaque))
		(m_calCoreModel (cal3d::CalCoreModel_New "dummy"))
		(m_calModel nil)
		(animID nil))
	  
	    ;; Load the Cal3D model.
	    (let ((scale 1.0)
;; 		  (skeleton "paladin/paladin.csf")
;; 		  (animation "paladin/paladin_walk.caf")
;; 		  (meshes '("paladin/paladin_body.cmf"
;; 			    "paladin/paladin_cape.cmf"
;; 			    "paladin/paladin_loincloth.cmf"))
;; 		  (materials '("paladin/paladin_cape.xrf"
;; 			       "paladin/paladin_head.xrf"
;; 			       "paladin/paladin_chest.xrf"))
		  
		  (skeleton "cally/cally.csf")
		  (animation "cally/cally_idle.caf")
		  (meshes '("cally/cally_calf_left.cmf"
			    "cally/cally_calf_right.cmf"
			    "cally/cally_chest.cmf"
			    "cally/cally_foot_left.cmf"
			    "cally/cally_foot_right.cmf"
			    "cally/cally_hand_left.cmf"
			    "cally/cally_hand_right.cmf"
			    "cally/cally_head.cmf"
			    "cally/cally_lowerarm_left.cmf"
			    "cally/cally_lowerarm_right.cmf"
			    "cally/cally_neck.cmf"
			    "cally/cally_pelvis.cmf"
			    "cally/cally_ponytail.cmf"
			    "cally/cally_thigh_left.cmf"
			    "cally/cally_thigh_right.cmf"
			    "cally/cally_upperarm_left.cmf"
			    "cally/cally_upperarm_right.cmf"))
		  (materials '("cally/cally_skin.xrf"
			       "cally/cally_ponytail.xrf"
			       "cally/cally_chest.xrf"
			       "cally/cally_pelvis.xrf"))
		  )
	      
	      ;; Load the core skeleton
	      (if (= (cal3d::CalCoreModel_LoadCoreSkeleton m_calCoreModel (namestring (merge-pathnames skeleton *path*)))
		     0)
		  (format t "Cannot load Skeleton data: ~A~%" (namestring (merge-pathnames skeleton *path*)))
		  (format t "Loaded Skeleton data: ~A~%" (namestring (merge-pathnames skeleton *path*))))
	      ;; Load the core animation "cally_idle"
	      (setf animID (cal3d::CalCoreModel_LoadCoreAnimation m_calCoreModel
								  (namestring (merge-pathnames animation *path*))))
	      (if (= animID -1)
		  (format t "Cannot load Animation data: ~A~%" (namestring (merge-pathnames animation *path*)))
		  (format t "Loaded Animation ID: ~A~%" animID))
	      ;; Load the core meshes
	      (dolist (mesh meshes)
		(if (= (cal3d::CalCoreModel_LoadCoreMesh m_calCoreModel
							 (namestring (merge-pathnames mesh *path*)))
		       -1)
		    (format t "Cannot load mesh data: ~A~%" (namestring (merge-pathnames mesh *path*)))))
	      ;; Load the core materials
	      (dolist (material materials)
		(let ((m (cal3d::CalCoreModel_LoadCoreMaterial m_calCoreModel
							       (namestring (merge-pathnames material *path*)))))
		  (if (= m -1)
		      (format t "Cannot load Materials data: ~A~%" (namestring (merge-pathnames material *path*)))))))
	      
	    
	    ;; make one material thread for each material. Dunno why this is required. Needs further research.
	    (dotimes (materialID (cal3d::CalCoreModel_GetCoreMaterialCount m_calCoreModel))
	      (format t "material thread: ~A~%" materialID)
	      ;; create the a material thread
	      (cal3d::CalCoreModel_CreateCoreMaterialThread m_calCoreModel materialID)
	      ;; initialize the material thread
	      (cal3d::CalCoreModel_SetCoreMaterialId m_calCoreModel materialId 0 materialId))

	    
 	    (setf m_calModel (cal3d::CalModel_New m_calCoreModel))
;;  	    (cal3d::CalModel_setLodLevel m_calModel 0.0)

	    (format t "Number of meshes: ~A~%" (cal3d::CalCoreModel_GetCoreMeshCount m_calCoreModel))
	    (format t "Number of materials: ~A~%" (cal3d::CalCoreModel_GetCoreMaterialCount m_calCoreModel))
	    (format t "Number of animations: ~A~%" (cal3d::CalCoreModel_GetCoreAnimationCount m_calCoreModel))


	    ;; load all textures and store the opengl texture id in the corresponding map in the material
	    (dotimes (materialId (cal3d::CalCoreModel_GetCoreMaterialCount m_calCoreModel))
	      ;; get the core material
	      (let ((pCoreMaterial (cal3d::CalCoreModel_GetCoreMaterial m_calCoreModel materialId)))
		;; loop through all maps of the core material
		(dotimes (mapID (cal3d::CalCoreMaterial_GetMapCount pCoreMaterial))
		  (let* ((strFilename (cal3d::CalCoreMaterial_GetMapFilename pCoreMaterial mapID))
			 (textureID 0))
		    (format t "Warning - Not yet implemented! Attempting to load texture: ~A : ~%" strFilename)
		    ;; load the texture from the file
		    ;;       GLuint textureId = loadTexture(strFilename);
		    
		    ;; store the opengl texture id in the user data of the map
		    (cal3d::CalCoreMaterial_SetMapUserData pCoreMaterial mapID 0)))))
	    
	    ;; Attach all meshes to the model    
	    (dotimes (i (cal3d::CalCoreModel_GetCoreMeshCount m_calCoreModel))
	      (cal3d::CalModel_AttachMesh m_calModel i))

	    ;; Set the material set of the whole model
 	    (cal3d::CalMesh_SetMaterialSet m_calModel 0)

	    ;; Set initial animation state
	    (let ((m_currentAnimationId 0)
		  (m_leftAnimationTime 0.0)
		  (m_blendTime 0.3))
	      (if (> (cal3d::CalCoreModel_GetCoreAnimationCount m_calCoreModel)
		     0)
		  (progn
		    (setf m_currentAnimationId 0)
		    (setf m_leftAnimationTime (- (cal3d::CalCoreAnimation_GetDuration
						  (cal3d::CalCoreModel_GetCoreAnimation m_calCoreModel
											m_currentAnimationId))
						 m_blendTime))
		    (if (> (cal3d::CalCoreModel_GetCoreAnimationCount m_calCoreModel) 1)
			(cal3d::CalMixer_ExecuteAction (cal3d::CalModel_GetMixer m_calModel)
						       m_currentAnimationId
						       0.0
						       m_blendTime)
			(cal3d::CalMixer_blendCycle (cal3d::CalModel_GetMixer m_calModel)
						    m_currentAnimationId
						    1.0
						    0.0))
		    (cal3d::CalModel_Update m_calModel (coerce (if (sdl::get-timescale)
								   (sdl::get-timescale)
								   0.0)
							       'single-float)))
		  (setf m_currentAnimationId -1
			m_leftAnimationTime -1.0)))
    
	    ;; onRender
	    (let ((m_vertexCount 0)
		  (m_faceCount 0)
		  (pCalRenderer (cal3d::CalModel_GetRenderer m_calModel))) ;get the renderer of the model
	      (when (> (cal3d::CalRenderer_BeginRendering pCalRenderer)
		       0)
		(format t "CalRenderer_BeginRendering~%")
		;; get the number of meshes. render all meshes of the model
		(dotimes (meshid (cal3d::CalRenderer_GetMeshCount pCalRenderer))
		  (format t "Mesh ~A of ~A : " (+ 1 meshid) (cal3d::CalRenderer_GetMeshCount pCalRenderer))
		  ;; get the number of submeshes. ;; render all submeshes of the mesh
		  (dotimes (submeshId (cal3d::CalRenderer_GetSubmeshCount pCalRenderer meshid))
		    (format t "Submesh ~A of ~A~%" (+ 1 submeshid) (cal3d::CalRenderer_GetSubmeshCount pCalRenderer meshid))
		    ;; select mesh and submesh for further data access
		    (when (> (cal3d::CalRenderer_SelectMeshSubmesh pCalRenderer meshId submeshId)
			     0)
		      (let ((submesh-node (rm::new-node :opacity :opaque))
			    (meshvertices (cffi:foreign-alloc :float
							      :count
							      (* 3 (cal3d::CalRenderer_GetVertexCount pCalRenderer))))
			    (meshNormals (cffi:foreign-alloc :float
							     :count
							     ( * 3 (cal3d::CalRenderer_GetVertexCount pCalRenderer))))
			    (meshFaces (cffi:foreign-alloc :int
							   :count
							   (* 3 (cal3d::CalRenderer_GetFaceCount pCalRenderer)))))
			(format t "Allocated space for mesh: vertices: ~A, normals: ~A, faces: ~A~%"
				(cal3d::CalRenderer_GetVertexCount pCalRenderer)
				(cal3d::CalRenderer_GetVertexCount pCalRenderer)
				(cal3d::CalRenderer_GetFaceCount pCalRenderer))

			;; get the transformed vertices of the submesh
			(format t "CalRenderer_GetVertices: ~A~%"
				(cal3d::CalRenderer_GetVertices pCalRenderer meshvertices))
			;; get the transformed normals of the submesh
			(format t "CalRenderer_GetNormals: ~A~%"
				(cal3d::CalRenderer_GetNormals pCalRenderer meshNormals))
			;; get the faces of the submesh
			(format t "CalRenderer_GetFaces: ~A~%"
				(cal3d::CalRenderer_GetFaces pCalRenderer meshFaces))

			(rm::set-node-property submesh-node
					   :ambient-color (cal3d::get-ambient-color pCalRenderer)
					   :diffuse-color (cal3d::get-diffuse-color pCalRenderer)
					   :specular-color (cal3d::get-specular-color pCalRenderer)
					   :specular-exponent (cal3d::get-specular-exponent pCalRenderer))

			(format t "ambient-color: ~A~%" (cal3d::get-ambient-color pCalRenderer))
			(format t "diffuse-color: ~A~%" (cal3d::get-diffuse-color pCalRenderer))
			(format t "specular-color: ~A~%" (cal3d::get-specular-color pCalRenderer))
			(format t "specular-exponent: ~A~%" (cal3d::get-specular-exponent pCalRenderer))
			

			(cal3d::CalMesh_SetMaterialSet (cal3d::CalModel_GetMesh m_calModel meshid) 2)
			
;; 			(let ((faces (loop for i from 0 upto (- (cal3d::CalRenderer_GetFaceCount pCalRenderer) 1)
;; 					   for index = 0 then (+ index 3)
;; 					   collect (list (cffi:mem-aref meshfaces :int (+ index 0))
;; 							 (cffi:mem-aref meshfaces :int (+ index 1))
;; 							 (cffi:mem-aref meshfaces :int (+ index 2))))))
;; 			  (dolist (face faces)
;; 			    (format t "Face: ~A: " face)
;; 			    (dolist (vertex face)
;; 			      (format t "Vertex: ~A : "
;; 				      (list (cffi:mem-ref meshvertices :float
;; 							  (+ 0 ( * 3 (* vertex
;; 									(foreign-type-size :float)))))
;; 					    (cffi:mem-ref meshvertices :float
;; 							  (+ 1 ( * 3 (* vertex
;; 									(foreign-type-size :float)))))
;; 					    (cffi:mem-ref meshvertices :float
;; 							  (+ 2 ( * 3 (* vertex
;; 									(foreign-type-size :float))))))))
;; 			    (format t "~%")))

			  

		
			;; create the indexed triangles
			(rm::add-primitive submesh-node
					   (rm::new-triangles-primitive meshvertices
									(cal3d::CalRenderer_GetVertexCount
									 pCalRenderer)
									meshFaces
									(* 3
									   (cal3d::CalRenderer_GetFaceCount
									    pCalRenderer))
									:normals meshNormals)
					   :bounds t :center t)

			(rm::add-node obj-mesh submesh-node)
			
			(cffi:foreign-free meshvertices)
			(cffi:foreign-free meshnormals)
			(cffi:foreign-free meshfaces)))))
		(cal3d::CalRenderer_EndRendering pCalRenderer)))

	    
	    (format t "Done adding model to scene graph~%")
;; 	    (rm::node-add-primitive obj-bounds (rm::create-bounds-from-node obj-mesh :primitives t))
	    (rm::add-node obj-root obj-mesh)
	    (rm::add-node obj-root obj-bounds)
	    (rm::add-node root-node
			  obj-root :union t :compute-center t)
	    (rm::set-default-scene root-node width height)

	    (sdl::with-events
	      (:quit () t)
	      (:keydown (:key key)
			(cond
			  ((sdl:key= key :SDLK_ESCAPE)
			   (sdl::push-quitevent))))
	      (:mousemotion (:state state :x x :y y)
			    (cond
			      ((equal (logand (sdl::sdl_button sdl::SDL_BUTTON_LEFT) state) 1)
			       (rm::arc root-node width height x y))
			      ((equal (logand (sdl::sdl_button sdl::SDL_BUTTON_RIGHT) state) 4)
			       (rm::dolly root-node width height y))
			      ((equal (logand (sdl::sdl_button sdl::SDL_BUTTON_MIDDLE) state) 2)
			       (rm::translate root-node width height x y))))
	      (:mousebuttondown (:button button :state state :x x :y y)
				(cond
				  ((equal button sdl::sdl_button_left)
				   (rm::reset-arc root-node width height x y))
				  ((equal button sdl::sdl_button_right)
				   (rm::reset-dolly width height y))
				  ((equal button sdl::sdl_button_middle)
				   (rm::reset-translate width height x y))))
	      (:idle
	       (rm::RMFRAME a-pipe root-node)
	       (sdl::SDL_GL_SwapBuffers)))))))))

