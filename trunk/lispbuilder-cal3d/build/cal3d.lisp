
;;;; Cal3D v0.10.0 CFFI lisp wrapper
;;;; Part of the CL-Gardeners project
;;;; http://wiki.alu.org/Application_Builder
;;;; (C)2006 Luke J Crook
;;;; See COPYING for license

(in-package #:lispbuilder-cal3d)

;;;; Macro to handle defenum (thanks to Frank Buss for this SWIG/CFFI feature
; this handles anonymous enum types differently

(defmacro defenum (&body enums)	
 `(progn ,@(loop for value in enums
                 for index = 0 then (1+ index)
                 when (listp value) do (setf index (second value)
                                             value (first value))
                 collect `(defconstant ,value ,index))))

(defcenum Boolean
	(:False 0)
	(:True 1))

(defcenum CalAnimationType
	(:ANIMATION_TYPE_NONE 0)
	:ANIMATION_TYPE_CYCLE
	:ANIMATION_TYPE_POSE
	:ANIMATION_TYPE_ACTION)

(defcenum CalAnimationState
	(:ANIMATION_STATE_NONE 0)
	:ANIMATION_STATE_SYNC
	:ANIMATION_STATE_ASYNC
	:ANIMATION_STATE_IN
	:ANIMATION_STATE_STEADY
	:ANIMATION_STATE_OUT)

(defcenum CalErrorCode
	(:ERROR_CODE_OK 0)
	:ERROR_CODE_INTERNAL
	:ERROR_CODE_INVALID_HANDLE
	:ERROR_CODE_MEMORY_ALLOCATION_FAILED
	:ERROR_CODE_FILE_NOT_FOUND
	:ERROR_CODE_INVALID_FILE_FORMAT
	:ERROR_CODE_FILE_PARSER_FAILED
	:ERROR_CODE_INDEX_BUILD_FAILED
	:ERROR_CODE_NO_PARSER_DOCUMENT
	:ERROR_CODE_INVALID_ANIMATION_DURATION
	:ERROR_CODE_BONE_NOT_FOUND
	:ERROR_CODE_INVALID_ATTRIBUTE_VALUE
	:ERROR_CODE_INVALID_KEYFRAME_COUNT
	:ERROR_CODE_INVALID_ANIMATION_TYPE
	:ERROR_CODE_FILE_CREATION_FAILED
	:ERROR_CODE_FILE_WRITING_FAILED
	:ERROR_CODE_INCOMPATIBLE_FILE_VERSION
	:ERROR_CODE_NO_MESH_IN_MODEL
	:ERROR_CODE_MAX_ERROR_CODE)

(defcfun ("CalCoreAnimation_New" CalCoreAnimation_New) :pointer
  (a-void :pointer))




;;;SWIG wrapper code starts here

(defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(progn ,@(loop for value in enums
                  for index = 0 then (1+ index)
                  when (listp value) do (setf index (second value)
                                              value (first value))
                  collect `(defconstant ,value ,index))))

;;;SWIG wrapper code ends here


(defcfun ("CalAnimation_Delete" CalAnimation_Delete) :void
  (self :pointer))

(defcfun ("CalAnimation_GetCoreAnimation" CalAnimation_GetCoreAnimation) :pointer
  (self :pointer))

(defcfun ("CalAnimation_GetState" CalAnimation_GetState) :int
  (self :pointer))

(defcfun ("CalAnimation_GetTime" CalAnimation_GetTime) :float
  (self :pointer))

(defcfun ("CalAnimation_GetType" CalAnimation_GetType) :int
  (self :pointer))

(defcfun ("CalAnimation_GetWeight" CalAnimation_GetWeight) :float
  (self :pointer))

(defcfun ("CalAnimationAction_Delete" CalAnimationAction_Delete) :void
  (self :pointer))

(defcfun ("CalAnimationAction_Execute" CalAnimationAction_Execute) :int
  (self :pointer)
  (delayIn :float)
  (delayOut :float))

(defcfun ("CalAnimationAction_New" CalAnimationAction_New) :pointer
  (pCoreAnimation :pointer))

(defcfun ("CalAnimationAction_Update" CalAnimationAction_Update) :int
  (self :pointer)
  (deltaTime :float))

(defcfun ("CalAnimationCycle_Blend" CalAnimationCycle_Blend) :int
  (self :pointer)
  (weight :float)
  (delay :float))

(defcfun ("CalAnimationCycle_Delete" CalAnimationCycle_Delete) :void
  (self :pointer))

(defcfun ("CalAnimationCycle_New" CalAnimationCycle_New) :pointer
  (pCoreAnimation :pointer))

(defcfun ("CalAnimationCycle_SetAsync" CalAnimationCycle_SetAsync) :void
  (self :pointer)
  (time :float)
  (duration :float))

(defcfun ("CalAnimationCycle_Update" CalAnimationCycle_Update) :int
  (self :pointer)
  (deltaTime :float))

(defcfun ("CalBone_BlendState" CalBone_BlendState) :void
  (self :pointer)
  (weight :float)
  (pTranslation :pointer)
  (pRotation :pointer))

(defcfun ("CalBone_CalculateState" CalBone_CalculateState) :void
  (self :pointer))

(defcfun ("CalBone_ClearState" CalBone_ClearState) :void
  (self :pointer))

(defcfun ("CalBone_Delete" CalBone_Delete) :void
  (self :pointer))

(defcfun ("CalBone_GetCoreBone" CalBone_GetCoreBone) :pointer
  (self :pointer))

(defcfun ("CalBone_GetRotation" CalBone_GetRotation) :pointer
  (self :pointer))

(defcfun ("CalBone_GetRotationAbsolute" CalBone_GetRotationAbsolute) :pointer
  (self :pointer))

(defcfun ("CalBone_GetRotationBoneSpace" CalBone_GetRotationBoneSpace) :pointer
  (self :pointer))

(defcfun ("CalBone_GetTranslation" CalBone_GetTranslation) :pointer
  (self :pointer))

(defcfun ("CalBone_GetTranslationAbsolute" CalBone_GetTranslationAbsolute) :pointer
  (self :pointer))

(defcfun ("CalBone_GetTranslationBoneSpace" CalBone_GetTranslationBoneSpace) :pointer
  (self :pointer))

(defcfun ("CalBone_LockState" CalBone_LockState) :void
  (self :pointer))

(defcfun ("CalBone_New" CalBone_New) :pointer
  (coreBone :pointer))

(defcfun ("CalBone_SetSkeleton" CalBone_SetSkeleton) :void
  (self :pointer)
  (pSkeleton :pointer))

(defcfun ("CalBone_SetTranslation" CalBone_SetTranslation) :void
  (self :pointer)
  (pTranslation :pointer))

(defcfun ("CalBone_SetRotation" CalBone_SetRotation) :void
  (self :pointer)
  (pRotation :pointer))

(defcfun ("CalBone_SetCoreState" CalBone_SetCoreState) :void
  (self :pointer))

(defcfun ("CalBone_SetCoreStateRecursive" CalBone_SetCoreStateRecursive) :void
  (self :pointer))

(defcfun ("CalCoreAnimation_AddCoreTrack" CalCoreAnimation_AddCoreTrack) :void
  (self :pointer)
  (pCoreTrack :pointer))

(defcfun ("CalCoreAnimation_Delete" CalCoreAnimation_Delete) :void
  (self :pointer))

(defcfun ("CalCoreAnimation_GetCoreTrack" CalCoreAnimation_GetCoreTrack) :pointer
  (self :pointer)
  (coreBoneId :int))

(defcfun ("CalCoreAnimation_GetDuration" CalCoreAnimation_GetDuration) :float
  (self :pointer))

(defcfun ("CalCoreAnimation_SetDuration" CalCoreAnimation_SetDuration) :void
  (self :pointer)
  (duration :float))

(defcfun ("CalCoreBone_AddChildId" CalCoreBone_AddChildId) :int
  (self :pointer)
  (childId :int))

(defcfun ("CalCoreBone_CalculateState" CalCoreBone_CalculateState) :void
  (self :pointer))

(defcfun ("CalCoreBone_Delete" CalCoreBone_Delete) :void
  (self :pointer))

(defcfun ("CalCoreBone_GetName" CalCoreBone_GetName) :string
  (self :pointer))

(defcfun ("CalCoreBone_GetParentId" CalCoreBone_GetParentId) :int
  (self :pointer))

(defcfun ("CalCoreBone_GetRotation" CalCoreBone_GetRotation) :pointer
  (self :pointer))

(defcfun ("CalCoreBone_GetRotationAbsolute" CalCoreBone_GetRotationAbsolute) :pointer
  (self :pointer))

(defcfun ("CalCoreBone_GetRotationBoneSpace" CalCoreBone_GetRotationBoneSpace) :pointer
  (self :pointer))

(defcfun ("CalCoreBone_GetTranslation" CalCoreBone_GetTranslation) :pointer
  (self :pointer))

(defcfun ("CalCoreBone_GetTranslationAbsolute" CalCoreBone_GetTranslationAbsolute) :pointer
  (self :pointer))

(defcfun ("CalCoreBone_GetTranslationBoneSpace" CalCoreBone_GetTranslationBoneSpace) :pointer
  (self :pointer))

(defcfun ("CalCoreBone_GetUserData" CalCoreBone_GetUserData) :pointer
  (self :pointer))

(defcfun ("CalCoreBone_New" CalCoreBone_New) :pointer
  (name :string))

(defcfun ("CalCoreBone_SetCoreSkeleton" CalCoreBone_SetCoreSkeleton) :void
  (self :pointer)
  (pCoreSkeleton :pointer))

(defcfun ("CalCoreBone_SetParentId" CalCoreBone_SetParentId) :void
  (self :pointer)
  (parentId :int))

(defcfun ("CalCoreBone_SetRotation" CalCoreBone_SetRotation) :void
  (self :pointer)
  (pRotation :pointer))

(defcfun ("CalCoreBone_SetRotationBoneSpace" CalCoreBone_SetRotationBoneSpace) :void
  (self :pointer)
  (pRotation :pointer))

(defcfun ("CalCoreBone_SetTranslation" CalCoreBone_SetTranslation) :void
  (self :pointer)
  (pTranslation :pointer))

(defcfun ("CalCoreBone_SetTranslationBoneSpace" CalCoreBone_SetTranslationBoneSpace) :void
  (self :pointer)
  (pTranslation :pointer))

(defcfun ("CalCoreBone_SetUserData" CalCoreBone_SetUserData) :void
  (self :pointer)
  (userData :pointer))

(defcfun ("CalCoreKeyframe_Delete" CalCoreKeyframe_Delete) :void
  (self :pointer))

(defcfun ("CalCoreKeyframe_GetRotation" CalCoreKeyframe_GetRotation) :pointer
  (self :pointer))

(defcfun ("CalCoreKeyframe_GetTime" CalCoreKeyframe_GetTime) :float
  (self :pointer))

(defcfun ("CalCoreKeyframe_GetTranslation" CalCoreKeyframe_GetTranslation) :pointer
  (self :pointer))

(defcfun ("CalCoreKeyframe_New" CalCoreKeyframe_New) :pointer)

(defcfun ("CalCoreKeyframe_SetRotation" CalCoreKeyframe_SetRotation) :void
  (self :pointer)
  (pRotation :pointer))

(defcfun ("CalCoreKeyframe_SetTime" CalCoreKeyframe_SetTime) :void
  (self :pointer)
  (time :float))

(defcfun ("CalCoreKeyframe_SetTranslation" CalCoreKeyframe_SetTranslation) :void
  (self :pointer)
  (pTranslation :pointer))

(defcfun ("CalCoreMaterial_Delete" CalCoreMaterial_Delete) :void
  (self :pointer))

(defcfun ("CalCoreMaterial_GetMapCount" CalCoreMaterial_GetMapCount) :int
  (self :pointer))

(defcfun ("CalCoreMaterial_GetMapFilename" CalCoreMaterial_GetMapFilename) :string
  (self :pointer)
  (mapId :int))

(defcfun ("CalCoreMaterial_GetMapUserData" CalCoreMaterial_GetMapUserData) :pointer
  (self :pointer)
  (mapId :int))

(defcfun ("CalCoreMaterial_GetShininess" CalCoreMaterial_GetShininess) :float
  (self :pointer))

(defcfun ("CalCoreMaterial_GetUserData" CalCoreMaterial_GetUserData) :pointer
  (self :pointer))

(defcfun ("CalCoreMaterial_New" CalCoreMaterial_New) :pointer)

(defcfun ("CalCoreMaterial_Reserve" CalCoreMaterial_Reserve) :int
  (self :pointer)
  (mapCount :int))

(defcfun ("CalCoreMaterial_SetMapUserData" CalCoreMaterial_SetMapUserData) :int
  (self :pointer)
  (mapId :int)
  (userData :pointer))

(defcfun ("CalCoreMaterial_SetShininess" CalCoreMaterial_SetShininess) :void
  (self :pointer)
  (shininess :float))

(defcfun ("CalCoreMaterial_SetUserData" CalCoreMaterial_SetUserData) :void
  (self :pointer)
  (userData :pointer))

(defcfun ("CalCoreMesh_AddCoreSubmesh" CalCoreMesh_AddCoreSubmesh) :int
  (self :pointer)
  (pCoreSubmesh :pointer))

(defcfun ("CalCoreMesh_Delete" CalCoreMesh_Delete) :void
  (self :pointer))

(defcfun ("CalCoreMesh_GetCoreSubmesh" CalCoreMesh_GetCoreSubmesh) :pointer
  (self :pointer)
  (id :int))

(defcfun ("CalCoreMesh_GetCoreSubmeshCount" CalCoreMesh_GetCoreSubmeshCount) :int
  (self :pointer))

(defcfun ("CalCoreMesh_New" CalCoreMesh_New) :pointer)

(defcfun ("CalCoreModel_AddCoreAnimation" CalCoreModel_AddCoreAnimation) :int
  (self :pointer)
  (pCoreAnimation :pointer))

(defcfun ("CalCoreModel_AddCoreMaterial" CalCoreModel_AddCoreMaterial) :int
  (self :pointer)
  (pCoreMaterial :pointer))

(defcfun ("CalCoreModel_AddCoreMesh" CalCoreModel_AddCoreMesh) :int
  (self :pointer)
  (pCoreMesh :pointer))

(defcfun ("CalCoreModel_CreateCoreMaterialThread" CalCoreModel_CreateCoreMaterialThread) :int
  (self :pointer)
  (coreMaterialThreadId :int))

(defcfun ("CalCoreModel_Delete" CalCoreModel_Delete) :void
  (self :pointer))

(defcfun ("CalCoreModel_GetCoreAnimation" CalCoreModel_GetCoreAnimation) :pointer
  (self :pointer)
  (coreAnimationId :int))

(defcfun ("CalCoreModel_GetCoreAnimationCount" CalCoreModel_GetCoreAnimationCount) :int
  (self :pointer))

(defcfun ("CalCoreModel_GetCoreMaterial" CalCoreModel_GetCoreMaterial) :pointer
  (self :pointer)
  (coreMaterialId :int))

(defcfun ("CalCoreModel_GetCoreMaterialCount" CalCoreModel_GetCoreMaterialCount) :int
  (self :pointer))

(defcfun ("CalCoreModel_GetCoreMaterialId" CalCoreModel_GetCoreMaterialId) :int
  (self :pointer)
  (coreMaterialThreadId :int)
  (coreMaterialSetId :int))

(defcfun ("CalCoreModel_GetCoreMesh" CalCoreModel_GetCoreMesh) :pointer
  (self :pointer)
  (coreMeshId :int))

(defcfun ("CalCoreModel_GetCoreMeshCount" CalCoreModel_GetCoreMeshCount) :int
  (self :pointer))

(defcfun ("CalCoreModel_GetCoreSkeleton" CalCoreModel_GetCoreSkeleton) :pointer
  (self :pointer))

(defcfun ("CalCoreModel_GetUserData" CalCoreModel_GetUserData) :pointer
  (self :pointer))

(defcfun ("CalCoreModel_LoadCoreAnimation" CalCoreModel_LoadCoreAnimation) :int
  (self :pointer)
  (strFilename :string))

(defcfun ("CalCoreModel_LoadCoreMaterial" CalCoreModel_LoadCoreMaterial) :int
  (self :pointer)
  (strFilename :string))

(defcfun ("CalCoreModel_LoadCoreMesh" CalCoreModel_LoadCoreMesh) :int
  (self :pointer)
  (strFilename :string))

(defcfun ("CalCoreModel_LoadCoreSkeleton" CalCoreModel_LoadCoreSkeleton) :int
  (self :pointer)
  (strFilename :string))

(defcfun ("CalCoreModel_New" CalCoreModel_New) :pointer
  (name :string))

(defcfun ("CalCoreModel_SaveCoreAnimation" CalCoreModel_SaveCoreAnimation) :int
  (self :pointer)
  (strFilename :string)
  (coreAnimtionId :int))

(defcfun ("CalCoreModel_SaveCoreMaterial" CalCoreModel_SaveCoreMaterial) :int
  (self :pointer)
  (strFilename :string)
  (coreMaterialId :int))

(defcfun ("CalCoreModel_SaveCoreMesh" CalCoreModel_SaveCoreMesh) :int
  (self :pointer)
  (strFilename :string)
  (coreMeshId :int))

(defcfun ("CalCoreModel_SaveCoreSkeleton" CalCoreModel_SaveCoreSkeleton) :int
  (self :pointer)
  (strFilename :string))

(defcfun ("CalCoreModel_SetCoreMaterialId" CalCoreModel_SetCoreMaterialId) :int
  (self :pointer)
  (coreMaterialThreadId :int)
  (coreMaterialSetId :int)
  (coreMaterialId :int))

(defcfun ("CalCoreModel_SetCoreSkeleton" CalCoreModel_SetCoreSkeleton) :void
  (self :pointer)
  (pCoreSkeleton :pointer))

(defcfun ("CalCoreModel_SetUserData" CalCoreModel_SetUserData) :void
  (self :pointer)
  (userData :pointer))

(defcfun ("CalCoreSkeleton_AddCoreBone" CalCoreSkeleton_AddCoreBone) :int
  (self :pointer)
  (pCoreBone :pointer))

(defcfun ("CalCoreSkeleton_CalculateState" CalCoreSkeleton_CalculateState) :void
  (self :pointer))

(defcfun ("CalCoreSkeleton_Delete" CalCoreSkeleton_Delete) :void
  (self :pointer))

(defcfun ("CalCoreSkeleton_GetCoreBone" CalCoreSkeleton_GetCoreBone) :pointer
  (self :pointer)
  (coreBoneId :int))

(defcfun ("CalCoreSkeleton_GetCoreBoneId" CalCoreSkeleton_GetCoreBoneId) :int
  (self :pointer)
  (strName :string))

(defcfun ("CalCoreSkeleton_New" CalCoreSkeleton_New) :pointer)

(defcfun ("CalCoreSubmesh_Delete" CalCoreSubmesh_Delete) :void
  (self :pointer))

(defcfun ("CalCoreSubmesh_GetCoreMaterialThreadId" CalCoreSubmesh_GetCoreMaterialThreadId) :int
  (self :pointer))

(defcfun ("CalCoreSubmesh_GetFaceCount" CalCoreSubmesh_GetFaceCount) :int
  (self :pointer))

(defcfun ("CalCoreSubmesh_GetLodCount" CalCoreSubmesh_GetLodCount) :int
  (self :pointer))

(defcfun ("CalCoreSubmesh_GetSpringCount" CalCoreSubmesh_GetSpringCount) :int
  (self :pointer))

(defcfun ("CalCoreSubmesh_GetVertexCount" CalCoreSubmesh_GetVertexCount) :int
  (self :pointer))

(defcfun ("CalCoreSubmesh_New" CalCoreSubmesh_New) :pointer)

(defcfun ("CalCoreSubmesh_Reserve" CalCoreSubmesh_Reserve) :int
  (self :pointer)
  (vertexCount :int)
  (textureCoordinateCount :int)
  (faceCount :int)
  (springCount :int))

(defcfun ("CalCoreSubmesh_SetCoreMaterialThreadId" CalCoreSubmesh_SetCoreMaterialThreadId) :void
  (self :pointer)
  (coreMaterialThreadId :int))

(defcfun ("CalCoreSubmesh_SetLodCount" CalCoreSubmesh_SetLodCount) :void
  (self :pointer)
  (lodCount :int))

(defcfun ("CalCoreSubmesh_IsTangentsEnabled" CalCoreSubmesh_IsTangentsEnabled) :int
  (self :pointer)
  (mapId :int))

(defcfun ("CalCoreSubmesh_EnableTangents" CalCoreSubmesh_EnableTangents) :int
  (self :pointer)
  (mapId :int)
  (enabled :pointer))

(defcfun ("CalCoreTrack_AddCoreKeyframe" CalCoreTrack_AddCoreKeyframe) :int
  (self :pointer)
  (pCoreKeyframe :pointer))

(defcfun ("CalCoreTrack_Delete" CalCoreTrack_Delete) :void
  (self :pointer))

(defcfun ("CalCoreTrack_GetCoreBoneId" CalCoreTrack_GetCoreBoneId) :int
  (self :pointer))

(defcfun ("CalCoreTrack_GetState" CalCoreTrack_GetState) :int
  (self :pointer)
  (time :float)
  (pTranslation :pointer)
  (pRotation :pointer))

(defcfun ("CalCoreTrack_New" CalCoreTrack_New) :pointer)

(defcfun ("CalCoreTrack_SetCoreBoneId" CalCoreTrack_SetCoreBoneId) :int
  (self :pointer)
  (coreBoneId :int))

(defcfun ("CalError_GetLastErrorCode" CalError_GetLastErrorCode) :int)

(defcfun ("CalError_GetLastErrorDescription" CalError_GetLastErrorDescription) :string)

(defcfun ("CalError_GetLastErrorFile" CalError_GetLastErrorFile) :string)

(defcfun ("CalError_GetLastErrorLine" CalError_GetLastErrorLine) :int)

(defcfun ("CalError_GetLastErrorText" CalError_GetLastErrorText) :string)

(defcfun ("CalError_PrintLastError" CalError_PrintLastError) :void)

(defcfun ("CalLoader_Delete" CalLoader_Delete) :void
  (self :pointer))

(defcfun ("CalLoader_LoadCoreAnimation" CalLoader_LoadCoreAnimation) :pointer
  (self :pointer)
  (strFilename :string))

(defcfun ("CalLoader_LoadCoreMaterial" CalLoader_LoadCoreMaterial) :pointer
  (self :pointer)
  (strFilename :string))

(defcfun ("CalLoader_LoadCoreMesh" CalLoader_LoadCoreMesh) :pointer
  (self :pointer)
  (strFilename :string))

(defcfun ("CalLoader_LoadCoreSkeleton" CalLoader_LoadCoreSkeleton) :pointer
  (self :pointer)
  (strFilename :string))

(defcfun ("CalLoader_New" CalLoader_New) :pointer)

(defcfun ("CalMesh_Delete" CalMesh_Delete) :void
  (self :pointer))

(defcfun ("CalMesh_GetCoreMesh" CalMesh_GetCoreMesh) :pointer
  (self :pointer))

(defcfun ("CalMesh_GetSubmesh" CalMesh_GetSubmesh) :pointer
  (self :pointer)
  (id :int))

(defcfun ("CalMesh_GetSubmeshCount" CalMesh_GetSubmeshCount) :int
  (self :pointer))

(defcfun ("CalMesh_New" CalMesh_New) :pointer
  (pCoreMesh :pointer))

(defcfun ("CalMesh_SetLodLevel" CalMesh_SetLodLevel) :void
  (self :pointer)
  (lodLevel :float))

(defcfun ("CalMesh_SetMaterialSet" CalMesh_SetMaterialSet) :void
  (self :pointer)
  (setId :int))

(defcfun ("CalMesh_SetModel" CalMesh_SetModel) :void
  (self :pointer)
  (pModel :pointer))

(defcfun ("CalMixer_BlendCycle" CalMixer_BlendCycle) :int
  (self :pointer)
  (id :int)
  (weight :float)
  (delay :float))

(defcfun ("CalMixer_ClearCycle" CalMixer_ClearCycle) :int
  (self :pointer)
  (id :int)
  (delay :float))

(defcfun ("CalMixer_Delete" CalMixer_Delete) :void
  (self :pointer))

(defcfun ("CalMixer_ExecuteAction" CalMixer_ExecuteAction) :int
  (self :pointer)
  (id :int)
  (delayIn :float)
  (delayOut :float))

(defcfun ("CalMixer_New" CalMixer_New) :pointer
  (pModel :pointer))

(defcfun ("CalMixer_UpdateAnimation" CalMixer_UpdateAnimation) :void
  (self :pointer)
  (deltaTime :float))

(defcfun ("CalMixer_UpdateSkeleton" CalMixer_UpdateSkeleton) :void
  (self :pointer))

(defcfun ("CalModel_AttachMesh" CalModel_AttachMesh) :int
  (self :pointer)
  (coreMeshId :int))

(defcfun ("CalModel_Delete" CalModel_Delete) :void
  (self :pointer))

(defcfun ("CalModel_DetachMesh" CalModel_DetachMesh) :int
  (self :pointer)
  (coreMeshId :int))

(defcfun ("CalModel_GetCoreModel" CalModel_GetCoreModel) :pointer
  (self :pointer))

(defcfun ("CalModel_GetMesh" CalModel_GetMesh) :pointer
  (self :pointer)
  (coreMeshId :int))

(defcfun ("CalModel_GetMixer" CalModel_GetMixer) :pointer
  (self :pointer))

(defcfun ("CalModel_GetPhysique" CalModel_GetPhysique) :pointer
  (self :pointer))

(defcfun ("CalModel_GetRenderer" CalModel_GetRenderer) :pointer
  (self :pointer))

(defcfun ("CalModel_GetSkeleton" CalModel_GetSkeleton) :pointer
  (self :pointer))

(defcfun ("CalModel_GetSpringSystem" CalModel_GetSpringSystem) :pointer
  (self :pointer))

(defcfun ("CalModel_GetUserData" CalModel_GetUserData) :pointer
  (self :pointer))

(defcfun ("CalModel_New" CalModel_New) :pointer
  (pCoreModel :pointer))

(defcfun ("CalModel_SetLodLevel" CalModel_SetLodLevel) :void
  (self :pointer)
  (lodLevel :float))

(defcfun ("CalModel_SetMaterialSet" CalModel_SetMaterialSet) :void
  (self :pointer)
  (setId :int))

(defcfun ("CalModel_SetUserData" CalModel_SetUserData) :void
  (self :pointer)
  (userData :pointer))

(defcfun ("CalModel_Update" CalModel_Update) :void
  (self :pointer)
  (deltaTime :float))

(defcfun ("CalPhysique_CalculateNormals" CalPhysique_CalculateNormals) :int
  (self :pointer)
  (pSubmesh :pointer)
  (pNormalBuffer :pointer))

(defcfun ("CalPhysique_CalculateVertices" CalPhysique_CalculateVertices) :int
  (self :pointer)
  (pSubmesh :pointer)
  (pVertexBuffer :pointer))

(defcfun ("CalPhysique_CalculateVerticesAndNormals" CalPhysique_CalculateVerticesAndNormals) :int
  (self :pointer)
  (pSubmesh :pointer)
  (pVertexBuffer :pointer))

(defcfun ("CalPhysique_CalculateVerticesNormalsAndTexCoords" CalPhysique_CalculateVerticesNormalsAndTexCoords) :int
  (self :pointer)
  (pSubmesh :pointer)
  (pVertexBuffer :pointer)
  (NumTexCoords :int))

(defcfun ("CalPhysique_CalculateTangentSpaces" CalPhysique_CalculateTangentSpaces) :int
  (self :pointer)
  (pSubmesh :pointer)
  (mapId :int)
  (pTangentSpaceBuffer :pointer))

(defcfun ("CalPhysique_Delete" CalPhysique_Delete) :void
  (self :pointer))

(defcfun ("CalPhysique_New" CalPhysique_New) :pointer
  (pModel :pointer))

(defcfun ("CalPhysique_Update" CalPhysique_Update) :void
  (self :pointer))

(defcfun ("CalQuaternion_Blend" CalQuaternion_Blend) :void
  (self :pointer)
  (d :float)
  (pQ :pointer))

(defcfun ("CalQuaternion_Clear" CalQuaternion_Clear) :void
  (self :pointer))

(defcfun ("CalQuaternion_Conjugate" CalQuaternion_Conjugate) :void
  (self :pointer))

(defcfun ("CalQuaternion_Delete" CalQuaternion_Delete) :void
  (self :pointer))

(defcfun ("CalQuaternion_Equal" CalQuaternion_Equal) :void
  (self :pointer)
  (pQ :pointer))

(defcfun ("CalQuaternion_Get" CalQuaternion_Get) :pointer
  (self :pointer))

(defcfun ("CalQuaternion_Multiply" CalQuaternion_Multiply) :void
  (self :pointer)
  (pQ :pointer))

(defcfun ("CalQuaternion_MultiplyVector" CalQuaternion_MultiplyVector) :void
  (self :pointer)
  (pV :pointer))

(defcfun ("CalQuaternion_New" CalQuaternion_New) :pointer)

(defcfun ("CalQuaternion_Op_Multiply" CalQuaternion_Op_Multiply) :void
  (pResult :pointer)
  (pQ :pointer)
  (pR :pointer))

(defcfun ("CalQuaternion_Set" CalQuaternion_Set) :void
  (self :pointer)
  (qx :float)
  (qy :float)
  (qz :float)
  (qw :float))

(defcfun ("CalRenderer_BeginRendering" CalRenderer_BeginRendering) :int
  (self :pointer))

(defcfun ("CalRenderer_Delete" CalRenderer_Delete) :void
  (self :pointer))

(defcfun ("CalRenderer_EndRendering" CalRenderer_EndRendering) :void
  (self :pointer))

(defcfun ("CalRenderer_GetAmbientColor" CalRenderer_GetAmbientColor) :void
  (self :pointer)
  (pColorBuffer :pointer))

(defcfun ("CalRenderer_GetDiffuseColor" CalRenderer_GetDiffuseColor) :void
  (self :pointer)
  (pColorBuffer :pointer))

(defcfun ("CalRenderer_GetFaceCount" CalRenderer_GetFaceCount) :int
  (self :pointer))

(defcfun ("CalRenderer_GetFaces" CalRenderer_GetFaces) :int
  (self :pointer)
  (pFaceBuffer :pointer))

(defcfun ("CalRenderer_GetMapCount" CalRenderer_GetMapCount) :int
  (self :pointer))

(defcfun ("CalRenderer_GetMapUserData" CalRenderer_GetMapUserData) :pointer
  (self :pointer)
  (mapId :int))

(defcfun ("CalRenderer_GetMeshCount" CalRenderer_GetMeshCount) :int
  (self :pointer))

(defcfun ("CalRenderer_GetNormals" CalRenderer_GetNormals) :int
  (self :pointer)
  (pNormalBuffer :pointer))

(defcfun ("CalRenderer_GetShininess" CalRenderer_GetShininess) :float
  (self :pointer))

(defcfun ("CalRenderer_GetSpecularColor" CalRenderer_GetSpecularColor) :void
  (self :pointer)
  (pColorBuffer :pointer))

(defcfun ("CalRenderer_GetSubmeshCount" CalRenderer_GetSubmeshCount) :int
  (self :pointer)
  (meshId :int))

(defcfun ("CalRenderer_GetTextureCoordinates" CalRenderer_GetTextureCoordinates) :int
  (self :pointer)
  (mapId :int)
  (pTextureCoordinateBuffer :pointer))

(defcfun ("CalRenderer_GetVertexCount" CalRenderer_GetVertexCount) :int
  (self :pointer))

(defcfun ("CalRenderer_GetVertices" CalRenderer_GetVertices) :int
  (self :pointer)
  (pVertexBuffer :pointer))

(defcfun ("CalRenderer_GetVerticesAndNormals" CalRenderer_GetVerticesAndNormals) :int
  (self :pointer)
  (pVertexBuffer :pointer))

(defcfun ("CalRenderer_GetVerticesNormalsAndTexCoords" CalRenderer_GetVerticesNormalsAndTexCoords) :int
  (self :pointer)
  (pVertexBuffer :pointer)
  (NumTexCoords :int))

(defcfun ("CalRenderer_GetTangentSpaces" CalRenderer_GetTangentSpaces) :int
  (self :pointer)
  (mapId :int)
  (pTangentSpaceBuffer :pointer))

(defcfun ("CalRenderer_IsTangentsEnabled" CalRenderer_IsTangentsEnabled) :int
  (self :pointer)
  (mapId :int))

(defcfun ("CalRenderer_New" CalRenderer_New) :pointer
  (pModel :pointer))

(defcfun ("CalRenderer_SelectMeshSubmesh" CalRenderer_SelectMeshSubmesh) :int
  (self :pointer)
  (meshId :int)
  (submeshId :int))

(defcfun ("CalSaver_Delete" CalSaver_Delete) :void
  (self :pointer))

(defcfun ("CalSaver_New" CalSaver_New) :pointer)

(defcfun ("CalSaver_SaveCoreAnimation" CalSaver_SaveCoreAnimation) :int
  (self :pointer)
  (strFilename :string)
  (pCoreAnimation :pointer))

(defcfun ("CalSaver_SaveCoreMaterial" CalSaver_SaveCoreMaterial) :int
  (self :pointer)
  (strFilename :string)
  (pCoreMaterial :pointer))

(defcfun ("CalSaver_SaveCoreMesh" CalSaver_SaveCoreMesh) :int
  (self :pointer)
  (strFilename :string)
  (pCoreMesh :pointer))

(defcfun ("CalSaver_SaveCoreSkeleton" CalSaver_SaveCoreSkeleton) :int
  (self :pointer)
  (strFilename :string)
  (pCoreSkeleton :pointer))

(defcfun ("CalSkeleton_CalculateState" CalSkeleton_CalculateState) :void
  (self :pointer))

(defcfun ("CalSkeleton_ClearState" CalSkeleton_ClearState) :void
  (self :pointer))

(defcfun ("CalSkeleton_Delete" CalSkeleton_Delete) :void
  (self :pointer))

(defcfun ("CalSkeleton_GetBone" CalSkeleton_GetBone) :pointer
  (self :pointer)
  (boneId :int))

(defcfun ("CalSkeleton_GetCoreSkeleton" CalSkeleton_GetCoreSkeleton) :pointer
  (self :pointer))

(defcfun ("CalSkeleton_LockState" CalSkeleton_LockState) :void
  (self :pointer))

(defcfun ("CalSkeleton_New" CalSkeleton_New) :pointer
  (pCoreSkeleton :pointer))

(defcfun ("CalSkeleton_GetBonePoints" CalSkeleton_GetBonePoints) :int
  (self :pointer)
  (pPoints :pointer))

(defcfun ("CalSkeleton_GetBonePointsStatic" CalSkeleton_GetBonePointsStatic) :int
  (self :pointer)
  (pPoints :pointer))

(defcfun ("CalSkeleton_GetBoneLines" CalSkeleton_GetBoneLines) :int
  (self :pointer)
  (pLines :pointer))

(defcfun ("CalSkeleton_GetBoneLinesStatic" CalSkeleton_GetBoneLinesStatic) :int
  (self :pointer)
  (pLines :pointer))

(defcfun ("CalSpringSystem_CalculateForces" CalSpringSystem_CalculateForces) :void
  (self :pointer)
  (pSubmesh :pointer)
  (deltaTime :float))

(defcfun ("CalSpringSystem_CalculateVertices" CalSpringSystem_CalculateVertices) :void
  (self :pointer)
  (pSubmesh :pointer)
  (deltaTime :float))

(defcfun ("CalSpringSystem_Delete" CalSpringSystem_Delete) :void
  (self :pointer))

(defcfun ("CalSpringSystem_New" CalSpringSystem_New) :pointer
  (pModel :pointer))

(defcfun ("CalSpringSystem_Update" CalSpringSystem_Update) :void
  (self :pointer)
  (deltaTime :float))

(defcfun ("CalSubmesh_Delete" CalSubmesh_Delete) :void
  (self :pointer))

(defcfun ("CalSubmesh_GetCoreSubmesh" CalSubmesh_GetCoreSubmesh) :pointer
  (self :pointer))

(defcfun ("CalSubmesh_GetCoreMaterialId" CalSubmesh_GetCoreMaterialId) :int
  (self :pointer))

(defcfun ("CalSubmesh_GetFaceCount" CalSubmesh_GetFaceCount) :int
  (self :pointer))

(defcfun ("CalSubmesh_GetFaces" CalSubmesh_GetFaces) :int
  (self :pointer)
  (pFaceBuffer :pointer))

(defcfun ("CalSubmesh_GetVertexCount" CalSubmesh_GetVertexCount) :int
  (self :pointer))

(defcfun ("CalSubmesh_HasInternalData" CalSubmesh_HasInternalData) :int
  (self :pointer))

(defcfun ("CalSubmesh_New" CalSubmesh_New) :pointer
  (coreSubmesh :pointer))

(defcfun ("CalSubmesh_SetCoreMaterialId" CalSubmesh_SetCoreMaterialId) :void
  (self :pointer)
  (coreMaterialId :int))

(defcfun ("CalSubmesh_SetLodLevel" CalSubmesh_SetLodLevel) :void
  (self :pointer)
  (lodLevel :float))

(defcfun ("CalVector_Add" CalVector_Add) :void
  (self :pointer)
  (pV :pointer))

(defcfun ("CalVector_Blend" CalVector_Blend) :void
  (self :pointer)
  (d :float)
  (pV :pointer))

(defcfun ("CalVector_Clear" CalVector_Clear) :void
  (self :pointer))

(defcfun ("CalVector_Delete" CalVector_Delete) :void
  (self :pointer))

(defcfun ("CalVector_Equal" CalVector_Equal) :void
  (self :pointer)
  (pV :pointer))

(defcfun ("CalVector_InverseScale" CalVector_InverseScale) :void
  (self :pointer)
  (d :float))

(defcfun ("CalVector_Get" CalVector_Get) :pointer
  (self :pointer))

(defcfun ("CalVector_Length" CalVector_Length) :float
  (self :pointer))

(defcfun ("CalVector_New" CalVector_New) :pointer)

(defcfun ("CalVector_Normalize" CalVector_Normalize) :float
  (self :pointer))

(defcfun ("CalVector_Op_Add" CalVector_Op_Add) :void
  (pResult :pointer)
  (pV :pointer)
  (pU :pointer))

(defcfun ("CalVector_Op_Subtract" CalVector_Op_Subtract) :void
  (pResult :pointer)
  (pV :pointer)
  (pU :pointer))

(defcfun ("CalVector_CalVector_Op_Scale" CalVector_CalVector_Op_Scale) :void
  (pResult :pointer)
  (pV :pointer)
  (d :float))

(defcfun ("CalVector_CalVector_Op_InverseScale" CalVector_CalVector_Op_InverseScale) :void
  (pResult :pointer)
  (pV :pointer)
  (d :float))

(defcfun ("CalVector_Op_Scalar" CalVector_Op_Scalar) :float
  (pV :pointer)
  (pU :pointer))

(defcfun ("CalVector_Op_Cross" CalVector_Op_Cross) :void
  (pResult :pointer)
  (pV :pointer)
  (pU :pointer))

(defcfun ("CalVector_Scale" CalVector_Scale) :void
  (self :pointer)
  (d :float))

(defcfun ("CalVector_Set" CalVector_Set) :void
  (self :pointer)
  (vx :float)
  (vy :float)
  (vz :float))

(defcfun ("CalVector_Subtract" CalVector_Subtract) :void
  (self :pointer)
  (pV :pointer))

(defcfun ("CalVector_Transform" CalVector_Transform) :void
  (self :pointer)
  (pQ :pointer))


