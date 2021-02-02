
--                                                                                                                                      --
--                                                         N E O  E N G I N E                                                           --
--                                                                                                                                      --
--                                                 Copyright (C) 2016 Justin Squirek                                                    --
--                                                                                                                                      --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the --
-- Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                    --
--                                                                                                                                      --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of                --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                            --
--                                                                                                                                      --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                       --
--                                                                                                                                      --    

package body Neo.World.Physics is

  ---------------------------
  -- Initialize_Simulation --
  ---------------------------
  
  procedure Initialize (gUsePVD, mUseGPU, mPCM, mPrecomputeActiveEdges,
                        mGaussMapLimit, mMeshCookingHint, mMidPhaseType,
                        mNbThreads, mStaticPruner, mDynamicPruner, mSQDynamicRebuildRateHint,
                        mEnableOneDirFriction, mEnableTwoDirFriction, mBroadPhaseType, mMaxNbCCDPasses)
    is
    PxTolerancesScale scale;
    begin
      gDefaultAllocator = new PEEL_PhysX3_AllocatorCallback;
      gDefaultErrorCallback = new PEEL_PhysX3_ErrorCallback;
      mFoundation = PxCreateFoundation(PX_FOUNDATION_VERSION, *gDefaultAllocator, *gDefaultErrorCallback);
      Assert (mFoundation);
      
      if gUsePVD then
        mProfileZoneManager = &PxProfileZoneManager::createProfileZoneManager(mFoundation);
        ASSERT(mProfileZoneManager);
      end if;
      
      mPhysics = PxCreatePhysics(PX_PHYSICS_VERSION, *mFoundation, scale, false, null);
      ASSERT(mPhysics);
      
      bool status = PxInitExtensions(*mPhysics, null);
      ASSERT(status);
      
      gDefaultCPUDispatcher = PxDefaultCpuDispatcherCreate(mParams.mNbThreads, null);
      
      -- (scale, PxMeshPreprocessingFlags(PxMeshPreprocessingFlag::eWELD_VERTICES));
      PxCookingParams Params(scale);
      Params.midphaseDesc.setToDefault(mParams.mMidPhaseType);
      Params.meshCookingHint = mParams.mMeshCookingHint;
      Params.meshPreprocessParams = mParams.mPrecomputeActiveEdges ? PxMeshPreprocessingFlags(0) : PxMeshPreprocessingFlag::eDISABLE_ACTIVE_EDGES_PRECOMPUTE; 
      if(mParams.mPCM) then
        Params.meshWeldTolerance = 0.001f;
        Params.meshPreprocessParams |= mesh_preprocess_params;
      end if;
      Params.gaussMapLimit = mParams.mGaussMapLimit;
      if(mParams.mUseGPU) then
         Params.BUILD_GPU_DATA = true;
      end if;
      mCooking = PxCreateCooking(PX_PHYSICS_VERSION, *mFoundation, Params);
      ASSERT(mCooking);
      if gUsePVD then
        gPVDHelper = new PVDHelper(mPhysics);
        gPVDHelper.togglePvdConnection();
        if(mPhysics.getPvdConnectionManager())
          mPhys>getPvdConnectionManager().addHandler(*gPVDHelper);
  end if;
      end if;
      
      PxSceneDesc sceneDesc(scale);
      sceneDesc.gravity  = ToPxVec3(desc.mGravity);
      
      --  #ifdef MODIFY_CONTACTS
      --      sceneDesc.contactModifyCallback    = &gContactModifyCallback;
      --      sceneDesc.filterShader        = ContactModifySimulationFilterShader;
      --  #else
      --      sceneDesc.filterShader        = mParams.mUseCCD ? CCDSimulationFilterShader : PxDefaultSimulationFilterShader;
      --    #ifdef SETUP_FILTERING
      --      sceneDesc.filterShader        = SampleVehicleFilterShader;
      --    #endif
      --  #endif
      
      sceneDesc.cpuDispatcher   = gDefaultCPUDispatcher;    
      sceneDesc.cpuDispatcher        = gDefaultCPUDispatcher;
      sceneDesc.staticStructure      = mParams.mStaticPruner;
      sceneDesc.dynamicStructure      = mParams.mDynamicPruner;
      sceneDesc.dynamicTreeRebuildRateHint= mParams.mSQDynamicRebuildRateHint;
      
      SetSceneFlag(sceneDesc, PxSceneFlag::eENABLE_PCM,        mParams.mPCM);
      SetSceneFlag(sceneDesc, PxSceneFlag::eADAPTIVE_FORCE,      mParams.mAdaptiveForce);
      SetSceneFlag(sceneDesc, PxSceneFlag::eENABLE_STABILIZATION,    mParams.mStabilization);
      SetSceneFlag(sceneDesc, PxSceneFlag::eENABLE_ACTIVETRANSFORMS,  mParams.mEnableActiveTransforms);
      SetSceneFlag(sceneDesc, PxSceneFlag::eDISABLE_CONTACT_CACHE,  !mParams.mEnableContactCache); 
      SetSceneFlag(sceneDesc, PxSceneFlag::eENABLE_CCD,        mParams.mUseCCD);
      
      if(mParams.mEnableOneDirFriction)
        sceneDesc.frictionType = PxFrictionType::eONE_DIRECTIONAL;
      end if;
      if(mParams.mEnableTwoDirFriction)
        sceneDesc.frictionType = PxFrictionType::eTWO_DIRECTIONAL;
      end if;
      
      sceneDesc.broadPhaseType = mParams.mBroadPhaseType;
      sceneDesc.ccdMaxPasses = mParams.mMaxNbCCDPasses;

      if(mParams.mUseGPU) then
        printf("Using GPU\n");
        sceneDesc.flags |= PxSceneFlag::eENABLE_GPU_DYNAMICS;
        sceneDesc.broadPhaseType = PxBroadPhaseType::eGPU;
        sceneDesc.gpuMaxNumPartitions = 8;
        sceneDesc.gpuDynamicsConfig.constraintBufferCapacity *= 2;
        sceneDesc.gpuDynamicsConfig.contactBufferCapacity *= 2;
        sceneDesc.gpuDynamicsConfig.tempBufferCapacity *= 2;
        sceneDesc.gpuDynamicsConfig.contactStreamSize *= 2;
        sceneDesc.gpuDynamicsConfig.patchStreamSize *= 2;
        sceneDesc.gpuDynamicsConfig.forceStreamCapacity *= 2;
        sceneDesc.gpuDynamicsConfig.heapCapacity *= 2;
        sceneDesc.gpuDynamicsConfig.foundLostPairsCapacity *= 2;
       
        PxCudaContextManagerDesc cudaContextManagerDesc;
        cudaContextManagerDesc.interopMode = PxCudaInteropMode::OGL_INTEROP;
        gCudaContextManager = PxCreateCudaContextManager(*mFoundation, cudaContextManagerDesc);
        
        if(gCudaContextManager) then
          if(!gCudaContextManager.contextIsValid()) then
            gCudaContextManager.release();
            gCudaContextManager = NULL;
          end if;
        end if;
        
        if(gCudaContextManager)
          sceneDesc.gpuDispatcher = gCudaContextManager.getGpuDispatcher(); --Set the GPU dispatcher, used by GRB to dispatch CUDA kernels.
        end if;
      end if;
      
      mScene = mPhysics.createScene(sceneDesc);
      ASSERT(mScene);
      
      if(mParams.mBroadPhaseType==PxBroadPhaseType::eMBP) then
        PxVec3 min, max;
        
        if(desc.mGlobalBounds.IsValid()) then
          min.x = desc.mGlobalBounds.GetMin(0);
          min.y = desc.mGlobalBounds.GetMin(1);
          min.z = desc.mGlobalBounds.GetMin(2);
          max.x = desc.mGlobalBounds.GetMax(0);
          max.y = desc.mGlobalBounds.GetMax(1);
          max.z = desc.mGlobalBounds.GetMax(2);
        else
          min = PxVec3(-mParams.mMBPRange);
          max = PxVec3(mParams.mMBPRange);
        end if;
        
        const PxBounds3 globalBounds(min, max);

        PxBounds3 regions[256];
        const PxU32 nbRegions = PxBroadPhaseExt::createRegionsFromWorldBounds(regions, globalBounds, mParams.mMBPSubdivLevel);
        for(PxU32 i=0;i<nbRegions;i++) loop
          PxBroadPhaseRegion region;
          region.bounds = regions[i];
          region.userData = (void*)i;
          mScene.addBroadPhaseRegion(region);
        end loop;

        mScene.setBroadPhaseCallback(&gBroadPhaseCallback);
      end if;

      PINT_MATERIAL_CREATE Desc;
      Desc.mStaticFriction  = mParams.mDefaultFriction;
      Desc.mDynamicFriction  = mParams.mDefaultFriction;
      Desc.mRestitution        = 0.0f;
      mDefaultMaterial = CreateMaterial(Desc);
      ASSERT(mDefaultMaterial);
    end;
    
  ----------------
  -- Make_Joint --
  ----------------

  procedure Make_Joint (PxPhysics& physics, const PINT_JOINT_CREATE& desc, bool enable_collision_between_jointed, const EditableParams& params)
    const bool use_d6_joint = params.mUseD6Joint;
    const bool enable_projection = params.mEnableJointProjection;
    const float projection_linear_tolerance = params.mProjectionLinearTolerance;
    const float projection_angular_tolerance = params.mProjectionAngularTolerance * DEGTORAD;
    PxRigidActor* actor0 = (PxRigidActor*)desc.mObject0;
    PxRigidActor* actor1 = (PxRigidActor*)desc.mObject1;
    PxJoint* CreatedJoint = null;
    begin
      case Joint.Kind is
      
        when Spherical_Joint =>
          const PINT_SPHERICAL_JOINT_CREATE& jc = static_cast<const PINT_SPHERICAL_JOINT_CREATE&>(desc);
          if(use_d6_joint)
            PxD6Joint* j = PxD6JointCreate(physics, actor0, PxTransform(ToPxVec3(jc.mLocalPivot0)), actor1, PxTransform(ToPxVec3(jc.mLocalPivot1)));
            ASSERT(j);
            CreatedJoint = j;    
            j.setMotion(PxD6Axis::eTWIST, PxD6Motion::eFREE);
            j.setMotion(PxD6Axis::eSWING1, PxD6Motion::eFREE);
            j.setMotion(PxD6Axis::eSWING2, PxD6Motion::eFREE);    
            SetupD6Projection(j, enable_projection, projection_linear_tolerance, projection_angular_tolerance);
          else
            PxSphericalJoint* j = PxSphericalJointCreate(physics, actor0, PxTransform(ToPxVec3(jc.mLocalPivot0)), actor1, PxTransform(ToPxVec3(jc.mLocalPivot1)));
            ASSERT(j);
            CreatedJoint = j;    
            if(enable_projection)
              -- Angular tolerance not used for spherical joints
              j.setProjectionLinearTolerance(projection_linear_tolerance);
            end if;
          end if;
          
        when Hinge_Joint => 
          const PINT_HINGE_JOINT_CREATE& jc = static_cast<const PINT_HINGE_JOINT_CREATE&>(desc);
          const PxQuat q0 = ComputeJointQuat(actor0, ToPxVec3(jc.mLocalAxis0));
          const PxQuat q1 = ComputeJointQuat(actor1, ToPxVec3(jc.mLocalAxis1));
          if(use_d6_joint)
            PxD6Joint* j = PxD6JointCreate(physics, actor0, PxTransform(ToPxVec3(jc.mLocalPivot0), q0), actor1, PxTransform(ToPxVec3(jc.mLocalPivot1), q1));
            ASSERT(j);
            CreatedJoint = j;
            j.setMotion(PxD6Axis::eTWIST, PxD6Motion::eFREE);
            if(jc.mMinLimitAngle!=MIN_FLOAT || jc.mMaxLimitAngle!=MAX_FLOAT) then
              const float limitContactDistance = 0.05f;
              PxJointAngularLimitPair limit(0.0f, 0.0f, limitContactDistance);
              limit.lower      = -jc.mMaxLimitAngle;
              limit.upper      = -jc.mMinLimitAngle;
              limit.lower      = jc.mMinLimitAngle;
              limit.upper      = jc.mMaxLimitAngle;
              j.setTwistLimit(limit);
              j.setMotion(PxD6Axis::eTWIST, PxD6Motion::eLIMITED);
            end if;    
            SetupD6Projection(j, enable_projection, projection_linear_tolerance, projection_angular_tolerance);
          else
            PxRevoluteJoint* j = PxRevoluteJointCreate(physics,  actor0, PxTransform(ToPxVec3(jc.mLocalPivot0), q0), actor1, PxTransform(ToPxVec3(jc.mLocalPivot1), q1));
            ASSERT(j);
            CreatedJoint = j;
            if(jc.mMinLimitAngle!=MIN_FLOAT || jc.mMaxLimitAngle!=MAX_FLOAT)
              const float limitContactDistance = 0.05f;
              PxJointAngularLimitPair limit(0.0f, 0.0f, limitContactDistance);
              limit.lower      = -jc.mMaxLimitAngle;
              limit.upper      = -jc.mMinLimitAngle;
              limit.lower      = jc.mMinLimitAngle;
              limit.upper      = jc.mMaxLimitAngle;
              j.setLimit(limit);
              j.setRevoluteJointFlag(PxRevoluteJointFlag::eLIMIT_ENABLED, true);
            end if;
            if(enable_projection)
              j.setProjectionLinearTolerance(projection_linear_tolerance);
              j.setProjectionAngularTolerance(projection_angular_tolerance);
            end if;
          end if;
          
        when Fixed_Joint =>
        when Prismatic_Joint =>
        when Distance_Joint =>
      end case;
    
{

//  projection_angular_tolerance *= DEGTORAD;


  switch(desc.mType)
  {
    case PINT_JOINT_SPHERICAL:
    {
    }
    break;

    case PINT_JOINT_HINGE:
    {
     
    }
    break;

    case PINT_JOINT_FIXED:
    {
      const PINT_FIXED_JOINT_CREATE& jc = static_cast<const PINT_FIXED_JOINT_CREATE&>(desc);

      if(use_d6_joint)
      {
        PxD6Joint* j = PxD6JointCreate(physics, actor0, PxTransform(ToPxVec3(jc.mLocalPivot0)), actor1, PxTransform(ToPxVec3(jc.mLocalPivot1)));
        ASSERT(j);
        CreatedJoint = j;

        SetupD6Projection(j, enable_projection, projection_linear_tolerance, projection_angular_tolerance);
      }
      else
      {
        PxFixedJoint* j = PxFixedJointCreate(physics, actor0, PxTransform(ToPxVec3(jc.mLocalPivot0)), actor1, PxTransform(ToPxVec3(jc.mLocalPivot1)));
        ASSERT(j);
        CreatedJoint = j;

        if(enable_projection)
        {
          j.setProjectionLinearTolerance(projection_linear_tolerance);
          j.setProjectionAngularTolerance(projection_angular_tolerance);
        }
      }
    }
    break;

    case PINT_JOINT_PRISMATIC:
    {
      const PINT_PRISMATIC_JOINT_CREATE& jc = static_cast<const PINT_PRISMATIC_JOINT_CREATE&>(desc);

      const PxQuat q0 = ComputeJointQuat(actor0, ToPxVec3(jc.mLocalAxis0));
      const PxQuat q1 = ComputeJointQuat(actor1, ToPxVec3(jc.mLocalAxis1));

      if(0 && use_d6_joint)
      {
        PxD6Joint* j = PxD6JointCreate(physics, actor0, PxTransform(ToPxVec3(jc.mLocalPivot0), q0), actor1, PxTransform(ToPxVec3(jc.mLocalPivot1), q1));
        ASSERT(j);
        CreatedJoint = j;

        j.setMotion(PxD6Axis::eX, PxD6Motion::eFREE);


        SetupD6Projection(j, enable_projection, projection_linear_tolerance, projection_angular_tolerance);
      }
      else
      {
        PxPrismaticJoint* j = PxPrismaticJointCreate(physics,  actor0, PxTransform(ToPxVec3(jc.mLocalPivot0), q0),
                                    actor1, PxTransform(ToPxVec3(jc.mLocalPivot1), q1));
        ASSERT(j);
        CreatedJoint = j;

        if(jc.mMinLimit<=jc.mMaxLimit)
        {

          const PxJointLinearLimitPair Limits(jc.mMinLimit, jc.mMaxLimit, PxSpring(jc.mSpringStiffness, jc.mSpringDamping));
          j.setLimit(Limits);
          j.setPrismaticJointFlag(PxPrismaticJointFlag::eLIMIT_ENABLED, true);
        }
        else
          j.setPrismaticJointFlag(PxPrismaticJointFlag::eLIMIT_ENABLED, false);

        if(enable_projection)
        {
          j.setProjectionLinearTolerance(projection_linear_tolerance);
          j.setProjectionAngularTolerance(projection_angular_tolerance);
        }
      }
    }
    break;

    case PINT_JOINT_DISTANCE:
    {
      const PINT_DISTANCE_JOINT_CREATE& jc = static_cast<const PINT_DISTANCE_JOINT_CREATE&>(desc);

      PxDistanceJoint* j = PxDistanceJointCreate(physics, actor0, PxTransform(ToPxVec3(jc.mLocalPivot0)), actor1, PxTransform(ToPxVec3(jc.mLocalPivot1)));
      ASSERT(j);
      CreatedJoint = j;
      if(jc.mMaxDistance>=0.0f)
      {
        j.setMaxDistance(jc.mMaxDistance);
        j.setDistanceJointFlag(PxDistanceJointFlag::eMAX_DISTANCE_ENABLED, true);
      }
      else j.setDistanceJointFlag(PxDistanceJointFlag::eMAX_DISTANCE_ENABLED, false);

      if(jc.mMinDistance>=0.0f)
      {
        j.setMinDistance(jc.mMinDistance);
        j.setDistanceJointFlag(PxDistanceJointFlag::eMIN_DISTANCE_ENABLED, true);
      }
      else j.setDistanceJointFlag(PxDistanceJointFlag::eMIN_DISTANCE_ENABLED, false);

/*      if(enable_projection)
      {
        j.setProjectionLinearTolerance(projection_linear_tolerance);
        j.setProjectionAngularTolerance(projection_angular_tolerance);
      }*/
//      j.setTolerance(projection_linear_tolerance);
    }
    break;
  }

  if(CreatedJoint)
  {
    if(enable_collision_between_jointed)
      CreatedJoint.setConstraintFlags(PxConstraintFlag::eCOLLISION_ENABLED);

    CreatedJoint.setConstraintFlag(PxConstraintFlag::eVISUALIZATION, true);
    CreatedJoint.setConstraintFlag(PxConstraintFlag::ePROJECTION, enable_projection);
#ifdef PHYSX_SUPPORT_DISABLE_PREPROCESSING
    const bool disable_preprocessing = params.mDisablePreprocessing;
    CreatedJoint.setConstraintFlag(PxConstraintFlag::eDISABLE_PREPROCESSING, disable_preprocessing);
#endif
#ifndef IS_PHYSX_3_2
    const float inverse_inertia_scale = params.mInverseInertiaScale;
    const float inverse_mass_scale = params.mInverseMassScale;

  #ifndef PHYSX_REMOVE_JOINT_32_COMPATIBILITY
    const bool enable_32_compatibility = params.mEnableJoint32Compatibility;
    CreatedJoint.setConstraintFlag(PxConstraintFlag::eDEPRECATED_32_COMPATIBILITY, enable_32_compatibility);
  #endif
    if(inverse_inertia_scale>=0.0f)
    {
      CreatedJoint.setInvInertiaScale0(inverse_inertia_scale);
      CreatedJoint.setInvInertiaScale1(inverse_inertia_scale);
    }

    if(inverse_mass_scale>=0.0f)
    {
      CreatedJoint.setInvMassScale0(inverse_mass_scale);
      CreatedJoint.setInvMassScale1(inverse_mass_scale);
    }
#endif
    // - Stabilization can create artefacts on jointed objects so we just disable it
    // - Ropes and thin cables can go through each other easily if we limit their depenetration velocity. So we just disable
    // this feature for jointed & articulated objects.
    if(actor0 && actor0.getConcreteType()==PxConcreteType::eRIGID_DYNAMIC)
    {
      PxRigidDynamic* RD = static_cast<PxRigidDynamic*>(actor0);
#ifdef PHYSX_SUPPORT_STABILIZATION_FLAG
      RD.setStabilizationThreshold(0.0f);
      RD.setMaxDepenetrationVelocity(MAX_FLOAT);
#endif
    }
    if(actor1 && actor1.getConcreteType()==PxConcreteType::eRIGID_DYNAMIC)
    {
      PxRigidDynamic* RD = static_cast<PxRigidDynamic*>(actor1);
#ifdef PHYSX_SUPPORT_STABILIZATION_FLAG
      RD.setStabilizationThreshold(0.0f);
      RD.setMaxDepenetrationVelocity(MAX_FLOAT);
#endif
    }
  }
  return CreatedJoint;
}
  --------------------
  -- Run_Simulation --
  --------------------
  
  procedure Run_Simulation (float dt) is
    begin
      for I in 1..PhysX_Steps.Get loop
        for Torque of Torques loop
          PxRigidBody* RigidBody = GetRigidBody(Current.mHandle);
          const PxVec3 GlobalTorque = RigidBody.getGlobalPose().rotate(ToPxVec3(Current.mLocalTorque));
          RigidBody.addTorque(GlobalTorque, PxForceMode::eACCELERATION, true);
        end loop;
        mScene.simulate(sdt, null, GetScratchPad(), GetScratchPadSize());
        mScene.fetchResults(true);
      end loop;
      mLocalTorques.clear();
      if(gRaycastCCDManager)
        gRaycastCCDManager.doRaycastCCD(mParams.mUseRaycastCCD_DynaDyna);
      end if;
    end;
  

end;
