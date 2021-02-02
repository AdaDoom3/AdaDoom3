
--                                                                                                                               --
--                                                      N E O  E N G I N E                                                       --
--                                                                                                                               --
--                                               Copyright (C) 2020 Justin Squirek                                               --
--                                                                                                                               --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published --
-- by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.                      --
--                                                                                                                               --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of         --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                     --
--                                                                                                                               --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                --
--                                                                                                                               --

package Neo.API.PhysX is

  -----------
  -- Actor --
  -----------

   subtype PxDominanceGroup is pxsimpletypes_h.PxU8;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:57

   subtype Enum is unsigned;
   eVISUALIZATION : constant unsigned := 1;
   eDISABLE_GRAVITY : constant unsigned := 2;
   eSEND_SLEEP_NOTIFIES : constant unsigned := 4;
   eDISABLE_SIMULATION : constant unsigned := 8;
   type PxActorFlag is record
      null;
   end record
   with Convention => C_Pass_By_Copy;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:64

   subtype PxActorFlags is PxFlags_h.Class_PxFlags.PxFlags;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:112

   type Enum;
   function operator_a (a : Enum; b : Enum) return PxFlags_h.Class_PxFlags.PxFlags  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:113
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physxanENS_11PxActorFlag4EnumES1_";

   function operator_ti (a : Enum) return PxFlags_h.Class_PxFlags.PxFlags  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:113
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physxcoENS_11PxActorFlag4EnumE";

   function operator_o (a : Enum; b : Enum) return PxFlags_h.Class_PxFlags.PxFlags  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:113
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physxorENS_11PxActorFlag4EnumES1_";

   subtype Enum is unsigned;
   eRIGID_STATIC : constant unsigned := 0;
   eRIGID_DYNAMIC : constant unsigned := 1;
   eARTICULATION_LINK : constant unsigned := 2;
   eACTOR_COUNT : constant unsigned := 3;
   eACTOR_FORCE_DWORD : constant unsigned := 2147483647;
   type PxActorType is record
      null;
   end record
   with Convention => C_Pass_By_Copy;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:119

   package Class_PxActor is
      type PxActor is abstract limited new PxBase_h.Class_PxBase.PxBase with record
         userData : System.Address;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:314
      end record
      with Import => True,
           Convention => CPP;

      procedure release (this : access PxActor) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:166

      function getType (this : access constant PxActor) return Enum is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:175

      function getScene (this : access constant PxActor) return access PxPhysXConfig_h.PxScene is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:184

      procedure setName (this : access PxActor; name : Interfaces.C.Strings.chars_ptr) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:200

      function getName (this : access constant PxActor) return Interfaces.C.Strings.chars_ptr is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:209

      function getWorldBounds (this : access constant PxActor; inflation : float) return PxBounds3_h.Class_PxBounds3.PxBounds3 is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:220

      procedure setActorFlag
        (this : access PxActor;
         flag : Enum;
         value : Extensions.bool) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:236

      procedure setActorFlags (this : access PxActor; inFlags : PxActorFlags) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:243

      function getActorFlags (this : access constant PxActor) return PxActorFlags is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:254

      procedure setDominanceGroup (this : access PxActor; dominanceGroup : PxDominanceGroup) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:272

      function getDominanceGroup (this : access constant PxActor) return PxDominanceGroup is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:281

      procedure setOwnerClient (this : access PxActor; inClient : PxClient_h.PxClientID) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:293

      function getOwnerClient (this : access constant PxActor) return PxClient_h.PxClientID is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:302

      function getAggregate (this : access constant PxActor) return access PxPhysXConfig_h.PxAggregate is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:311

      function New_PxActor (concreteType : PxBase_h.PxType; baseFlags : access PxBase_h.Class_PxBaseFlags.PxBaseFlags) return PxActor is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:317
      pragma CPP_Constructor (New_PxActor, "_ZN5physx7PxActorC1EtNS_7PxFlagsINS_10PxBaseFlag4EnumEtEE");

      function New_PxActor (baseFlags : access PxBase_h.Class_PxBaseFlags.PxBaseFlags) return PxActor is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:318
      pragma CPP_Constructor (New_PxActor, "_ZN5physx7PxActorC1ENS_7PxFlagsINS_10PxBaseFlag4EnumEtEE");

      procedure Delete_PxActor (this : access PxActor)  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:319
      with Import => True,
           Convention => CPP,
           External_Name => "_ZN5physx7PxActorD1Ev";

      procedure Delete_And_Free_PxActor (this : access PxActor)  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:319
      with Import => True,
           Convention => CPP,
           External_Name => "_ZN5physx7PxActorD0Ev";

      function isKindOf (this : access constant PxActor; name : Interfaces.C.Strings.chars_ptr) return Extensions.bool  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxActor.h:320
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx7PxActor8isKindOfEPKc";
   end;
   use Class_PxActor;

  ----------
  -- Math --
  ----------

   PxPi : aliased constant float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:58
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physxL4PxPiE";

   PxHalfPi : aliased constant float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:59
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physxL8PxHalfPiE";

   PxTwoPi : aliased constant float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:60
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physxL7PxTwoPiE";

   PxInvPi : aliased constant float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:61
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physxL7PxInvPiE";

   PxInvTwoPi : aliased constant float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:62
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physxL10PxInvTwoPiE";

   PxPiDivTwo : aliased constant float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:63
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physxL10PxPiDivTwoE";

   PxPiDivFour : aliased constant float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:64
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physxL11PxPiDivFourE";

   function PxAbs (a : float) return float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:107
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx5PxAbsEf";

   function PxEquals
     (a : float;
      b : float;
      eps : float) return Extensions.bool  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:112
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx8PxEqualsEfff";

   function PxAbs (a : double) return double  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:120
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx5PxAbsEd";

   function PxAbs (a : stdint_h.int32_t) return stdint_h.int32_t  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:128
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx5PxAbsEi";

   function PxSqrt (a : float) return float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:144
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx6PxSqrtEf";

   function PxSqrt (a : double) return double  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:150
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx6PxSqrtEd";

   function PxRecipSqrt (a : float) return float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:156
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx11PxRecipSqrtEf";

   function PxRecipSqrt (a : double) return double  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:162
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx11PxRecipSqrtEd";

   function PxSin (a : float) return float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:170
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx5PxSinEf";

   function PxSin (a : double) return double  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:176
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx5PxSinEd";

   function PxCos (a : float) return float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:182
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx5PxCosEf";

   function PxCos (a : double) return double  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:188
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx5PxCosEd";

   function PxTan (a : float) return float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:197
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx5PxTanEf";

   function PxTan (a : double) return double  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:206
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx5PxTanEd";

   function PxAsin (f : float) return float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:216
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx6PxAsinEf";

   function PxAsin (f : double) return double  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:226
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx6PxAsinEd";

   function PxAcos (f : float) return float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:236
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx6PxAcosEf";

   function PxAcos (f : double) return double  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:246
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx6PxAcosEd";

   function PxAtan (a : float) return float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:256
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx6PxAtanEf";

   function PxAtan (a : double) return double  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:266
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx6PxAtanEd";

   function PxAtan2 (x : float; y : float) return float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:276
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx7PxAtan2Eff";

   function PxAtan2 (x : double; y : double) return double  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:286
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx7PxAtan2Edd";

   function PxIsFinite (f : float) return Extensions.bool  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:292
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx10PxIsFiniteEf";

   function PxIsFinite (f : double) return Extensions.bool  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:298
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx10PxIsFiniteEd";

   function PxFloor (a : float) return float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:303
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx7PxFloorEf";

   function PxExp (a : float) return float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:308
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx5PxExpEf";

   function PxCeil (a : float) return float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:313
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx6PxCeilEf";

   function PxSign (a : float) return float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:318
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx6PxSignEf";

   function PxPow (x : float; y : float) return float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:323
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx5PxPowEff";

   function PxLog (x : float) return float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxmath.h:328
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx5PxLogEf";

  ---------------
  -- Transform --
  ---------------

    package Class_PxTransform is
      type PxTransform is limited record
         q : aliased pxquat_h.Class_PxQuat.PxQuat;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:51
         p : aliased pxvec3_h.Class_PxVec3.PxVec3;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:52
      end record
      with Import => True,
           Convention => CPP;

      function New_PxTransform return PxTransform;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:54
      pragma CPP_Constructor (New_PxTransform, "_ZN5physx11PxTransformC1Ev");

      function New_PxTransform (position : access constant pxvec3_h.Class_PxVec3.PxVec3) return PxTransform;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:58
      pragma CPP_Constructor (New_PxTransform, "_ZN5physx11PxTransformC1ERKNS_6PxVec3E");

      function New_PxTransform (r : px_h.PxIDENTITY) return PxTransform;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:62
      pragma CPP_Constructor (New_PxTransform, "_ZN5physx11PxTransformC1ENS_10PxIDENTITYE");

      function New_PxTransform (orientation : access constant pxquat_h.Class_PxQuat.PxQuat) return PxTransform;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:67
      pragma CPP_Constructor (New_PxTransform, "_ZN5physx11PxTransformC1ERKNS_6PxQuatE");

      function New_PxTransform
        (x : float;
         y : float;
         z : float;
         aQ : access pxquat_h.Class_PxQuat.PxQuat) return PxTransform;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:72
      pragma CPP_Constructor (New_PxTransform, "_ZN5physx11PxTransformC1EfffNS_6PxQuatE");

      function New_PxTransform (p0 : access constant pxvec3_h.Class_PxVec3.PxVec3; q0 : access constant pxquat_h.Class_PxQuat.PxQuat) return PxTransform;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:77
      pragma CPP_Constructor (New_PxTransform, "_ZN5physx11PxTransformC1ERKNS_6PxVec3ERKNS_6PxQuatE");

      function New_PxTransform (m : access constant px_h.PxMat44) return PxTransform;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:82
      pragma CPP_Constructor (New_PxTransform, "_ZN5physx11PxTransformC1ERKNS_7PxMat44E");

      function operator_eq (this : access constant PxTransform; t : access constant PxTransform) return Extensions.bool  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:87
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx11PxTransformeqERKS0_";

      function operator_t (this : access constant PxTransform; x : access constant PxTransform) return PxTransform  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:92
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx11PxTransformmlERKS0_";

      function Assign_PxTransform (this : access PxTransform; other : access PxTransform) return access PxTransform  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:99
      with Import => True,
           Convention => CPP,
           External_Name => "_ZN5physx11PxTransformmLERS0_";

      function getInverse (this : access constant PxTransform) return PxTransform  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:105
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx11PxTransform10getInverseEv";

      function transform (this : access constant PxTransform; input : access constant pxvec3_h.Class_PxVec3.PxVec3) return pxvec3_h.Class_PxVec3.PxVec3  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:111
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx11PxTransform9transformERKNS_6PxVec3E";

      function transformInv (this : access constant PxTransform; input : access constant pxvec3_h.Class_PxVec3.PxVec3) return pxvec3_h.Class_PxVec3.PxVec3  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:117
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx11PxTransform12transformInvERKNS_6PxVec3E";

      function rotate (this : access constant PxTransform; input : access constant pxvec3_h.Class_PxVec3.PxVec3) return pxvec3_h.Class_PxVec3.PxVec3  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:123
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx11PxTransform6rotateERKNS_6PxVec3E";

      function rotateInv (this : access constant PxTransform; input : access constant pxvec3_h.Class_PxVec3.PxVec3) return pxvec3_h.Class_PxVec3.PxVec3  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:129
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx11PxTransform9rotateInvERKNS_6PxVec3E";

      function transform (this : access constant PxTransform; src : access constant PxTransform) return PxTransform  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:136
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx11PxTransform9transformERKS0_";

      function isValid (this : access constant PxTransform) return Extensions.bool  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:148
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx11PxTransform7isValidEv";

      function isSane (this : access constant PxTransform) return Extensions.bool  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:158
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx11PxTransform6isSaneEv";

      function isFinite (this : access constant PxTransform) return Extensions.bool  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:166
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx11PxTransform8isFiniteEv";

      function transformInv (this : access constant PxTransform; src : access constant PxTransform) return PxTransform  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:172
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx11PxTransform12transformInvERKS0_";

      function transform (this : access constant PxTransform; plane : access constant pxplane_h.Class_PxPlane.PxPlane) return pxplane_h.Class_PxPlane.PxPlane  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:185
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx11PxTransform9transformERKNS_7PxPlaneE";

      function inverseTransform (this : access constant PxTransform; plane : access constant pxplane_h.Class_PxPlane.PxPlane) return pxplane_h.Class_PxPlane.PxPlane  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:195
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx11PxTransform16inverseTransformERKNS_7PxPlaneE";

      function getNormalized (this : access constant PxTransform) return PxTransform  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxtransform.h:204
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx11PxTransform13getNormalizedEv";
   end;
   use Class_PxTransform;

  -----------
  -- Plane --
  -----------

   package Class_PxPlane is
      type PxPlane is limited record
         n : aliased pxvec3_h.Class_PxVec3.PxVec3;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxplane.h:136
         d : aliased float;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxplane.h:137
      end record
      with Import => True,
           Convention => CPP;

      function New_PxPlane return PxPlane;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxplane.h:56
      pragma CPP_Constructor (New_PxPlane, "_ZN5physx7PxPlaneC1Ev");

      function New_PxPlane
        (nx : float;
         ny : float;
         nz : float;
         distance : float) return PxPlane;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxplane.h:63
      pragma CPP_Constructor (New_PxPlane, "_ZN5physx7PxPlaneC1Effff");

      function New_PxPlane (normal : access constant pxvec3_h.Class_PxVec3.PxVec3; distance : float) return PxPlane;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxplane.h:70
      pragma CPP_Constructor (New_PxPlane, "_ZN5physx7PxPlaneC1ERKNS_6PxVec3Ef");

      function New_PxPlane (point : access constant pxvec3_h.Class_PxVec3.PxVec3; normal : access constant pxvec3_h.Class_PxVec3.PxVec3) return PxPlane;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxplane.h:77
      pragma CPP_Constructor (New_PxPlane, "_ZN5physx7PxPlaneC1ERKNS_6PxVec3ES3_");

      function New_PxPlane
        (p0 : access constant pxvec3_h.Class_PxVec3.PxVec3;
         p1 : access constant pxvec3_h.Class_PxVec3.PxVec3;
         p2 : access constant pxvec3_h.Class_PxVec3.PxVec3) return PxPlane;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxplane.h:85
      pragma CPP_Constructor (New_PxPlane, "_ZN5physx7PxPlaneC1ERKNS_6PxVec3ES3_S3_");

      function operator_eq (this : access constant PxPlane; p : access constant PxPlane) return Extensions.bool  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxplane.h:94
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx7PxPlaneeqERKS0_";

      function distance (this : access constant PxPlane; p : access constant pxvec3_h.Class_PxVec3.PxVec3) return float  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxplane.h:99
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx7PxPlane8distanceERKNS_6PxVec3E";

      function contains (this : access constant PxPlane; p : access constant pxvec3_h.Class_PxVec3.PxVec3) return Extensions.bool  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxplane.h:104
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx7PxPlane8containsERKNS_6PxVec3E";

      function project (this : access constant PxPlane; p : access constant pxvec3_h.Class_PxVec3.PxVec3) return pxvec3_h.Class_PxVec3.PxVec3  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxplane.h:112
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx7PxPlane7projectERKNS_6PxVec3E";

      function pointInPlane (this : access constant PxPlane) return pxvec3_h.Class_PxVec3.PxVec3  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxplane.h:120
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx7PxPlane12pointInPlaneEv";

      procedure normalize (this : access PxPlane)  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\foundation\pxplane.h:129
      with Import => True,
           Convention => CPP,
           External_Name => "_ZN5physx7PxPlane9normalizeEv";
   end;

  -----------
  -- Shape --
  -----------

   type PxFilterData is null record;   -- incomplete struct

   type PxRaycastHit is null record;   -- incomplete struct

   type PxSweepHit is null record;   -- incomplete struct

   subtype Enum is unsigned;
   eSIMULATION_SHAPE : constant unsigned := 1;
   eSCENE_QUERY_SHAPE : constant unsigned := 2;
   eTRIGGER_SHAPE : constant unsigned := 4;
   eVISUALIZATION : constant unsigned := 8;
   type PxShapeFlag is record
      null;
   end record
   with Convention => C_Pass_By_Copy;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:63

   subtype PxShapeFlags is PxFlags_h.Class_PxFlags.PxFlags;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:122

   type Enum;
   function operator_a (a : Enum; b : Enum) return PxFlags_h.Class_PxFlags.PxFlags  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:123
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physxanENS_11PxShapeFlag4EnumES1_";

   function operator_ti (a : Enum) return PxFlags_h.Class_PxFlags.PxFlags  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:123
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physxcoENS_11PxShapeFlag4EnumE";

   function operator_o (a : Enum; b : Enum) return PxFlags_h.Class_PxFlags.PxFlags  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:123
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physxorENS_11PxShapeFlag4EnumES1_";

   package Class_PxShape is
      type PxShape is abstract limited new PxBase_h.Class_PxBase.PxBase with record
         userData : System.Address;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:624
      end record
      with Import => True,
           Convention => CPP;

      procedure release (this : access PxShape) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:156

      function getReferenceCount (this : access constant PxShape) return px_h.PxU32 is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:166

      procedure acquireReference (this : access PxShape) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:173

      function getGeometryType (this : access constant PxShape) return PxGeometry_h.Enum is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:182

      procedure setGeometry (this : access PxShape; geometry : access constant PxGeometry_h.Class_PxGeometry.PxGeometry) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:195

      function getGeometry (this : access constant PxShape) return PxGeometryHelpers_h.Class_PxGeometryHolder.PxGeometryHolder is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:206

      function getBoxGeometry (this : access constant PxShape; geometry : access pxboxgeometry_h.Class_PxBoxGeometry.PxBoxGeometry) return Extensions.bool is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:220

      function getSphereGeometry (this : access constant PxShape; geometry : access pxspheregeometry_h.Class_PxSphereGeometry.PxSphereGeometry) return Extensions.bool is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:233

      function getCapsuleGeometry (this : access constant PxShape; geometry : access pxcapsulegeometry_h.Class_PxCapsuleGeometry.PxCapsuleGeometry) return Extensions.bool is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:246

      function getPlaneGeometry (this : access constant PxShape; geometry : access pxplanegeometry_h.Class_PxPlaneGeometry.PxPlaneGeometry) return Extensions.bool is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:259

      function getConvexMeshGeometry (this : access constant PxShape; geometry : access pxconvexmeshgeometry_h.Class_PxConvexMeshGeometry.PxConvexMeshGeometry) return Extensions.bool is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:272

      function getTriangleMeshGeometry (this : access constant PxShape; geometry : access pxtrianglemeshgeometry_h.Class_PxTriangleMeshGeometry.PxTriangleMeshGeometry) return Extensions.bool is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:285

      function getHeightFieldGeometry (this : access constant PxShape; geometry : access pxheightfieldgeometry_h.Class_PxHeightFieldGeometry.PxHeightFieldGeometry) return Extensions.bool is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:299

      function getActor (this : access constant PxShape) return access PxRigidActor_h.Class_PxRigidActor.PxRigidActor is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:308

      procedure setLocalPose (this : access PxShape; pose : access constant pxtransform_h.Class_PxTransform.PxTransform) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:335

      function getLocalPose (this : access constant PxShape) return pxtransform_h.Class_PxTransform.PxTransform is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:346

      procedure setSimulationFilterData (this : access PxShape; data : access constant PxFilterData) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:365

      function getSimulationFilterData (this : access constant PxShape) return PxFilterData is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:372

      procedure setQueryFilterData (this : access PxShape; data : access constant PxFilterData) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:381

      function getQueryFilterData (this : access constant PxShape) return PxFilterData is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:388

      procedure setMaterials
        (this : access PxShape;
         materials : System.Address;
         materialCount : pxsimpletypes_h.PxU16) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:403

      function getNbMaterials (this : access constant PxShape) return pxsimpletypes_h.PxU16 is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:414

      function getMaterials
        (this : access constant PxShape;
         userBuffer : System.Address;
         bufferSize : px_h.PxU32;
         startIndex : px_h.PxU32) return px_h.PxU32 is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:430

      function getMaterialFromInternalFaceIndex (this : access constant PxShape; faceIndex : px_h.PxU32) return access PxPhysXConfig_h.PxMaterial is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:450

      procedure setContactOffset (this : access PxShape; contactOffset : pxsimpletypes_h.PxReal) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:468

      function getContactOffset (this : access constant PxShape) return pxsimpletypes_h.PxReal is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:477

      procedure setRestOffset (this : access PxShape; restOffset : pxsimpletypes_h.PxReal) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:494

      function getRestOffset (this : access constant PxShape) return pxsimpletypes_h.PxReal is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:503

      procedure setTorsionalPatchRadius (this : access PxShape; radius : pxsimpletypes_h.PxReal) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:517

      function getTorsionalPatchRadius (this : access constant PxShape) return pxsimpletypes_h.PxReal is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:529

      procedure setMinTorsionalPatchRadius (this : access PxShape; radius : pxsimpletypes_h.PxReal) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:542

      function getMinTorsionalPatchRadius (this : access constant PxShape) return pxsimpletypes_h.PxReal is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:554

      procedure setFlag
        (this : access PxShape;
         flag : Enum;
         value : Extensions.bool) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:571

      procedure setFlags (this : access PxShape; inFlags : PxShapeFlags) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:578

      function getFlags (this : access constant PxShape) return PxShapeFlags is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:587

      function isExclusive (this : access constant PxShape) return Extensions.bool is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:594

      procedure setName (this : access PxShape; name : Interfaces.C.Strings.chars_ptr) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:608

      function getName (this : access constant PxShape) return Interfaces.C.Strings.chars_ptr is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:617

      function getConcreteTypeName (this : access constant PxShape) return Interfaces.C.Strings.chars_ptr  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:620
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx7PxShape19getConcreteTypeNameEv";

      function New_PxShape (baseFlags : access PxBase_h.Class_PxBaseFlags.PxBaseFlags) return PxShape is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:627
      pragma CPP_Constructor (New_PxShape, "_ZN5physx7PxShapeC1ENS_7PxFlagsINS_10PxBaseFlag4EnumEtEE");

      function New_PxShape (concreteType : PxBase_h.PxType; baseFlags : access PxBase_h.Class_PxBaseFlags.PxBaseFlags) return PxShape is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:628
      pragma CPP_Constructor (New_PxShape, "_ZN5physx7PxShapeC1EtNS_7PxFlagsINS_10PxBaseFlag4EnumEtEE");

      procedure Delete_PxShape (this : access PxShape)  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:629
      with Import => True,
           Convention => CPP,
           External_Name => "_ZN5physx7PxShapeD1Ev";

      procedure Delete_And_Free_PxShape (this : access PxShape)  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:629
      with Import => True,
           Convention => CPP,
           External_Name => "_ZN5physx7PxShapeD0Ev";

      function isKindOf (this : access constant PxShape; name : Interfaces.C.Strings.chars_ptr) return Extensions.bool  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxShape.h:630
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx7PxShape8isKindOfEPKc";
   end;
   use Class_PxShape;

  -----------------
  -- Rigid_Actor --
  -----------------

   type PxBVHStructure is null record;   -- incomplete struct

   package Class_PxRigidActor is
      type PxRigidActor is abstract limited new PxActor_h.Class_PxActor.PxActor with record
         null;
      end record
      with Import => True,
           Convention => CPP;

      procedure release (this : access PxRigidActor) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxRigidActor.h:77

      function getGlobalPose (this : access constant PxRigidActor) return pxtransform_h.Class_PxTransform.PxTransform is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxRigidActor.h:93

      procedure setGlobalPose
        (this : access PxRigidActor;
         pose : access constant pxtransform_h.Class_PxTransform.PxTransform;
         autowake : Extensions.bool) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxRigidActor.h:120

      function attachShape (this : access PxRigidActor; shape : access PxShape_h.Class_PxShape.PxShape'Class) return Extensions.bool is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxRigidActor.h:145

      procedure detachShape
        (this : access PxRigidActor;
         shape : access PxShape_h.Class_PxShape.PxShape'Class;
         wakeOnLostTouch : Extensions.bool) is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxRigidActor.h:158

      function getNbShapes (this : access constant PxRigidActor) return px_h.PxU32 is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxRigidActor.h:170

      function getShapes
        (this : access constant PxRigidActor;
         userBuffer : System.Address;
         bufferSize : px_h.PxU32;
         startIndex : px_h.PxU32) return px_h.PxU32 is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxRigidActor.h:189

      function getNbConstraints (this : access constant PxRigidActor) return px_h.PxU32 is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxRigidActor.h:205

      function getConstraints
        (this : access constant PxRigidActor;
         userBuffer : System.Address;
         bufferSize : px_h.PxU32;
         startIndex : px_h.PxU32) return px_h.PxU32 is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxRigidActor.h:222

      function New_PxRigidActor (concreteType : PxBase_h.PxType; baseFlags : access PxBase_h.Class_PxBaseFlags.PxBaseFlags) return PxRigidActor is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxRigidActor.h:225
      pragma CPP_Constructor (New_PxRigidActor, "_ZN5physx12PxRigidActorC1EtNS_7PxFlagsINS_10PxBaseFlag4EnumEtEE");

      function New_PxRigidActor (baseFlags : access PxBase_h.Class_PxBaseFlags.PxBaseFlags) return PxRigidActor is abstract;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxRigidActor.h:226
      pragma CPP_Constructor (New_PxRigidActor, "_ZN5physx12PxRigidActorC1ENS_7PxFlagsINS_10PxBaseFlag4EnumEtEE");

      procedure Delete_PxRigidActor (this : access PxRigidActor)  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxRigidActor.h:227
      with Import => True,
           Convention => CPP,
           External_Name => "_ZN5physx12PxRigidActorD1Ev";

      procedure Delete_And_Free_PxRigidActor (this : access PxRigidActor)  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxRigidActor.h:227
      with Import => True,
           Convention => CPP,
           External_Name => "_ZN5physx12PxRigidActorD0Ev";

      function isKindOf (this : access constant PxRigidActor; name : Interfaces.C.Strings.chars_ptr) return Extensions.bool  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\PxRigidActor.h:228
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx12PxRigidActor8isKindOfEPKc";
   end;
   use Class_PxRigidActor;

   --------------
   -- Geometry --
   --------------

      subtype Enum is int;
   eSPHERE : constant int := 0;
   ePLANE : constant int := 1;
   eCAPSULE : constant int := 2;
   eBOX : constant int := 3;
   eCONVEXMESH : constant int := 4;
   eTRIANGLEMESH : constant int := 5;
   eHEIGHTFIELD : constant int := 6;
   eGEOMETRY_COUNT : constant int := 7;
   eINVALID : constant int := -1;
   type PxGeometryType is record
      null;
   end record
   with Convention => C_Pass_By_Copy;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\geometry/PxGeometry.h:51

   package Class_PxGeometry is
      type PxGeometry is limited record
         mType : aliased Enum;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\geometry/PxGeometry.h:86
      end record
      with Import => True,
           Convention => CPP;

      function getType (this : access constant PxGeometry) return Enum  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\geometry/PxGeometry.h:82
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx10PxGeometry7getTypeEv";

      function New_PxGeometry (c_type : Enum) return PxGeometry;  -- C:\Users\exitcode0\Desktop\PhysX-4.1\physx\include\geometry/PxGeometry.h:85
      pragma CPP_Constructor (New_PxGeometry, "_ZN5physx10PxGeometryC1ENS_14PxGeometryType4EnumE");
   end;
   use Class_PxGeometry;

  ---------------------
  -- Sphere_Geometry --
  ---------------------

     package Class_PxSphereGeometry is
      type PxSphereGeometry is limited record
         parent : aliased PxGeometry_h.Class_PxGeometry.PxGeometry;
         radius : aliased pxsimpletypes_h.PxReal;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxspheregeometry.h:72
      end record
      with Import => True,
           Convention => CPP;

      function New_PxSphereGeometry return PxSphereGeometry;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxspheregeometry.h:52
      pragma CPP_Constructor (New_PxSphereGeometry, "_ZN5physx16PxSphereGeometryC1Ev");

      function New_PxSphereGeometry (ir : pxsimpletypes_h.PxReal) return PxSphereGeometry;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxspheregeometry.h:53
      pragma CPP_Constructor (New_PxSphereGeometry, "_ZN5physx16PxSphereGeometryC1Ef");

      function isValid (this : access constant PxSphereGeometry) return Extensions.bool  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxspheregeometry.h:76
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx16PxSphereGeometry7isValidEv";
   end;
   use Class_PxSphereGeometry;

  -----------------
  -- Convex_Mesh --
  -----------------

   type PxHullPolygon_mPlane_array is array (0 .. 3) of aliased pxsimpletypes_h.PxReal;
   type PxHullPolygon is record
      mPlane : aliased PxHullPolygon_mPlane_array;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:53
      mNbVerts : aliased pxsimpletypes_h.PxU16;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:54
      mIndexBase : aliased pxsimpletypes_h.PxU16;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:55
   end record
   with Convention => C_Pass_By_Copy;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:51

   package Class_PxConvexMesh is
      type PxConvexMesh is abstract limited new PxBase_h.Class_PxBase.PxBase with record
         null;
      end record
      with Import => True,
           Convention => CPP;

      function getNbVertices (this : access constant PxConvexMesh) return px_h.PxU32 is abstract;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:93

      function getVertices (this : access constant PxConvexMesh) return access constant pxvec3_h.Class_PxVec3.PxVec3 is abstract;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:100

      function getIndexBuffer (this : access constant PxConvexMesh) return access pxsimpletypes_h.PxU8 is abstract;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:107

      function getNbPolygons (this : access constant PxConvexMesh) return px_h.PxU32 is abstract;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:114

      function getPolygonData
        (this : access constant PxConvexMesh;
         index : px_h.PxU32;
         data : access PxHullPolygon) return Extensions.bool is abstract;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:123

      procedure release (this : access PxConvexMesh) is abstract;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:130

      function getReferenceCount (this : access constant PxConvexMesh) return px_h.PxU32 is abstract;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:140

      procedure acquireReference (this : access PxConvexMesh) is abstract;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:147

      procedure getMassInformation
        (this : access constant PxConvexMesh;
         mass : access pxsimpletypes_h.PxReal;
         localInertia : access pxmat33_h.Class_PxMat33.PxMat33;
         localCenterOfMass : access pxvec3_h.Class_PxVec3.PxVec3) is abstract;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:165

      function getLocalBounds (this : access constant PxConvexMesh) return PxBounds3_h.Class_PxBounds3.PxBounds3 is abstract;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:172

      function getConcreteTypeName (this : access constant PxConvexMesh) return Interfaces.C.Strings.chars_ptr  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:174
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx12PxConvexMesh19getConcreteTypeNameEv";

      function isGpuCompatible (this : access constant PxConvexMesh) return Extensions.bool is abstract;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:183

      function New_PxConvexMesh (concreteType : PxBase_h.PxType; baseFlags : access PxBase_h.Class_PxBaseFlags.PxBaseFlags) return PxConvexMesh is abstract;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:186
      pragma CPP_Constructor (New_PxConvexMesh, "_ZN5physx12PxConvexMeshC1EtNS_7PxFlagsINS_10PxBaseFlag4EnumEtEE");

      function New_PxConvexMesh (baseFlags : access PxBase_h.Class_PxBaseFlags.PxBaseFlags) return PxConvexMesh is abstract;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:187
      pragma CPP_Constructor (New_PxConvexMesh, "_ZN5physx12PxConvexMeshC1ENS_7PxFlagsINS_10PxBaseFlag4EnumEtEE");

      procedure Delete_PxConvexMesh (this : access PxConvexMesh)  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:188
      with Import => True,
           Convention => CPP,
           External_Name => "_ZN5physx12PxConvexMeshD1Ev";

      procedure Delete_And_Free_PxConvexMesh (this : access PxConvexMesh)  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:188
      with Import => True,
           Convention => CPP,
           External_Name => "_ZN5physx12PxConvexMeshD0Ev";

      function isKindOf (this : access constant PxConvexMesh; name : Interfaces.C.Strings.chars_ptr) return Extensions.bool  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxconvexmesh.h:189
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx12PxConvexMesh8isKindOfEPKc";
   end;
   use Class_PxConvexMesh;

  ----------------------
  -- Capsule_Geometry --
  ----------------------

     package Class_PxCapsuleGeometry is
      type PxCapsuleGeometry is limited record
         parent : aliased PxGeometry_h.Class_PxGeometry.PxGeometry;
         radius : aliased pxsimpletypes_h.PxReal;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxcapsulegeometry.h:84
         halfHeight : aliased pxsimpletypes_h.PxReal;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxcapsulegeometry.h:89
      end record
      with Import => True,
           Convention => CPP;

      function New_PxCapsuleGeometry return PxCapsuleGeometry;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxcapsulegeometry.h:61
      pragma CPP_Constructor (New_PxCapsuleGeometry, "_ZN5physx17PxCapsuleGeometryC1Ev");

      function New_PxCapsuleGeometry (radius_u : pxsimpletypes_h.PxReal; halfHeight_u : pxsimpletypes_h.PxReal) return PxCapsuleGeometry;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxcapsulegeometry.h:66
      pragma CPP_Constructor (New_PxCapsuleGeometry, "_ZN5physx17PxCapsuleGeometryC1Eff");

      function isValid (this : access constant PxCapsuleGeometry) return Extensions.bool  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxcapsulegeometry.h:93
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx17PxCapsuleGeometry7isValidEv";
   end;
   use Class_PxCapsuleGeometry;
   function PxTransformFromSegment
     (p0 : access constant pxvec3_h.Class_PxVec3.PxVec3;
      p1 : access constant pxvec3_h.Class_PxVec3.PxVec3;
      halfHeight : access pxsimpletypes_h.PxReal) return pxtransform_h.Class_PxTransform.PxTransform  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxcapsulegeometry.h:114
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physx22PxTransformFromSegmentERKNS_6PxVec3ES2_Pf";

  -------------------
  -- Triangle_Mesh --
  -------------------

   subtype Enum is unsigned;
   eDOUBLE_SIDED : constant unsigned := 2;
   type PxMeshGeometryFlag is record
      null;
   end record
   with Convention => C_Pass_By_Copy;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxtrianglemeshgeometry.h:53

   subtype PxMeshGeometryFlags is PxFlags_h.Class_PxFlags.PxFlags;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxtrianglemeshgeometry.h:68

   type Enum;
   function operator_o (a : Enum; b : Enum) return PxFlags_h.Class_PxFlags.PxFlags  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxtrianglemeshgeometry.h:69
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physxorENS_18PxMeshGeometryFlag4EnumES1_";

   function operator_a (a : Enum; b : Enum) return PxFlags_h.Class_PxFlags.PxFlags  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxtrianglemeshgeometry.h:69
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physxanENS_18PxMeshGeometryFlag4EnumES1_";

   function operator_ti (a : Enum) return PxFlags_h.Class_PxFlags.PxFlags  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxtrianglemeshgeometry.h:69
   with Import => True,
        Convention => CPP,
        External_Name => "_ZN5physxcoENS_18PxMeshGeometryFlag4EnumE";

   package Class_PxTriangleMeshGeometry is
      type PxTriangleMeshGeometry is limited record
         parent : aliased PxGeometry_h.Class_PxGeometry.PxGeometry;
         scale : aliased pxmeshscale_h.Class_PxMeshScale.PxMeshScale;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxtrianglemeshgeometry.h:124
         meshFlags : aliased PxMeshGeometryFlags;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxtrianglemeshgeometry.h:125
         paddingFromFlags : aliased pxcoreutilitytypes_h.Class_PxPadding.PxPadding;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxtrianglemeshgeometry.h:126
         triangleMesh : access PxPhysXCommonConfig_h.PxTriangleMesh;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxtrianglemeshgeometry.h:127
      end record
      with Import => True,
           Convention => CPP;

      function New_PxTriangleMeshGeometry return PxTriangleMeshGeometry;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxtrianglemeshgeometry.h:90
      pragma CPP_Constructor (New_PxTriangleMeshGeometry, "_ZN5physx22PxTriangleMeshGeometryC1Ev");

      function New_PxTriangleMeshGeometry
        (mesh : access PxPhysXCommonConfig_h.PxTriangleMesh;
         scaling : access constant pxmeshscale_h.Class_PxMeshScale.PxMeshScale;
         flags : access PxMeshGeometryFlags) return PxTriangleMeshGeometry;  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxtrianglemeshgeometry.h:102
      pragma CPP_Constructor (New_PxTriangleMeshGeometry, "_ZN5physx22PxTriangleMeshGeometryC1EPNS_14PxTriangleMeshERKNS_11PxMeshScaleENS_7PxFlagsINS_18PxMeshGeometryFlag4EnumEhEE");

      function isValid (this : access constant PxTriangleMeshGeometry) return Extensions.bool  -- c:\users\exitcode0\desktop\physx-4.1\physx\include\geometry\pxtrianglemeshgeometry.h:131
      with Import => True,
           Convention => CPP,
           External_Name => "_ZNK5physx22PxTriangleMeshGeometry7isValidEv";
   end;
   use Class_PxTriangleMeshGeometry;




  PxShape
        PxFoundation*        mFoundation;
        PxPhysics*          mPhysics;
        PxScene*          mScene;
        PxCooking*          mCooking;
        PxMaterial*          mDefaultMaterial;
        std::vector<PxConvexMesh*>  mConvexObjects;
          PxMaterial*          CreateMaterial(const PINT_MATERIAL_CREATE& desc);
        PxConvexMesh*        CreateConvexMesh(const Point* verts, udword vertCount, PxConvexFlags flags, PintShapeRenderer* renderer);
        PxTriangleMesh*        CreateTriangleMesh(const SurfaceInterface& surface, PintShapeRenderer* renderer);
end;
--  //  PxShape* Shape = GetShapeFromHandle(handle);
--      PxShape* Shape = (PxShape*)handle;
--      PxTransform lp = Shape->getLocalPose();
--      lp.q = ToPxQuat(q);
--          const PxU32 NbArticulations = mScene->getNbArticulations();
--          for(PxU32 i=0;i<NbArticulations;i++)
--          {
--              PxArticulation* Articulation;
--              mScene->getArticulations(&Articulation, 1, i);
--              const PxU32 NbLinks = Articulation->getNbLinks();
--              PxArticulationLink* Links[256];
--              PxU32 Nb = Articulation->getLinks(Links, 256);
--              for(PxU32 jj=0;jj<NbLinks;jj++)
--                      PxU32 nbShapes = rigidActor->getNbShapes();
--                      for(PxU32 j=0;j<nbShapes;j++)
--                                  PxSphereGeometry geometry;
--                                  PxBoxGeometry geometry;
--                                  PxCapsuleGeometry geometry;
--                              else if(geomType==PxGeometryType::eCONVEXMESH)
--  /*                              const PxQuat q = PxShortestRotation(PxVec3(0.0f, 1.0f, 0.0f), PxVec3(1.0f,
--  #include "PxRaycastCCD.h"
--  PxConvexMesh* SharedPhysX::CreateConvexMesh(const Point* verts, udword vertCount, PxConvexFlags flags,
--  //  M->setFrictionCombineMode(PxCombineMode::eMIN);
--      PxRigidActor* actor;
--      PxRigidDynamic* rigidDynamic = null;
--          PxSetJointGlobalFrame(*rv, &globalAnchor, &globalAxis);
--      if(desc.mMass!=0.0f)
--                  PxD6Joint* j = PxD6JointCreate(physics, actor0, PxTransform(ToPxVec3(jc.mLocalPivot0)), actor1,
--  //              j->setLimit(PxJointLimitPair(jc.mMinLimitAngle, jc.mMaxLimitAngle, TWOPI));
--                  j->setRevoluteJointFlag(PxRevoluteJointFlag::eLIMIT_ENABLED, true);
--  //  M->setRestitutionCombineMode(PxCombineMode::eMIN);
--                  PxSphericalJoint* j = PxSphericalJointCreate(physics, actor0, PxTransform(
--      PxCombineMode::Enum defMode = M->getFrictionCombineMode();
--  PxMaterial* SharedPhysX::CreateMaterial(const PINT_MATERIAL_CREATE& desc)
--          const PxRenderBuffer& RenderBuffer = mScene->getRenderBuffer();
--  PxErrorCode::Enum code,
--  PxQuat q = PxShortestRotation(PxVec3(0.0f, 1.0f, 0.0f), PxVec3(1.0f, 0.0f, 0.0f));
--  PxRigidActor* RigidActor = GetActorFromHandle(handle);
--  PxShape* Shape = GetShapeFromHandle(handle);
--  PxConcreteType::eRIGID_DYNAMIC)
--  PxRigidDynamic* RigidDynamic = static_cast<PxRigidDynamic*>(RigidActor);
--  RigidDynamic->getRigidDynamicFlags() & PxRigidDynamicFlag::eKINEMATIC)
--  RigidBody->getRigidBodyFlags() & PxRigidBodyFlag::eKINEMATIC)
--  PxRigidBodyExt::setMassAndUpdateInertia(rigid_body, desc.mMass);
--  PxRigidBodyExt::setMassAndUpdateInertia(rigid_body, desc.mMassForInertia);
--   PxTransform Pose = rigid_body.getCMassLocalPose();
--   rigid_body.setCMassLocalPose(Pose);
--  PxU32 gNbLocalVectors = 0;
--  PxVec3 gLocalVectors[256];
--          localPose[i] = PxTransform(PxIdentity);
--              localPose[i].p = actors[i] ? actors[i]->getGlobalPose().transformInv(*wsAnchor) : *wsAnchor;
--  PxMat33 mM(m.q);
--  //const PxQuat q0 = PxShortestRotation(PxVec3(1.0f, 0.0f, 0.0f), rot.column0);
--  PxD6Joint* j
--  PxPhysics& physics
--          PxForceMode::Enum mode;
--          PxSetGroupCollisionFlag(groups[i].mGroup0, groups[i].mGroup1, false);
--  }
--          PxRigidBodyExt::addForceAtPos(*RigidBody, ToPxVec3(action), ToPxVec3(pos), mode);
--      const PxTransform Pose(ToPxVec3(pose.mPos), ToPxQuat(pose.mRot));
--          PxRigidStatic* rigidStatic = mPhysics->createRigidStatic(pose);
--      PxRigidActor* actor0 = (PxRigidActor*)desc.mObject0;
--      PxJoint* CreatedJoint = null;                PxD6Joint* j = PxD6JointCreate(physics, actor0, PxTransform(ToPxVec3(jc.mLocalPivot0)), actor1, PxTransform(ToPxVec3(jc.mLocalPivot1)));
--                  ASSERT(j);
--                  CreatedJoint = j;
--                      j->setPrismaticJointFlag(PxPrismaticJointFlag::eLIMIT_ENABLED, false);
--                  SetupD6Projection(j, enable_projection, projection_linear_tolerance,
--                  j->setMotion(PxD6Axis::eX, PxD6Motion::eFREE);
--      Params.meshPreprocessParams = mParams.mPrecomputeActiveEdges ? PxMeshPreprocessingFlags(0) : PxMeshPreprocessingFlag::eDISABLE_ACTIVE_EDGES_PRECOMPUTE;
--
--  /*      mScene->setFlag(PxSceneFlag::eENABLE_MANUAL_QUERY_UPDATE, gSQManualFlushUpdates);
--                      //      RigidBody->addTorque(GlobalTorque, PxForceMode::eFORCE, true);
--                              RigidBody->addTorque(GlobalTorque, PxForceMode::eACCELERATION, true);
--      link.setRigidBodyFlag(PxRigidBodyFlag::eENABLE_CCD, mParams.mUseCCD);
--      link.setRigidBodyFlag(PxRigidBodyFlag::eKINEMATIC, desc.mKinematic);
--              udword Nb = mScene->getActors(PxActorTypeFlag::eRIGID_DYNAMIC, Actors, 2048);
--      rigidDynamic.setRigidDynamicFlag(PxRigidDynamicFlag::eKINEMATIC, desc.mKinematic);
--  #endif
--      mCooking = PxCreateCooking(PX_PHYSICS_VERSION, *mFoundation, Params);
--      PxCookingParams Params(scale);
--          PxSetJointGlobalFrame(*rv, &globalAnchor, &globalAxis);
--              PxJointLimitPair limit(-PxPi/2, PxPi/2, 0.05f);
--          rv->setConstraintFlags(PxConstraintFlag::ePROJECTION);
--              rv->setRevoluteJointFlags(PxRevoluteJointFlag::eLIMIT_ENABLED);
--          PxRevoluteJoint* rv = PxRevoluteJointCreate(physics, b0->mBody, PxTransform::createIdentity(),
--                  j->setProjectionLinearTolerance(projection_linear_tolerance);
--                  j->setProjectionAngularTolerance(projection_angular_tolerance);
--                  j->setRevoluteJointFlag(PxRevoluteJointFlag::eLIMIT_ENABLED, true);
--                  PxJointAngularLimitPair limit(0.0f, 0.0f, limitContactDistance);
--                  PxSetJointGlobalFrame(*j, &GlobalAnchor, &GlobalAxis);
--                  PxSetJointGlobalFrame(*j, actor0, ToPxVec3(jc.mLocalPivot0), ToPxVec3(jc.mLocalAxis0), 0);
--
--                  j->setMotion(PxD6Axis::eTWIST, PxD6Motion::eFREE);
--                  j->setMotion(PxD6Axis::eSWING1, PxD6Motion::eFREE);
--                  PxJointAngularLimitPair limit(0.0f, 0.0f, limitContactDistance);
--
--                PxSetJointGlobalFrame(*j, actor1, ToPxVec3(jc.mLocalPivot1), ToPxVec3(jc.mLocalAxis1), 1);
