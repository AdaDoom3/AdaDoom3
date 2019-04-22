
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

-- ???
separate (Neo.Engine.Renderer) package body FGED2 is

 package Test_Shader is new Shader ("test", ((Kind     => Fragment_Stage,
                                              --Uniforms => (),
                                              others   => <>),
                                             (Kind     => Vertex_Stage,
                                              --Uniforms => (),
                                              others   => <>)));

 -----------------
 -- Build_Frame --
 -----------------

 procedure Build_Frame (Frame : in out Framebuffer_State; View : View_State) is
   State : Bottom_Level_State := (others => <>);
   begin

     -- State
     State.Mesh := U ("zombie");

     -- Shader
     Test_Shader.Commit;

     -- Rasterization
     Pipeline.Cull_Mode          := VK_CULL_MODE_BACK_BIT;          -- VkCullModeFlagBits
     Pipeline.Front_Face         := VK_FRONT_FACE_COUNTER_CLOCKWISE; -- VkFrontFace
     Pipeline.Has_Mirror_View    := False;
     Pipeline.Has_Polygon_Offset := False;

     -- Color blending
     Pipeline.Color_Blend              := VK_BLEND_OP_ADD;      -- VkBlendOp
     Pipeline.Alpha_Blend              := VK_BLEND_OP_ADD;      -- VkBlendOp
     Pipeline.Source_Blend_Factor      := VK_BLEND_FACTOR_ZERO; -- VkBlendFactor
     Pipeline.Destination_Blend_Factor := VK_BLEND_FACTOR_ZERO; -- VkBlendFactor
     Pipeline.Color_Mask               := VK_COLOR_COMPONENT_R_BIT
                                            or VK_COLOR_COMPONENT_G_BIT
                                            or VK_COLOR_COMPONENT_B_BIT
                                            or VK_COLOR_COMPONENT_A_BIT;

     -- Depth stencil
     Pipeline.Front_Fail         := VK_STENCIL_OP_KEEP;  -- VkStencilOp
     Pipeline.Front_Pass         := VK_STENCIL_OP_KEEP;  -- VkStencilOp
     Pipeline.Front_Depth_Fail   := VK_STENCIL_OP_KEEP;  -- VkStencilOp
     Pipeline.Back_Fail          := VK_STENCIL_OP_KEEP;  -- VkStencilOp
     Pipeline.Back_Pass          := VK_STENCIL_OP_KEEP;  -- VkStencilOp
     Pipeline.Back_Depth_Fail    := VK_STENCIL_OP_KEEP;  -- VkStencilOp
     Pipeline.Stencil_Compare    := VK_COMPARE_OP_NEVER; -- VkCompareOp
     Pipeline.Depth_Compare      := VK_COMPARE_OP_NEVER; -- VkCompareOp
     Pipeline.Compare_Mask       := 0;
     Pipeline.Stencil_Reference  := 0;
     Pipeline.Depth_Write_Enable := False;
     Pipeline.Depths_Bounds_Test := False;
     Pipeline.Has_Back_Stencil   := False;

     -- Actually draw
     Draw (State, Frame.Commands, (others => True)); -- Data : Bottom_Level_State; Commands : in out Ptr; Surface_Sort : Surface_Sort_Array)
   end;

 ----------------
 -- Build_View --
 ----------------

 First_Time : Bool := True;
 function Build_View return View_State is
   Bottom_Level : Bottom_Level_State;
   View         : View_State;
   begin
     if First_Time then

    Load_Model ("zombie");
    Buffer_Mesh ("zombie");
       First_Time := False;
     end if;
     --Line ("Build View");
     return View;
   end;
end;
