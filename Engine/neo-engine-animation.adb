
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

package body Neo.Engine.Animation is

  -----------
  -- Frame --
  -----------

  -- Build a skeleton and its 3D bounds from a Joint array into a tree for animation purposes
  function Build_Frame (Joints : Vector_Indexed_Joint.Unsafe.Vector) return Animation_Frame_State is
    Frame : Animation_Frame_State := (others => <>);
    begin
      for Joint of Joints loop
        Adjust_Bounding (Joint.Point, Frame.Bounding);
        if Joint.Parent_Index = -1 then Frame.Skeleton.Append_Child (Frame.Skeleton.Root, Joint);
        else Frame.Skeleton.Append_Child (Frame.Skeleton.Find (Joints.Element (Int (Joint.Parent_Index + 1))), Joint); end if;
      end loop;
      return Frame;
    end;
    
  ------------
  -- Render --
  ------------

  function Render (Mesh : Skeletal_Mesh_State; Pose : Pose_State) return Vector_Mesh.Unsafe.Vector is
    Result   : Vector_Mesh.Unsafe.Vector;
    Skeleton : Treed_Joint.Unsafe.Tree;
    begin
      for Blend of Pose.Blends loop
        
      end loop;
    end;
end;
