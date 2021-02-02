
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

with Neo.World.CVars;    use Neo.World.CVars;
with Neo.World.Graphics; use Neo.World.Graphics;

with Neo.Core.Math;
use Neo.Core.Math.Calc_32;

package body Neo.World is
  use Geometry_Name_Str;

  ------------
  -- Camera --
  ------------

  protected body Safe_Camera is
    procedure Set_Position (Val : Vector_3D) is begin Position := Val; Update; end;
    procedure Set_Yaw      (Val : Real)      is begin Yaw      := Val; Update; end;
    procedure Set_Pitch    (Val : Real)      is begin Pitch    := Val; Update; end;
    function View return Matrix_4D is (Look_At (Position, Position + Direction, Up));
    procedure Move (Dir : Direction_Kind; Start_Time : Time) is
      begin
        case Dir is
          when Backward_Direction => Position := Position + (Direction * Real_32 (Clock - Start_Time)  * Move_Speed);
          when Forward_Direction  => Position := Position - (Direction * Real_32 (Clock - Start_Time)  * Move_Speed);
          when Right_Direction    => Position := Position - (Normal (Cross (Direction, Up)) * Real_32 (Clock - Start_Time) * Move_Speed * 3.0);
          when Left_Direction     => Position := Position + (Normal (Cross (Direction, Up)) * Real_32 (Clock - Start_Time) * Move_Speed * 3.0);
        end case;
      end;
    procedure Look (X_Offset, Y_Offset : Int; Clamp : Bool := True) is
      begin
        Yaw   := Yaw   + Sensitivity * (Real_32 (X_Offset) * (100.0 / Real_32 (Window_Width.Get)));
        Pitch := Pitch + Sensitivity * (Real_32 (Y_Offset) * (100.0 / Real_32 (Window_Height.Get)));
        if Clamp then
          if Pitch > (PI / 2.0) - 0.01 then
            Pitch := (PI / 2.0) - 0.01;
          end if;
          if Pitch < -(PI / 2.0) + 0.01 then
            Pitch := -(PI / 2.0) + 0.01;
          end if;
        end if;
        Update;
      end;
    procedure Update is
      begin
        Direction := Normal (Vector_3D'(X => Cos (Yaw) * Cos (Pitch),
                                        Y => Sin (Yaw) * Cos (Pitch),
                                        Z => Sin (Pitch)));
        Right := Cross (Direction, World_Up);
        Up    := Normal (Cross (Right, Direction));
      end;
  end;

  -----------
  -- Scene --
  -----------


  procedure Load_Map (Path : Str; Memory : Memory_Kind) is
    Map : Ptr_Map_State;
    begin
      Load_Map (S (OS_Info.App_Path) & PATH_MAPS & Path, Map);
      if Memory /= CPU_Memory then
        for Entity of Map.Entities loop
          if Entity.Mesh /= null then
            null;
          end if;
        end loop;
        --if Memory = GPU_Memroy then
        --  null;
        --end if;
      end if;
    end;
  procedure Load_Model (Path : Str; Memory : Memory_Kind) is
--      Mesh : Ptr_Mesh_State;
    begin
--        Load (S (OS_Info.App_Path) & PATH_MODELS & Path, Mesh);
--        Meshes.Insert (U (Path), Mesh);
--        Buffer_Mesh (Path, Mesh);
      null;
    end;
  procedure Free_Map (Path : Str; Memory : Memory_Kind) is
    begin
      null;
    end;
  procedure Free_Model (Path : Str; Memory : Memory_Kind) is
    begin
      null;
    end;


  procedure Initialize_Map (Path : Str) is
    Map  : Ptr_Map_State;
    --Mesh : Ptr_Mesh_State;
    Pos  : Point_3D;

    Start    : Ada.Calendar.Time;
    begin
      Load_Map (S (OS_Info.App_Path) & PATH_MAPS & Path, Map);

      -- Buffer entities
      --for Entity of Map.Entities loop
      --  if Entity.Mesh /= null then
      --    Line ("Buffer geometry: " & Geometry_Name_Str.S (Entity.Mesh.Name));
          --Buffer_Mesh (Path, Entity.Mesh);
        --elsif Entity.Mesh.Name /= Geometry_Name_Str.NULL_STR then
      --    Load_Mesh (S (Entity.Mesh.Name), Mesh);
      --    Meshes.Insert (S (Entity.Mesh.Name), Mesh);
      --    Buffer_Mesh (S (Entity.Mesh.Name), Entity.Mesh);
      --    Entity.Mesh := Mesh;
      --  end if;
      --end loop;

      -- Buffer geometry
      for Geometry_Mesh of Map.Geometry loop
        if Geometry_Mesh /= null then
          Start := Clock;
          Buffer_Mesh (Geometry_Name_Str.S (Geometry_Mesh.Name), Geometry_Mesh);
          Line ("Buffered " & Geometry_Name_Str.S (Geometry_Mesh.Name) & " in" & Duration'Wide_Image (Clock - Start) & "s");
        end if;
      end loop;

      -- Save map state
      World.Map.Set (Map);

      -- Setup camera
      Pos   := Map.Entities.Element (U ("info_player_start_1")).Origin;
      Pos.Z := Pos.Z + 90.0; -- Add some height so we aren't on the floor
      Camera.Set_Position (To_Vector_3D (Pos));
      In_Game.Occupied (True);
    end;

  procedure Initialize_Materials is
    Directory : Directory_Entry_Type;
    Materials : Hashed_Material.Unsafe.Map;
    Search    : Search_Type;
    begin
      Start_Search (Search, To_Str_8 (S (OS_Info.App_Path) & PATH_MATERIALS), "*.ogex");
      loop
        Get_Next_Entry (Search, Directory);
        Load_Materials (To_Str_16 (Full_Name (Directory)), Materials);
        World.Materials.Set (Materials);
        exit when not More_Entries (Search);
      end loop;
    end;
  procedure Finalize_Map is
    begin
      null;
    end;
end;
