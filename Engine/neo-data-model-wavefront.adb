
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

-- Wavefront OBJ mesh loader
separate (Neo.Data.Model) package body Wavefront is

  ----------
  -- Mesh --
  ----------

  function Load (Path : Str) return Skeletal_Mesh_State is

    -- Load an obj mesh: https://web.archive.org/web/20160810123453/https://www.cs.utah.edu/~boulos/cs3505/obj_spec.pdf
    package Mesh_Parser is new Parser (Path, Comment => "#"); use Mesh_Parser;

    -- # 3ds Max Wavefront OBJ Exporter v0.97b
    -- # File Created: 26.02.2011 09:47:12
    -- 
    -- mtllib enemy 1 i phone.mtl
    -- 
    -- #
    -- # object default
    -- #
    -- 
    -- v -135.4830 297.2050 23.1310
    -- v -365.3680 261.2659 11.0974
    -- v -336.4890 186.0040 86.3631
    -- # 162 vertices
    -- 
    -- vn -0.1249 0.8332 0.5387
    -- vn -0.2482 0.8568 0.4520
    -- vn -0.1928 0.6503 0.7348
    -- # 365 vertex normals
    -- 
    -- vt 0.9341 0.0496 0.0000
    -- vt 0.0000 0.0000 0.0000
    -- vt 0.0296 0.1190 0.0000
    -- # 175 texture coords
    -- 
    -- g default
    -- usemtl 07___Default
    -- s 1
    -- f 1/1/1 2/2/2 3/3/3 4/4/4 
    -- s 2
    -- f 3/5/5 5/6/6 6/7/7 4/8/8 
    -- s 1
    -- f 7/9/9 8/10/10 2/2/2 
    -- f 9/11/11 10/12/12 11/13/13 12/14/14 
    -- s 4
    -- # 56 polygons - 208 triangles
    -- 
    -- #
    -- # object Object03
    -- #
    -- 
    -- v -604.2183 156.1393 17.1663
    -- v -597.2630 169.2069 19.4969
    -- v -598.0356 169.2363 16.9821
    -- # 162 vertices
    -- 
    -- vn -0.1249 0.8332 0.5387
    -- vn -0.2482 0.8568 0.4520
    -- vn -0.1928 0.6503 0.7348
    -- # 365 vertex normals
    -- 
    -- vt 0.9341 0.0496 0.0000
    -- vt 0.0000 0.0000 0.0000
    -- vt 0.0296 0.1190 0.0000
    -- # 175 texture coords
    -- 
    -- g Object03
    -- usemtl wire_000000000
    -- s off
    -- f 1194/515/1040 1195/516/1040 1196/517/1040 1197/518/1040 
    -- f 1198/519/1041 1199/520/1041 1197/518/1041 1196/517/1041 
    -- f 1199/518/1042 1200/515/1042 1194/515/1042 1197/518/1042 
    -- s 1
    -- f 1210/529/1046 1207/530/1047 1206/526/1048 1211/531/1049 
    -- f 1205/532/1050 1212/533/1050 1213/534/1051 1202/535/1052 1205/532/1052 
    -- # 32 polygons

    Meshes   : Vector_Mesh_State.Unsafe.Vector;
    Indicies : Vector_Int_32_Natural.Unsafe.Vector;
    Normals  : Vector_Point_3D.Unsafe.Vector;
    Texture  : Vector_Point_2D.Unsafe.Vector;
    Vertex   : Vertex_State (Has_Weights => True);
    Index    : Int_32_Natural;
    Mesh     : Mesh_State;
    begin

      -- Handle groups
      while not At_EOF loop

        -- Load verticies
        while Peek = "v" loop Skip; 
          Mesh.Verticies.Append ((Next, Next, Next))
        end loop;
        Assert (Mesh.Verticies.Length > 0);

        -- Load vertex normals
        while Peek = "vn" loop Skip;
          Normals.Append ((Next, Next, Next));
        end loop
        Assert (Normals.Length > 0);

        -- Load vertex texture coordinates
        while Peek = "vt" loop Skip;
          Texture.Append ((Next, Next)); Skip; -- Skip the weight
        end loop
        Assert (Texture.Length > 0);
        
        -- Load material
        Skip_Until ("usemtl", Fail_On_EOF => True);
        Mesh.Material := Next;

        -- Load and build triangles 
        while not At_EOF loop
          Skip_Until ("f", "v");
          exit when Peek = "v"; Skip;

          -- Load polygon face
          while not At_EOL loop
            Index  := Next; Assert ("/"); 
            Vertex := Mesh.Verticies.Element (Index);
            if Vertex.Texture = (0.0, 0.0) then Index := -1; end if;
            Vertex.Normal  := Normals.Element (Next); Assert ("/");
            Vertex.Texture := Texture.Element (Next);

            -- Duplicate verticies when different normals or texture points are encountered for the same vertex
            if Index = -1 then
              Mesh.Verticies.Append (Vertex);
              Indicies.Append (Mesh.Verticies.Last_Index - 1);
            else
              Mesh.Verticies.Replace (Index, Vertex);
              Indicies.Append (Index - 1);
            end if;
          end loop;
          Assert (Indicies.Length > 0);

          -- Add resulting indicies
          Mesh.Indicies.Append (To_Triangles (Indicies));
          Indicies.Clear;
        end loop;
        Assert (Mesh.Indicies.Length > 0);

        -- Add to result
        Meshes.Append (Mesh);
        Normals.Clear;
        Texture.Clear;
        Mesh.Verticies.Clear;
        Mesh.Indicies.Clear;
      end loop;
      return Meshes;
    end;
end;