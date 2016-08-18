
--                                                                                                                    
--                                                              N E O  E N G I N E                                                    
--                                                                                                                    
--                                                      Copyright (C) 2016 Justin Squirek                                          
-- 
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any later version. 
--                                                                                                                    
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.         
--                                                                                                                    
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses     
--

separate (Neo.Model) package OBJ is

  -- This parser is used for the loading of static scene meshes only and applies 
  -- * Free-form curves and surfaces (parameter space vertices)
  -- * General statements

  ----------
  -- Load --
  ----------

  function Load (Name : Str_16) return Mesh_State is
    package Mesh_Parser is new Parser (Name, "#"); use Mesh_Parser;
    Mesh : Mesh_State (Animated => False);
    begin
      while not At_EOF loop
        case Peek is
          when "o"      => Mesh.Groups.Add (Trim (Next_Line));
          when "v"      => Mesh.Groups.Last.Add (Next, Next, Next);
          when "vt"     => Mesh.Groups.Last.Add (Next, Next); Skip;
          when "vn"     => Mesh.Groups.Last.
          when "f"      => Mesh.
          when "usemtl" => 
          when "mtllib" =>
        when others => null; end case; -- Ignore unsupported and hope for the best
      end loop;
    end;

  ----------
  -- Save --
  ----------

  procedure Save (Name : Str_16; Mesh : Mesh_State) is
    begin

    end;
end;