
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
separate (Neo.Engine.Renderer) package body Raytrace is

 -----------------
 -- Build_Frame --
 -----------------

 procedure Build_Frame (Frame : in out Framebuffer_State; View : View_State) is
   begin null;
   end;

 ----------------
 -- Build_View --
 ----------------

 function Build_View return View_State is
   begin
     return (others => <>);
   end;
end;
