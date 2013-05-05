-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2011-2013, AdaCore               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

package body Cairo.SVG is

   ------------
   -- Create --
   ------------

   function Create
     (Filename        : String;
      Width_In_Point  : Gdouble;
      Height_In_Point : Gdouble)
      return Cairo_Surface
   is
      function C_Internal
        (Filename : System.Address;
         W, H     : Gdouble) return Cairo_Surface;
      pragma Import (C, C_Internal, "cairo_svg_surface_create");

      S : constant String := Filename & ASCII.NUL;
   begin
      return C_Internal (S (S'First)'Address, Width_In_Point, Height_In_Point);
   end Create;

end Cairo.SVG;
