-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010-2013, AdaCore               --
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

package body Cairo.Png is

   ------------------
   -- Write_To_Png --
   ------------------

   function Write_To_Png
     (Surface  : Cairo_Surface;
      Filename : String)
      return Cairo_Status
   is
      function C_Cairo_Surface_Write_To_Png
        (Surface  : Cairo_Surface;
         Filename : System.Address) return Cairo_Status;
      pragma Import
        (C,
         C_Cairo_Surface_Write_To_Png,
         "cairo_surface_write_to_png");

      S : constant String := Filename & ASCII.NUL;
   begin
      return C_Cairo_Surface_Write_To_Png
        (Surface, S (S'First)'Address);

   end Write_To_Png;

   ---------------------
   -- Create_From_Png --
   ---------------------

   function Create_From_Png
     (Filename : String)
      return     Cairo_Surface
   is
      function C_Cairo_Image_Surface_Create_From_Png
        (Filename : System.Address) return Cairo_Surface;

      pragma Import
        (C,
         C_Cairo_Image_Surface_Create_From_Png,
         "cairo_image_surface_create_from_png");

      S : constant String := Filename & ASCII.NUL;
   begin
      return C_Cairo_Image_Surface_Create_From_Png (S (S'First)'Address);
   end Create_From_Png;

end Cairo.Png;
