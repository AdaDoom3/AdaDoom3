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

package Cairo.PDF is

   function Create
     (Filename         : String;
      Width_In_Points  : Gdouble;
      Height_In_Points : Gdouble) return Cairo_Surface;
   --  Creates a PDF surface of the specified size in points to be written to
   --  filename.
   --
   --  filename :
   --   a filename for the PDF output (must be writable), NULL may be used to
   --   specify no output. This will generate a PDF surface that may be queried
   --   and used as a source, without generating a temporary file.
   --  width_in_points:
   --   width of the surface, in points (1 point == 1/72.0 inch)
   --  height_in_points:
   --   height of the surface, in points (1 point == 1/72.0 inch)
   --  Returns:
   --   a pointer to the newly created surface. The caller owns the surface and
   --   should call cairo_surface_destroy() when done with it. This function
   --   always returns a valid pointer, but it will return a pointer to a "nil"
   --   surface if an error such as out of memory occurs. You can use
   --   cairo_surface_status() to check for this.
   --
   --  Since 1.2

end Cairo.PDF;
