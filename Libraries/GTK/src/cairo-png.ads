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

--  <description>
--  Reading and writing PNG images.
--  </description>
--
--  <c_version>1.8.8</c_version>
--  <group>Cairo</group>

package Cairo.Png is

   function Write_To_Png
     (Surface  : Cairo_Surface;
      Filename : String)
      return     Cairo_Status;
   --  Surface: a Cairo_Surface with pixel contents
   --  Filename: the name of a file to write to
   --
   --  Writes the contents of surface to a new file filename as a PNG
   --  image.
   --
   --  Return value: Cairo_Status_Success if the PNG file was written
   --  successfully. Otherwise, Cairo_Status_No_Memory if memory could not
   --  be allocated for the operation or
   --  Cairo_Status_Surface_Type_Mismatch if the surface does not have
   --  pixel contents, or Cairo_Status_Write_Error if an I/O error occurs
   --  while attempting to write the file.

   function Create_From_Png
     (Filename : String)
      return     Cairo_Surface;
   --  Filename: name of PNG file to load
   --
   --  Creates a new image surface and initializes the contents to the
   --  given PNG file.
   --
   --  Return value: a new Cairo_Surface initialized with the contents
   --  of the PNG file, or a "nil" surface if any error occurred. A nil
   --  surface can be checked for with Cairo.Surface.Status (Surface) which
   --  may return one of the following values:
   --
   --  Cairo_Status_No_Memory
   --  Cairo_Status_File_Not_Found
   --  Cairo_Status_Read_Error
   --
   --  Alternatively, you can allow errors to propagate through the drawing
   --  operations and check the status on the context upon completion
   --  using Cairo_Status.

end Cairo.Png;
