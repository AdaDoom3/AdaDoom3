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

package body Cairo.Image_Surface is

   ----------------------------
   -- Create_For_Data_ARGB32 --
   ----------------------------

   function Create_For_Data_ARGB32
     (Data   : ARGB32_Array_Access;
      Width  : Gint;
      Height : Gint)
      return Cairo_Surface
   is
      Stride : constant Gint := Cairo_Format_Stride_For_Width
        (Format => Cairo_Format_ARGB32,
         Width  => Width);
   begin
      return Create_For_Data_Generic
        (Data (Data'First)'Address,
         Cairo_Format_ARGB32,
         Width,
         Height,
         Stride);
   end Create_For_Data_ARGB32;

   ---------------------------
   -- Create_For_Data_RGB24 --
   ---------------------------

   function Create_For_Data_RGB24
     (Data   : RGB24_Array_Access;
      Width  : Gint;
      Height : Gint)
      return Cairo_Surface
   is
      Stride : constant Gint := Cairo_Format_Stride_For_Width
        (Format => Cairo_Format_RGB24,
         Width  => Width);
   begin
      return Create_For_Data_Generic
        (Data (Data'First)'Address,
         Cairo_Format_RGB24,
         Width,
         Height,
         Stride);
   end Create_For_Data_RGB24;

   ------------------------
   -- Create_For_Data_A8 --
   ------------------------

   function Create_For_Data_A8
     (Data   : Byte_Array_Access;
      Width  : Gint;
      Height : Gint)
      return Cairo_Surface
   is
      Stride : constant Gint := Cairo_Format_Stride_For_Width
        (Format => Cairo_Format_A8,
         Width  => Width);
   begin
      return Create_For_Data_Generic
        (Data (Data'First)'Address,
         Cairo_Format_A8,
         Width,
         Height,
         Stride);
   end Create_For_Data_A8;

end Cairo.Image_Surface;
