-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

package Gdk.GL is

   type GL_Configs is new Integer;
   for GL_Configs'Size use Integer'Size;

   Gdk_GL_None : constant GL_Configs := 0;
   Gdk_GL_Use_Gl : constant GL_Configs := 1;
   Gdk_GL_Buffer_Size : constant GL_Configs := 2;
   Gdk_GL_Level : constant GL_Configs := 3;
   Gdk_GL_Rgba : constant GL_Configs := 4;
   Gdk_GL_Doublebuffer : constant GL_Configs := 5;
   Gdk_GL_Stereo : constant GL_Configs := 6;
   Gdk_GL_Aux_Buffers : constant GL_Configs := 7;
   Gdk_GL_Red_Size : constant GL_Configs := 8;
   Gdk_GL_Green_Size : constant GL_Configs := 9;
   Gdk_GL_Blue_Size : constant GL_Configs := 10;
   Gdk_GL_Alpha_Size : constant GL_Configs := 11;
   Gdk_GL_Depth_Size : constant GL_Configs := 12;
   Gdk_GL_Stencil_Size : constant GL_Configs := 13;
   Gdk_GL_Accum_Red_Size : constant GL_Configs := 14;
   Gdk_GL_Accum_Green_Size : constant GL_Configs := 15;
   Gdk_GL_Accum_Blue_Size : constant GL_Configs := 16;
   Gdk_GL_Accum_Alpha_Size : constant GL_Configs := 17;

   --  Extensions
   Gdk_GL_X_Visual_Type_Ext : constant GL_Configs := 16#22#;
   Gdk_GL_Transparent_Type_Ext : constant GL_Configs := 16#23#;
   Gdk_GL_Transparent_Index_Value_Ext : constant GL_Configs := 16#24#;
   Gdk_GL_Transparent_Red_Value_Ext : constant GL_Configs := 16#25#;
   Gdk_GL_Transparent_Green_Value_Ext : constant GL_Configs := 16#26#;
   Gdk_GL_Transparent_Blue_Value_Ext : constant GL_Configs := 16#27#;
   Gdk_GL_Transparent_Alpha_Value_Ext : constant GL_Configs := 16#28#;

   function Query return Boolean;
   --  Returns true if OpenGL is supported

end Gdk.GL;
