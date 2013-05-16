-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2002 ACT-Europe                 --
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

with Glib;        use Glib;
with Glib.Object; use Glib.Object;
with Pango.Font;  use Pango.Font;
with System;      use System;

package body Pango.Context is

   --------------------------
   -- Get_Font_Description --
   --------------------------

   function Get_Font_Description
     (Context : Pango_Context)
      return Pango.Font.Pango_Font_Description
   is
      function Internal (Context : System.Address)
         return Pango_Font_Description;
      pragma Import
        (C, Internal, "pango_context_get_font_description");
   begin
      return Internal (Get_Object (Context));
   end Get_Font_Description;

   --------------------------
   -- Set_Font_Description --
   --------------------------

   procedure Set_Font_Description
     (Context     : Pango_Context;
      Description : Pango.Font.Pango_Font_Description)
   is
      procedure Internal (Context : System.Address;
                          Description : Pango_Font_Description);
      pragma Import
        (C, Internal, "pango_context_set_font_description");
   begin
      Internal (Get_Object (Context), Description);
   end Set_Font_Description;

   ---------------
   -- Load_Font --
   ---------------

   function Load_Font
     (Context : access Pango_Context_Record'Class;
      Descr   : Pango.Font.Pango_Font_Description)
      return Pango.Font.Pango_Font
   is
      function Internal (Context : System.Address;
                         Descr   : Pango_Font_Description)
         return System.Address;
      pragma Import (C, Internal, "pango_context_load_font");
      Stub : Pango_Font_Record;
   begin
      return Pango_Font
        (Get_User_Data (Internal (Get_Object (Context), Descr), Stub));
   end Load_Font;

end Pango.Context;
