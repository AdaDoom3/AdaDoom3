-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2003 ACT-Europe                 --
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

with Interfaces.C.Strings;
with Glib; use Glib;

package body Gdk.Window_Attr is

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Window_Attr : in out Gdk_Window_Attr) is
      procedure Internal (Window_Attr : Gdk_Window_Attr);
      pragma Import (C, Internal, "ada_gdk_window_attr_destroy");

   begin
      Internal (Window_Attr);
      Window_Attr := Null_Window_Attr;
   end Destroy;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New
     (Window_Attr       : out Gdk_Window_Attr;
      Title             : UTF8_String := "";
      Event_Mask        : Gdk.Event.Gdk_Event_Mask := 0;
      X, Y              : Glib.Gint := 0;
      Width             : Glib.Gint := 0;
      Height            : Glib.Gint := 0;
      Wclass            : Gdk.Window.Gdk_Window_Class :=
        Gdk.Window.Input_Output;
      Visual            : Gdk.Visual.Gdk_Visual := null;
      Colormap          : Gdk.Color.Gdk_Colormap := null;
      Window_Type       : Gdk.Window.Gdk_Window_Type :=
        Gdk.Window.Window_Root;
      Cursor            : Gdk.Cursor.Gdk_Cursor := null;
      Wmclass_Name      : String := "";
      Wmclass_Class     : String := "";
      Override_Redirect : Boolean := True)
   is
      function Internal return Gdk_Window_Attr;
      pragma Import (C, Internal, "ada_gdk_window_attr_new");

   begin
      Window_Attr := Internal;

      Set_Title (Window_Attr, Title);
      Set_Event_Mask (Window_Attr, Event_Mask);
      Set_X (Window_Attr, X);
      Set_Y (Window_Attr, Y);
      Set_Width (Window_Attr, Width);
      Set_Height (Window_Attr, Height);
      Set_Window_Class (Window_Attr, Wclass);
      Set_Visual (Window_Attr, Visual);
      Set_Colormap (Window_Attr, Colormap);
      Set_Window_Type (Window_Attr, Window_Type);
      Set_Cursor (Window_Attr, Cursor);
      Set_Wmclass_Name (Window_Attr, Wmclass_Name);
      Set_Wmclass_Class (Window_Attr, Wmclass_Class);
      Set_Override_Redirect (Window_Attr, Override_Redirect);
   end Gdk_New;

   ---------------------------
   -- Get_Override_Redirect --
   ---------------------------

   function Get_Override_Redirect
     (Window_Attr : Gdk_Window_Attr) return Boolean
   is
      function Internal (Window_Attr : Gdk_Window_Attr) return Glib.Gboolean;
      pragma Import (C, Internal, "ada_gdk_window_attr_get_override_redirect");

   begin
      return Internal (Window_Attr) /= 0;
   end Get_Override_Redirect;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title (Window_Attr : Gdk_Window_Attr) return UTF8_String is
      function Internal
        (Window_Attr : Gdk_Window_Attr) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_gdk_window_attr_get_title");

   begin
      return Interfaces.C.Strings.Value (Internal (Window_Attr));
   end Get_Title;

   -----------------------
   -- Get_Wmclass_Class --
   -----------------------

   function Get_Wmclass_Class
     (Window_Attr : Gdk_Window_Attr) return String
   is
      function Internal
        (Window_Attr : Gdk_Window_Attr) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_gdk_window_attr_get_wmclass_class");

   begin
      return Interfaces.C.Strings.Value (Internal (Window_Attr));
   end Get_Wmclass_Class;

   ----------------------
   -- Get_Wmclass_Name --
   ----------------------

   function Get_Wmclass_Name
     (Window_Attr : Gdk_Window_Attr) return String
   is
      function Internal
        (Window_Attr : Gdk_Window_Attr) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_gdk_window_attr_get_wmclass_name");

   begin
      return Interfaces.C.Strings.Value (Internal (Window_Attr));
   end Get_Wmclass_Name;

   ---------------------------
   -- Set_Override_Redirect --
   ---------------------------

   procedure Set_Override_Redirect
     (Window_Attr       : Gdk_Window_Attr;
      Override_Redirect : Boolean)
   is
      procedure Internal
        (Window_Attr       : Gdk_Window_Attr;
         Override_Redirect : Glib.Gboolean);
      pragma Import (C, Internal, "ada_gdk_window_attr_set_override_redirect");

   begin
      Internal (Window_Attr, Boolean'Pos (Override_Redirect));
   end Set_Override_Redirect;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Window_Attr : Gdk_Window_Attr;
      Title       : UTF8_String)
   is
      procedure Internal
        (Window_Attr : Gdk_Window_Attr;
         Title       : UTF8_String);
      pragma Import (C, Internal, "ada_gdk_window_attr_set_title");

   begin
      Internal (Window_Attr, Title & ASCII.NUL);
   end Set_Title;

   -----------------------
   -- Set_Wmclass_Class --
   -----------------------

   procedure Set_Wmclass_Class
     (Window_Attr   : Gdk_Window_Attr;
      Wmclass_Class : String)
   is
      procedure Internal
        (Window_Attr   : Gdk_Window_Attr;
         Wmclass_Class : String);
      pragma Import (C, Internal, "ada_gdk_window_attr_set_wmclass_class");

   begin
      Internal (Window_Attr, Wmclass_Class & ASCII.NUL);
   end Set_Wmclass_Class;

   ----------------------
   -- Set_Wmclass_Name --
   ----------------------

   procedure Set_Wmclass_Name
     (Window_Attr  : Gdk_Window_Attr;
      Wmclass_Name : String)
   is
      procedure Internal
        (Window_Attr  : Gdk_Window_Attr;
         Wmclass_Name : String);
      pragma Import (C, Internal, "ada_gdk_window_attr_set_wmclass_name");

   begin
      Internal (Window_Attr, Wmclass_Name & ASCII.NUL);
   end Set_Wmclass_Name;

end Gdk.Window_Attr;
