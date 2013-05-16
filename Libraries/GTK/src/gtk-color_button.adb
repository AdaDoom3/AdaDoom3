-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2006-2013, AdaCore                  --
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

with Gdk.Color;             use Gdk.Color;
with Gtk.Button;            use Gtk.Button;
with Interfaces.C.Strings;  use Interfaces.C.Strings;
with System;                use System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Color_Button is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Color_Button_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Button : out Gtk_Color_Button) is
   begin
      Button := new Gtk_Color_Button_Record;
      Gtk.Color_Button.Initialize (Button);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Button : access Gtk_Color_Button_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_color_button_new");
   begin
      Set_Object (Button, Internal);
   end Initialize;

   ------------------------
   -- Gtk_New_With_Color --
   ------------------------

   procedure Gtk_New_With_Color
     (Button : out Gtk_Color_Button;
      Color  : Gdk.Color.Gdk_Color) is
   begin
      Button := new Gtk_Color_Button_Record;
      Initialize_With_Color (Button, Color);
   end Gtk_New_With_Color;

   ---------------------------
   -- Initialize_With_Color --
   ---------------------------

   procedure Initialize_With_Color
     (Button : access Gtk_Color_Button_Record'Class;
      Color  : Gdk.Color.Gdk_Color)
   is
      function Internal (Color : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_color_button_new_with_color");

      C : aliased Gdk_Color := Color;
   begin
      if C /= Null_Color then
         Set_Object (Button, Internal (C'Address));
      else
         Set_Object (Button, Internal (System.Null_Address));
      end if;
   end Initialize_With_Color;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (Button : access Gtk_Color_Button_Record;
      Color  : Gdk.Color.Gdk_Color)
   is
      procedure Internal (Button : System.Address; Color : System.Address);
      pragma Import (C, Internal, "gtk_color_button_set_color");
      C : aliased Gdk_Color := Color;
   begin
      if C /= Null_Color then
         Internal (Get_Object (Button), C'Address);
      else
         Internal (Get_Object (Button), System.Null_Address);
      end if;
   end Set_Color;

   ---------------
   -- Get_Color --
   ---------------

   function Get_Color
     (Button : access Gtk_Color_Button_Record) return Gdk.Color.Gdk_Color
   is
      procedure Internal (Button : System.Address; Color : System.Address);
      pragma Import (C, Internal, "gtk_color_button_get_color");
      C : aliased Gdk_Color;
   begin
      Internal (Get_Object (Button), C'Address);
      return C;
   end Get_Color;

   ---------------
   -- Set_Alpha --
   ---------------

   procedure Set_Alpha
     (Button : access Gtk_Color_Button_Record;
      Alpha  : Guint16)
   is
      procedure Internal (Button : System.Address; Alpha : Guint16);
      pragma Import (C, Internal, "gtk_color_button_set_alpha");
   begin
      Internal (Get_Object (Button), Alpha);
   end Set_Alpha;

   ---------------
   -- Get_Alpha --
   ---------------

   function Get_Alpha
     (Button : access Gtk_Color_Button_Record)
      return Glib.Guint16
   is
      function Internal (Button : System.Address) return Guint16;
      pragma Import (C, Internal, "gtk_color_button_get_alpha");
   begin
      return Internal (Get_Object (Button));
   end Get_Alpha;

   -------------------
   -- Set_Use_Alpha --
   -------------------

   procedure Set_Use_Alpha
     (Button    : access Gtk_Color_Button_Record;
      Use_Alpha : Boolean)
   is
      procedure Internal (Button : System.Address; Use_Alpha : Gboolean);
      pragma Import (C, Internal, "gtk_color_button_set_use_alpha");
   begin
      Internal (Get_Object (Button), Boolean'Pos (Use_Alpha));
   end Set_Use_Alpha;

   -------------------
   -- Get_Use_Alpha --
   -------------------

   function Get_Use_Alpha
     (Button : access Gtk_Color_Button_Record) return Boolean
   is
      function Internal (Button : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_color_button_get_use_alpha");
   begin
      return Boolean'Val (Internal (Get_Object (Button)));
   end Get_Use_Alpha;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Button : access Gtk_Color_Button_Record;
      Title  : String)
   is
      procedure Internal (Button : System.Address; Title : String);
      pragma Import (C, Internal, "gtk_color_button_set_title");
   begin
      Internal (Get_Object (Button), Title & ASCII.NUL);
   end Set_Title;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
     (Button : access Gtk_Color_Button_Record) return String
   is
      function Internal (Button : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_color_button_get_title");
   begin
      --  Do not free returned value, this is internal to gtk+
      return Value (Internal (Get_Object (Button)));
   end Get_Title;

end Gtk.Color_Button;
