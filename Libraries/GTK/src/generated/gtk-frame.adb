-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2013, AdaCore                   --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Frame is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Frame_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Frame : out Gtk_Frame; Label : UTF8_String := "") is
   begin
      Frame := new Gtk_Frame_Record;
      Gtk.Frame.Initialize (Frame, Label);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Frame : access Gtk_Frame_Record'Class;
       Label : UTF8_String := "")
   is
      function Internal
         (Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_frame_new");
      Tmp_Label  : Interfaces.C.Strings.chars_ptr;
      Tmp_Return : System.Address;
   begin
      if Label = "" then
         Tmp_Label := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      Tmp_Return := Internal (Tmp_Label);
      Free (Tmp_Label);
      Set_Object (Frame, Tmp_Return);
   end Initialize;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (Frame : access Gtk_Frame_Record) return UTF8_String is
      function Internal
         (Frame : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_frame_get_label");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Frame)));
   end Get_Label;

   ---------------------
   -- Get_Label_Align --
   ---------------------

   procedure Get_Label_Align
      (Frame  : access Gtk_Frame_Record;
       Xalign : out Gfloat;
       Yalign : out Gfloat)
   is
      procedure Internal
         (Frame  : System.Address;
          Xalign : out Gfloat;
          Yalign : out Gfloat);
      pragma Import (C, Internal, "gtk_frame_get_label_align");
   begin
      Internal (Get_Object (Frame), Xalign, Yalign);
   end Get_Label_Align;

   ----------------------
   -- Get_Label_Widget --
   ----------------------

   function Get_Label_Widget
      (Frame : access Gtk_Frame_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Frame : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_frame_get_label_widget");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Frame)), Stub));
   end Get_Label_Widget;

   ---------------------
   -- Get_Shadow_Type --
   ---------------------

   function Get_Shadow_Type
      (Frame : access Gtk_Frame_Record) return Gtk.Enums.Gtk_Shadow_Type
   is
      function Internal (Frame : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_frame_get_shadow_type");
   begin
      return Gtk.Enums.Gtk_Shadow_Type'Val (Internal (Get_Object (Frame)));
   end Get_Shadow_Type;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
      (Frame : access Gtk_Frame_Record;
       Label : UTF8_String)
   is
      procedure Internal
         (Frame : System.Address;
          Label : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_frame_set_label");
      Tmp_Label : Interfaces.C.Strings.chars_ptr := New_String (Label);
   begin
      Internal (Get_Object (Frame), Tmp_Label);
      Free (Tmp_Label);
   end Set_Label;

   ---------------------
   -- Set_Label_Align --
   ---------------------

   procedure Set_Label_Align
      (Frame  : access Gtk_Frame_Record;
       Xalign : Gfloat;
       Yalign : Gfloat)
   is
      procedure Internal
         (Frame  : System.Address;
          Xalign : Gfloat;
          Yalign : Gfloat);
      pragma Import (C, Internal, "gtk_frame_set_label_align");
   begin
      Internal (Get_Object (Frame), Xalign, Yalign);
   end Set_Label_Align;

   ----------------------
   -- Set_Label_Widget --
   ----------------------

   procedure Set_Label_Widget
      (Frame        : access Gtk_Frame_Record;
       Label_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Frame        : System.Address;
          Label_Widget : System.Address);
      pragma Import (C, Internal, "gtk_frame_set_label_widget");
   begin
      Internal (Get_Object (Frame), Get_Object (Label_Widget));
   end Set_Label_Widget;

   ---------------------
   -- Set_Shadow_Type --
   ---------------------

   procedure Set_Shadow_Type
      (Frame    : access Gtk_Frame_Record;
       The_Type : Gtk.Enums.Gtk_Shadow_Type)
   is
      procedure Internal (Frame : System.Address; The_Type : Integer);
      pragma Import (C, Internal, "gtk_frame_set_shadow_type");
   begin
      Internal (Get_Object (Frame), Gtk.Enums.Gtk_Shadow_Type'Pos (The_Type));
   end Set_Shadow_Type;

end Gtk.Frame;
