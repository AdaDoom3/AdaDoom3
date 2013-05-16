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
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Gtk.Buildable is
   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child
      (Self     : Gtk_Buildable;
       Builder  : access Gtk.Builder.Gtk_Builder_Record'Class;
       Child    : access Glib.Object.GObject_Record'Class;
       The_Type : UTF8_String)
   is
      procedure Internal
         (Self     : Gtk_Buildable;
          Builder  : System.Address;
          Child    : System.Address;
          The_Type : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_buildable_add_child");
      Tmp_The_Type : Interfaces.C.Strings.chars_ptr := New_String (The_Type);
   begin
      Internal (Self, Get_Object (Builder), Get_Object (Child), Tmp_The_Type);
      Free (Tmp_The_Type);
   end Add_Child;

   ---------------------
   -- Construct_Child --
   ---------------------

   function Construct_Child
      (Self    : Gtk_Buildable;
       Builder : access Gtk.Builder.Gtk_Builder_Record'Class;
       Name    : UTF8_String) return Glib.Object.GObject
   is
      function Internal
         (Self    : Gtk_Buildable;
          Builder : System.Address;
          Name    : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_buildable_construct_child");
      Tmp_Name   : Interfaces.C.Strings.chars_ptr := New_String (Name);
      Stub       : Glib.Object.GObject_Record;
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Self, Get_Object (Builder), Tmp_Name);
      Free (Tmp_Name);
      return Get_User_Data (Tmp_Return, Stub);
   end Construct_Child;

   ---------------------
   -- Custom_Finished --
   ---------------------

   procedure Custom_Finished
      (Self    : Gtk_Buildable;
       Builder : access Gtk.Builder.Gtk_Builder_Record'Class;
       Child   : access Glib.Object.GObject_Record'Class;
       Tagname : UTF8_String;
       Data    : System.Address)
   is
      procedure Internal
         (Self    : Gtk_Buildable;
          Builder : System.Address;
          Child   : System.Address;
          Tagname : Interfaces.C.Strings.chars_ptr;
          Data    : System.Address);
      pragma Import (C, Internal, "gtk_buildable_custom_finished");
      Tmp_Tagname : Interfaces.C.Strings.chars_ptr := New_String (Tagname);
   begin
      Internal (Self, Get_Object (Builder), Get_Object (Child), Tmp_Tagname, Data);
      Free (Tmp_Tagname);
   end Custom_Finished;

   --------------------
   -- Custom_Tag_End --
   --------------------

   procedure Custom_Tag_End
      (Self    : Gtk_Buildable;
       Builder : access Gtk.Builder.Gtk_Builder_Record'Class;
       Child   : access Glib.Object.GObject_Record'Class;
       Tagname : UTF8_String;
       Data    : System.Address)
   is
      procedure Internal
         (Self    : Gtk_Buildable;
          Builder : System.Address;
          Child   : System.Address;
          Tagname : Interfaces.C.Strings.chars_ptr;
          Data    : System.Address);
      pragma Import (C, Internal, "gtk_buildable_custom_tag_end");
      Tmp_Tagname : Interfaces.C.Strings.chars_ptr := New_String (Tagname);
   begin
      Internal (Self, Get_Object (Builder), Get_Object (Child), Tmp_Tagname, Data);
      Free (Tmp_Tagname);
   end Custom_Tag_End;

   ------------------------
   -- Get_Internal_Child --
   ------------------------

   function Get_Internal_Child
      (Self      : Gtk_Buildable;
       Builder   : access Gtk.Builder.Gtk_Builder_Record'Class;
       Childname : UTF8_String) return Glib.Object.GObject
   is
      function Internal
         (Self      : Gtk_Buildable;
          Builder   : System.Address;
          Childname : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_buildable_get_internal_child");
      Tmp_Childname : Interfaces.C.Strings.chars_ptr := New_String (Childname);
      Stub          : Glib.Object.GObject_Record;
      Tmp_Return    : System.Address;
   begin
      Tmp_Return := Internal (Self, Get_Object (Builder), Tmp_Childname);
      Free (Tmp_Childname);
      return Get_User_Data (Tmp_Return, Stub);
   end Get_Internal_Child;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Self : Gtk_Buildable) return UTF8_String is
      function Internal
         (Self : Gtk_Buildable) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_buildable_get_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Self));
   end Get_Name;

   ---------------------
   -- Parser_Finished --
   ---------------------

   procedure Parser_Finished
      (Self    : Gtk_Buildable;
       Builder : access Gtk.Builder.Gtk_Builder_Record'Class)
   is
      procedure Internal (Self : Gtk_Buildable; Builder : System.Address);
      pragma Import (C, Internal, "gtk_buildable_parser_finished");
   begin
      Internal (Self, Get_Object (Builder));
   end Parser_Finished;

   ----------------------------
   -- Set_Buildable_Property --
   ----------------------------

   procedure Set_Buildable_Property
      (Self    : Gtk_Buildable;
       Builder : access Gtk.Builder.Gtk_Builder_Record'Class;
       Name    : UTF8_String;
       Value   : out Glib.Values.GValue)
   is
      procedure Internal
         (Self    : Gtk_Buildable;
          Builder : System.Address;
          Name    : Interfaces.C.Strings.chars_ptr;
          Value   : out Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_buildable_set_buildable_property");
      Tmp_Name : Interfaces.C.Strings.chars_ptr := New_String (Name);
   begin
      Internal (Self, Get_Object (Builder), Tmp_Name, Value);
      Free (Tmp_Name);
   end Set_Buildable_Property;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Self : Gtk_Buildable; Name : UTF8_String) is
      procedure Internal
         (Self : Gtk_Buildable;
          Name : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_buildable_set_name");
      Tmp_Name : Interfaces.C.Strings.chars_ptr := New_String (Name);
   begin
      Internal (Self, Tmp_Name);
      Free (Tmp_Name);
   end Set_Name;

end Gtk.Buildable;
