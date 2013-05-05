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

package body Gtk.Size_Group is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Size_Group_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Size_Group : out Gtk_Size_Group;
       Mode       : Size_Group_Mode := Both)
   is
   begin
      Size_Group := new Gtk_Size_Group_Record;
      Gtk.Size_Group.Initialize (Size_Group, Mode);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Size_Group : access Gtk_Size_Group_Record'Class;
       Mode       : Size_Group_Mode := Both)
   is
      function Internal (Mode : Size_Group_Mode) return System.Address;
      pragma Import (C, Internal, "gtk_size_group_new");
   begin
      Set_Object (Size_Group, Internal (Mode));
   end Initialize;

   ----------------
   -- Add_Widget --
   ----------------

   procedure Add_Widget
      (Size_Group : access Gtk_Size_Group_Record;
       Widget     : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Size_Group : System.Address;
          Widget     : System.Address);
      pragma Import (C, Internal, "gtk_size_group_add_widget");
   begin
      Internal (Get_Object (Size_Group), Get_Object (Widget));
   end Add_Widget;

   -----------------------
   -- Get_Ignore_Hidden --
   -----------------------

   function Get_Ignore_Hidden
      (Size_Group : access Gtk_Size_Group_Record) return Boolean
   is
      function Internal (Size_Group : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_size_group_get_ignore_hidden");
   begin
      return Boolean'Val (Internal (Get_Object (Size_Group)));
   end Get_Ignore_Hidden;

   --------------
   -- Get_Mode --
   --------------

   function Get_Mode
      (Size_Group : access Gtk_Size_Group_Record) return Size_Group_Mode
   is
      function Internal (Size_Group : System.Address) return Size_Group_Mode;
      pragma Import (C, Internal, "gtk_size_group_get_mode");
   begin
      return Internal (Get_Object (Size_Group));
   end Get_Mode;

   -----------------
   -- Get_Widgets --
   -----------------

   function Get_Widgets
      (Size_Group : access Gtk_Size_Group_Record)
       return Gtk.Widget.Widget_SList.GSList
   is
      function Internal (Size_Group : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_size_group_get_widgets");
      Tmp_Return : Gtk.Widget.Widget_SList.GSList;
   begin
      Gtk.Widget.Widget_SList.Set_Object (Tmp_Return, Internal (Get_Object (Size_Group)));
      return Tmp_Return;
   end Get_Widgets;

   -------------------
   -- Remove_Widget --
   -------------------

   procedure Remove_Widget
      (Size_Group : access Gtk_Size_Group_Record;
       Widget     : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Size_Group : System.Address;
          Widget     : System.Address);
      pragma Import (C, Internal, "gtk_size_group_remove_widget");
   begin
      Internal (Get_Object (Size_Group), Get_Object (Widget));
   end Remove_Widget;

   -----------------------
   -- Set_Ignore_Hidden --
   -----------------------

   procedure Set_Ignore_Hidden
      (Size_Group    : access Gtk_Size_Group_Record;
       Ignore_Hidden : Boolean)
   is
      procedure Internal
         (Size_Group    : System.Address;
          Ignore_Hidden : Integer);
      pragma Import (C, Internal, "gtk_size_group_set_ignore_hidden");
   begin
      Internal (Get_Object (Size_Group), Boolean'Pos (Ignore_Hidden));
   end Set_Ignore_Hidden;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
      (Size_Group : access Gtk_Size_Group_Record;
       Mode       : Size_Group_Mode)
   is
      procedure Internal
         (Size_Group : System.Address;
          Mode       : Size_Group_Mode);
      pragma Import (C, Internal, "gtk_size_group_set_mode");
   begin
      Internal (Get_Object (Size_Group), Mode);
   end Set_Mode;

end Gtk.Size_Group;
