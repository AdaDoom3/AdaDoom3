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

package body Gtk.Box is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Box_Record);
   pragma Unreferenced (Type_Conversion);

   ------------------
   -- Gtk_New_Hbox --
   ------------------

   procedure Gtk_New_Hbox
      (Box         : out Gtk_Hbox;
       Homogeneous : Boolean := False;
       Spacing     : Gint := 0)
   is
   begin
      Box := new Gtk_Hbox_Record;
      Gtk.Box.Initialize_Hbox (Box, Homogeneous, Spacing);
   end Gtk_New_Hbox;

   ------------------
   -- Gtk_New_Vbox --
   ------------------

   procedure Gtk_New_Vbox
      (Box         : out Gtk_Vbox;
       Homogeneous : Boolean := False;
       Spacing     : Gint := 0)
   is
   begin
      Box := new Gtk_Vbox_Record;
      Gtk.Box.Initialize_Vbox (Box, Homogeneous, Spacing);
   end Gtk_New_Vbox;

   ---------------------
   -- Initialize_Hbox --
   ---------------------

   procedure Initialize_Hbox
      (Box         : access Gtk_Hbox_Record'Class;
       Homogeneous : Boolean := False;
       Spacing     : Gint := 0)
   is
      function Internal
         (Homogeneous : Integer;
          Spacing     : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_hbox_new");
   begin
      Set_Object (Box, Internal (Boolean'Pos (Homogeneous), Spacing));
   end Initialize_Hbox;

   ---------------------
   -- Initialize_Vbox --
   ---------------------

   procedure Initialize_Vbox
      (Box         : access Gtk_Vbox_Record'Class;
       Homogeneous : Boolean := False;
       Spacing     : Gint := 0)
   is
      function Internal
         (Homogeneous : Integer;
          Spacing     : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_vbox_new");
   begin
      Set_Object (Box, Internal (Boolean'Pos (Homogeneous), Spacing));
   end Initialize_Vbox;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
      (Box : access Gtk_Box_Record;
       Num : Gint) return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Box : System.Address;
          Num : Gint) return System.Address;
      pragma Import (C, Internal, "ada_box_get_child");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Box), Num), Stub));
   end Get_Child;

   ---------------------
   -- Get_Homogeneous --
   ---------------------

   function Get_Homogeneous (Box : access Gtk_Box_Record) return Boolean is
      function Internal (Box : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_box_get_homogeneous");
   begin
      return Boolean'Val (Internal (Get_Object (Box)));
   end Get_Homogeneous;

   -----------------
   -- Get_Spacing --
   -----------------

   function Get_Spacing (Box : access Gtk_Box_Record) return Gint is
      function Internal (Box : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_box_get_spacing");
   begin
      return Internal (Get_Object (Box));
   end Get_Spacing;

   --------------
   -- Pack_End --
   --------------

   procedure Pack_End
      (In_Box  : access Gtk_Box_Record;
       Child   : access Gtk.Widget.Gtk_Widget_Record'Class;
       Expand  : Boolean := True;
       Fill    : Boolean := True;
       Padding : Guint := 0)
   is
      procedure Internal
         (In_Box  : System.Address;
          Child   : System.Address;
          Expand  : Integer;
          Fill    : Integer;
          Padding : Guint);
      pragma Import (C, Internal, "gtk_box_pack_end");
   begin
      Internal (Get_Object (In_Box), Get_Object (Child), Boolean'Pos (Expand), Boolean'Pos (Fill), Padding);
   end Pack_End;

   -----------------------
   -- Pack_End_Defaults --
   -----------------------

   procedure Pack_End_Defaults
      (Box    : access Gtk_Box_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Box : System.Address; Widget : System.Address);
      pragma Import (C, Internal, "gtk_box_pack_end_defaults");
   begin
      Internal (Get_Object (Box), Get_Object (Widget));
   end Pack_End_Defaults;

   ----------------
   -- Pack_Start --
   ----------------

   procedure Pack_Start
      (In_Box  : access Gtk_Box_Record;
       Child   : access Gtk.Widget.Gtk_Widget_Record'Class;
       Expand  : Boolean := True;
       Fill    : Boolean := True;
       Padding : Guint := 0)
   is
      procedure Internal
         (In_Box  : System.Address;
          Child   : System.Address;
          Expand  : Integer;
          Fill    : Integer;
          Padding : Guint);
      pragma Import (C, Internal, "gtk_box_pack_start");
   begin
      Internal (Get_Object (In_Box), Get_Object (Child), Boolean'Pos (Expand), Boolean'Pos (Fill), Padding);
   end Pack_Start;

   -------------------------
   -- Pack_Start_Defaults --
   -------------------------

   procedure Pack_Start_Defaults
      (Box    : access Gtk_Box_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Box : System.Address; Widget : System.Address);
      pragma Import (C, Internal, "gtk_box_pack_start_defaults");
   begin
      Internal (Get_Object (Box), Get_Object (Widget));
   end Pack_Start_Defaults;

   -------------------------
   -- Query_Child_Packing --
   -------------------------

   procedure Query_Child_Packing
      (Box       : access Gtk_Box_Record;
       Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
       Expand    : out Boolean;
       Fill      : out Boolean;
       Padding   : out Guint;
       Pack_Type : out Gtk.Enums.Gtk_Pack_Type)
   is
      procedure Internal
         (Box       : System.Address;
          Child     : System.Address;
          Expand    : out Integer;
          Fill      : out Integer;
          Padding   : out Guint;
          Pack_Type : out Integer);
      pragma Import (C, Internal, "gtk_box_query_child_packing");
      Tmp_Expand    : Integer;
      Tmp_Fill      : Integer;
      Tmp_Pack_Type : Integer;
   begin
      Internal (Get_Object (Box), Get_Object (Child), Tmp_Expand, Tmp_Fill, Padding, Tmp_Pack_Type);
      Expand := Boolean'Val (Tmp_Expand);
      Fill := Boolean'Val (Tmp_Fill);
      Pack_Type := Gtk.Enums.Gtk_Pack_Type'Val (Tmp_Pack_Type);
   end Query_Child_Packing;

   -------------------
   -- Reorder_Child --
   -------------------

   procedure Reorder_Child
      (Box      : access Gtk_Box_Record;
       Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
       Position : Gint)
   is
      procedure Internal
         (Box      : System.Address;
          Child    : System.Address;
          Position : Gint);
      pragma Import (C, Internal, "gtk_box_reorder_child");
   begin
      Internal (Get_Object (Box), Get_Object (Child), Position);
   end Reorder_Child;

   -----------------------
   -- Set_Child_Packing --
   -----------------------

   procedure Set_Child_Packing
      (Box       : access Gtk_Box_Record;
       Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
       Expand    : Boolean;
       Fill      : Boolean;
       Padding   : Guint;
       Pack_Type : Gtk.Enums.Gtk_Pack_Type)
   is
      procedure Internal
         (Box       : System.Address;
          Child     : System.Address;
          Expand    : Integer;
          Fill      : Integer;
          Padding   : Guint;
          Pack_Type : Integer);
      pragma Import (C, Internal, "gtk_box_set_child_packing");
   begin
      Internal (Get_Object (Box), Get_Object (Child), Boolean'Pos (Expand), Boolean'Pos (Fill), Padding, Gtk.Enums.Gtk_Pack_Type'Pos (Pack_Type));
   end Set_Child_Packing;

   ---------------------
   -- Set_Homogeneous --
   ---------------------

   procedure Set_Homogeneous
      (Box         : access Gtk_Box_Record;
       Homogeneous : Boolean)
   is
      procedure Internal (Box : System.Address; Homogeneous : Integer);
      pragma Import (C, Internal, "gtk_box_set_homogeneous");
   begin
      Internal (Get_Object (Box), Boolean'Pos (Homogeneous));
   end Set_Homogeneous;

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing (Box : access Gtk_Box_Record; Spacing : Gint) is
      procedure Internal (Box : System.Address; Spacing : Gint);
      pragma Import (C, Internal, "gtk_box_set_spacing");
   begin
      Internal (Get_Object (Box), Spacing);
   end Set_Spacing;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : access Gtk_Box_Record) return Gtk.Enums.Gtk_Orientation
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Gtk.Enums.Gtk_Orientation'Val (Internal (Get_Object (Self)));
   end Get_Orientation;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : access Gtk_Box_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal (Self : System.Address; Orientation : Integer);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Gtk.Enums.Gtk_Orientation'Pos (Orientation));
   end Set_Orientation;

end Gtk.Box;
