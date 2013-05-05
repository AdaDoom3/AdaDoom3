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

package body Gtk.Button_Box is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Button_Box_Record);
   pragma Unreferenced (Type_Conversion);

   ------------------------
   -- Get_Child_Ipadding --
   ------------------------

   procedure Get_Child_Ipadding
      (Widget : access Gtk_Button_Box_Record;
       Ipad_X : out Gint;
       Ipad_Y : out Gint)
   is
      procedure Internal
         (Widget : System.Address;
          Ipad_X : out Gint;
          Ipad_Y : out Gint);
      pragma Import (C, Internal, "gtk_button_box_get_child_ipadding");
   begin
      Internal (Get_Object (Widget), Ipad_X, Ipad_Y);
   end Get_Child_Ipadding;

   -------------------------
   -- Get_Child_Secondary --
   -------------------------

   function Get_Child_Secondary
      (Widget : access Gtk_Button_Box_Record;
       Child  : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean
   is
      function Internal
         (Widget : System.Address;
          Child  : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_button_box_get_child_secondary");
   begin
      return Boolean'Val (Internal (Get_Object (Widget), Get_Object (Child)));
   end Get_Child_Secondary;

   --------------------
   -- Get_Child_Size --
   --------------------

   procedure Get_Child_Size
      (Widget     : access Gtk_Button_Box_Record;
       Min_Width  : out Gint;
       Min_Height : out Gint)
   is
      procedure Internal
         (Widget     : System.Address;
          Min_Width  : out Gint;
          Min_Height : out Gint);
      pragma Import (C, Internal, "gtk_button_box_get_child_size");
   begin
      Internal (Get_Object (Widget), Min_Width, Min_Height);
   end Get_Child_Size;

   ----------------
   -- Get_Layout --
   ----------------

   function Get_Layout
      (Widget : access Gtk_Button_Box_Record)
       return Gtk.Enums.Gtk_Button_Box_Style
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_button_box_get_layout");
   begin
      return Gtk.Enums.Gtk_Button_Box_Style'Val (Internal (Get_Object (Widget)));
   end Get_Layout;

   ------------------------
   -- Set_Child_Ipadding --
   ------------------------

   procedure Set_Child_Ipadding
      (Widget : access Gtk_Button_Box_Record;
       Ipad_X : Gint;
       Ipad_Y : Gint)
   is
      procedure Internal
         (Widget : System.Address;
          Ipad_X : Gint;
          Ipad_Y : Gint);
      pragma Import (C, Internal, "gtk_button_box_set_child_ipadding");
   begin
      Internal (Get_Object (Widget), Ipad_X, Ipad_Y);
   end Set_Child_Ipadding;

   -------------------------
   -- Set_Child_Secondary --
   -------------------------

   procedure Set_Child_Secondary
      (Widget       : access Gtk_Button_Box_Record;
       Child        : access Gtk.Widget.Gtk_Widget_Record'Class;
       Is_Secondary : Boolean)
   is
      procedure Internal
         (Widget       : System.Address;
          Child        : System.Address;
          Is_Secondary : Integer);
      pragma Import (C, Internal, "gtk_button_box_set_child_secondary");
   begin
      Internal (Get_Object (Widget), Get_Object (Child), Boolean'Pos (Is_Secondary));
   end Set_Child_Secondary;

   --------------------
   -- Set_Child_Size --
   --------------------

   procedure Set_Child_Size
      (Widget     : access Gtk_Button_Box_Record;
       Min_Width  : Gint;
       Min_Height : Gint)
   is
      procedure Internal
         (Widget     : System.Address;
          Min_Width  : Gint;
          Min_Height : Gint);
      pragma Import (C, Internal, "gtk_button_box_set_child_size");
   begin
      Internal (Get_Object (Widget), Min_Width, Min_Height);
   end Set_Child_Size;

   ----------------
   -- Set_Layout --
   ----------------

   procedure Set_Layout
      (Widget       : access Gtk_Button_Box_Record;
       Layout_Style : Gtk.Enums.Gtk_Button_Box_Style)
   is
      procedure Internal (Widget : System.Address; Layout_Style : Integer);
      pragma Import (C, Internal, "gtk_button_box_set_layout");
   begin
      Internal (Get_Object (Widget), Gtk.Enums.Gtk_Button_Box_Style'Pos (Layout_Style));
   end Set_Layout;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : access Gtk_Button_Box_Record) return Gtk.Enums.Gtk_Orientation
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
      (Self        : access Gtk_Button_Box_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal (Self : System.Address; Orientation : Integer);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Gtk.Enums.Gtk_Orientation'Pos (Orientation));
   end Set_Orientation;

end Gtk.Button_Box;
