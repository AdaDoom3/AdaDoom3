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

--  <description>
--  A Gtk_Button_Box is a special type of Gtk_Box specially tailored to
--  contain buttons.
--
--  This is only a base class for Gtk_Hbutton_Box and Gtk_Vbutton_Box which
--  provide a way to arrange their children horizontally (resp. vertically).
--  You can not instantiate a Gtk_Button_Box directly, and have to use one the
--  above two instead.
--
--  </description>
--  <screenshot>gtk-button_box</screenshot>
--  <group>Layout containers</group>
--  <testgtk>create_button_box.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Glib.Types;     use Glib.Types;
with Gtk.Box;        use Gtk.Box;
with Gtk.Buildable;  use Gtk.Buildable;
with Gtk.Enums;      use Gtk.Enums;
with Gtk.Orientable; use Gtk.Orientable;
with Gtk.Widget;     use Gtk.Widget;

package Gtk.Button_Box is

   type Gtk_Button_Box_Record is new Gtk_Box_Record with null record;
   type Gtk_Button_Box is access all Gtk_Button_Box_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_button_box_get_type");

   -------------
   -- Methods --
   -------------

   procedure Get_Child_Ipadding
      (Widget : access Gtk_Button_Box_Record;
       Ipad_X : out Gint;
       Ipad_Y : out Gint);
   procedure Set_Child_Ipadding
      (Widget : access Gtk_Button_Box_Record;
       Ipad_X : Gint;
       Ipad_Y : Gint);
   pragma Obsolescent (Set_Child_Ipadding);
   --  Deprecated

   function Get_Child_Secondary
      (Widget : access Gtk_Button_Box_Record;
       Child  : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean;
   procedure Set_Child_Secondary
      (Widget       : access Gtk_Button_Box_Record;
       Child        : access Gtk.Widget.Gtk_Widget_Record'Class;
       Is_Secondary : Boolean);
   --  Set whether Child should appear in a secondary group of children. A
   --  typical use of a secondary child is the help button in a dialog.
   --  This group appears after the other children if the style is
   --  Buttonbox_Start, Buttonbox_Spread or Buttonbox_Edge, and before the
   --  other children if the style is Buttonbox_End. For horizontal button
   --  boxes, the definition of before/after depends on direction of the
   --  widget. (See Gtk.Widget.Set_Direction) If the style is Buttonbox_Start,
   --  or Buttonbox_End, then the secondary children are aligned at the other
   --  end of the button box from the main children. For the other styles, they
   --  appear immediately next to the main children.
   --  Is_Secondary: if True, the Child appears in a secondary group of the
   --  button box.
   --  "child": a child of Widget
   --  "is_secondary": if True, the Child appears in a secondary group of the
   --  button box.

   procedure Get_Child_Size
      (Widget     : access Gtk_Button_Box_Record;
       Min_Width  : out Gint;
       Min_Height : out Gint);
   pragma Obsolescent (Get_Child_Size);
   procedure Set_Child_Size
      (Widget     : access Gtk_Button_Box_Record;
       Min_Width  : Gint;
       Min_Height : Gint);
   pragma Obsolescent (Set_Child_Size);
   --  Deprecated

   function Get_Layout
      (Widget : access Gtk_Button_Box_Record)
       return Gtk.Enums.Gtk_Button_Box_Style;
   procedure Set_Layout
      (Widget       : access Gtk_Button_Box_Record;
       Layout_Style : Gtk.Enums.Gtk_Button_Box_Style);

   ---------------------
   -- Interfaces_Impl --
   ---------------------

   function Get_Orientation
      (Self : access Gtk_Button_Box_Record) return Gtk.Enums.Gtk_Orientation;
   procedure Set_Orientation
      (Self        : access Gtk_Button_Box_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Button_Box_Record, Gtk_Button_Box);
   function "+"
     (Widget : access Gtk_Button_Box_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Button_Box
   renames Implements_Buildable.To_Object;

   package Implements_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Button_Box_Record, Gtk_Button_Box);
   function "+"
     (Widget : access Gtk_Button_Box_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Button_Box
   renames Implements_Orientable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Layout_Style_Property
   --  Type: Gtk.Enums.Gtk_Button_Box_Style
   --  Flags: read-write

   Layout_Style_Property : constant Gtk.Enums.Property_Gtk_Button_Box_Style;

private
   Layout_Style_Property : constant Gtk.Enums.Property_Gtk_Button_Box_Style :=
     Gtk.Enums.Build ("layout-style");
end Gtk.Button_Box;
