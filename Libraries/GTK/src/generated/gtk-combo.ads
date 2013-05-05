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
--  The Gtk_Combo widget consists of a single-line text entry field and a
--  drop-down list. The drop-down list is displayed when the user clicks on a
--  small arrow button to the right of the entry field.
--
--  The drop-down list is a Gtk_List widget and can be accessed using the list
--  member of the Gtk_Combo. List elements can contain arbitrary widgets, but
--  if an element is not a plain label, then you must use the
--  Gtk_List.Set_Item_String function. This sets the string which will be
--  placed in the text entry field when the item is selected.
--
--  By default, the user can step through the items in the list using the
--  arrow (cursor) keys, though this behaviour can be turned off with
--  Set_Use_Arrows.
--
--  Normally the arrow keys are only active when the contents of the text
--  entry field matches one of the items in the list. If the contents of the
--  entry field do not match any of the list items, then pressing the arrow
--  keys does nothing. However, by calling Set_Use_Arrows_Always you can
--  specify that the arrow keys are always active. If the contents of the entry
--  field does not match any of the items in the list, then pressing the up or
--  down arrow key will set the entry field to the last or first item in the
--  list, respectively.
--
--  </description>
--  <screenshot>gtk-combo</screenshot>
--  <group>Obsolescent widgets</group>
--  <see>Gtk.Combo_Box</see>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Box;         use Gtk.Box;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Item;        use Gtk.Item;
with Gtk.Orientable;  use Gtk.Orientable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Combo is

   pragma Obsolescent;

   type Gtk_Combo_Record is new Gtk_Hbox_Record with null record;
   type Gtk_Combo is access all Gtk_Combo_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Combo_Box : out Gtk_Combo);
   procedure Initialize (Combo_Box : access Gtk_Combo_Record'Class);

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_combo_get_type");

   -------------
   -- Methods --
   -------------

   procedure Disable_Activate (Combo_Box : access Gtk_Combo_Record);
   --  Disable the standard handler for the return key in the entry field. The
   --  default behavior is to popdown the combo box list, so that the user can
   --  choose from it. However, if you want to add your own callback for the
   --  return key, you need to call this subprogram, and connect a handler to
   --  the "activate" signal for the entry.

   procedure Set_Case_Sensitive
      (Combo_Box : access Gtk_Combo_Record;
       Val       : Boolean := True);
   --  Specify whether the text entered into the Gtk_Entry field and the text
   --  in the list items are case sensitive. This may be useful, for example,
   --  when you have called Set_Value_In_List to limit the values entered, but
   --  you are not worried about differences in case.

   procedure Set_Item_String
      (Combo_Box  : access Gtk_Combo_Record;
       Item       : access Gtk.Item.Gtk_Item_Record'Class;
       Item_Value : UTF8_String);
   --  Set the string to place in the Gtk_Entry field when a particular list
   --  item is selected. This is needed if the list item is not a simple label.

   procedure Set_Popdown_Strings
      (Combo_Box : access Gtk_Combo_Record;
       Strings   : Gtk.Enums.String_List.Glist);
   --  Set all the items in the popup list.

   procedure Set_Use_Arrows
      (Combo_Box : access Gtk_Combo_Record;
       Val       : Boolean := True);
   --  Specify if the arrow (cursor) keys can be used to step through the
   --  items in the list. This is on by default.

   procedure Set_Use_Arrows_Always
      (Combo_Box : access Gtk_Combo_Record;
       Val       : Boolean := True);
   --  Specify if the arrow keys will still work even if the current contents
   --  of the Gtk_Entry field do not match any of the list items.

   procedure Set_Value_In_List
      (Combo_Box   : access Gtk_Combo_Record;
       Val         : Boolean := True;
       Ok_If_Empty : Boolean := False);
   --  Specify whether the value entered in the text entry field must match
   --  one of the values in the list. If this is set then the user will not be
   --  able to perform any other action until a valid value has been entered.
   --  If an empty field is acceptable, the Ok_If_Empty parameter should be
   --  True. If the value entered must match one of the values in the list, val
   --  should be True.

   ------------
   -- Fields --
   ------------

   function Get_Entry
      (Combo_Box : access Gtk_Combo_Record) return Gtk.Widget.Gtk_Widget;
   procedure Set_Entry
      (Combo_Box : access Gtk_Combo_Record;
       Value     : access Gtk.Widget.Gtk_Widget_Record'Class);

   function Get_List
      (Combo_Box : access Gtk_Combo_Record) return Gtk.Widget.Gtk_Widget;

   ---------------------
   -- Interfaces_Impl --
   ---------------------

   function Get_Orientation
      (Self : access Gtk_Combo_Record) return Gtk.Enums.Gtk_Orientation;
   procedure Set_Orientation
      (Self        : access Gtk_Combo_Record;
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
     (Gtk.Buildable.Gtk_Buildable, Gtk_Combo_Record, Gtk_Combo);
   function "+"
     (Widget : access Gtk_Combo_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Combo
   renames Implements_Buildable.To_Object;

   package Implements_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Combo_Record, Gtk_Combo);
   function "+"
     (Widget : access Gtk_Combo_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Combo
   renames Implements_Orientable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Allow_Empty_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Case_Sensitive_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Enable_Arrow_Keys_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Enable_Arrows_Always_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Value_In_List_Property
   --  Type: Boolean
   --  Flags: read-write

   Allow_Empty_Property : constant Glib.Properties.Property_Boolean;
   Case_Sensitive_Property : constant Glib.Properties.Property_Boolean;
   Enable_Arrow_Keys_Property : constant Glib.Properties.Property_Boolean;
   Enable_Arrows_Always_Property : constant Glib.Properties.Property_Boolean;
   Value_In_List_Property : constant Glib.Properties.Property_Boolean;

private
   Allow_Empty_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("allow-empty");
   Case_Sensitive_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("case-sensitive");
   Enable_Arrow_Keys_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("enable-arrow-keys");
   Enable_Arrows_Always_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("enable-arrows-always");
   Value_In_List_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("value-in-list");
end Gtk.Combo;
