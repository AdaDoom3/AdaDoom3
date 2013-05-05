-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
--  The Gtk_Accel_Label widget is a child of Gtk_Label that also displays an
--  accelerator key on the right of the label text, e.g. 'Ctl+S'. It is
--  commonly used in menus to show the keyboard short-cuts for commands.
--
--  The accelerator key to display is not set explicitly. Instead, the
--  Gtk_Accel_Label displays the accelerators which have been added to a
--  particular widget. This widget is set by calling Set_Accel_Widget.
--
--  For example, a Gtk_Menu_Item widget may have an accelerator added to emit
--  the "activate" signal when the 'Ctl+S' key combination is pressed.
--  A Gtk_Accel_Label is created and added to the Gtk_Menu_Item, and
--  Set_Accel_Widget is called with the Gtk_Menu_Item as the second argument.
--  The Gtk_Accel_Label will now display 'Ctl+S' after its label.
--
--  Note that creating a Gtk_Menu_Item with Gtk_New and a non null "label"
--  parameter (ditto for Gtk_Check_Menu_Item and Gtk_Radio_Menu_Item)
--  automatically adds a Gtk_Accel_Label to the Gtk_Menu_Item and calls
--  Set_Accel_Widget to set it up for you.
--
--  A Gtk_Accel_Label will only display accelerators which have the
--  Accel_Visible (see Gtk.Accel_Group) flag set. A Gtk_Accel_Label can display
--  multiple accelerators and even signal names, though it is almost always
--  used to display just one accelerator key.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Display widgets</group>
--  <screenshot>gtk-accel_label</screenshot>

with Glib.Properties;
with Gtk.Label;
with Gtk.Widget;

package Gtk.Accel_Label is

   type Gtk_Accel_Label_Record is new Gtk.Label.Gtk_Label_Record with private;
   type Gtk_Accel_Label is access all Gtk_Accel_Label_Record;

   procedure Gtk_New (Accel_Label : out Gtk_Accel_Label; Str : UTF8_String);
   --  Create a new Gtk_Accel_Label.
   --  Str is the label string.

   procedure Initialize
     (Accel_Label : access Gtk_Accel_Label_Record'Class; Str : UTF8_String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Accel_Label.

   procedure Set_Accel_Widget
     (Accel_Label  : access Gtk_Accel_Label_Record;
      Accel_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Add an accelerator to a particular widget.

   function Get_Accel_Widget
     (Accel_Label : access Gtk_Accel_Label_Record)
      return Gtk.Widget.Gtk_Widget;
   --  Return the widget monitored by Accel_Label.

   function Get_Accel_Width
     (Accel_Label : access Gtk_Accel_Label_Record) return Guint;
   --  Return the width needed to display the accelerator key(s).
   --  This is used by menus to align all of the Gtk_Menu_Item widgets, and
   --  shouldn't be needed by applications.

   function Refetch
     (Accel_Label : access Gtk_Accel_Label_Record) return Boolean;
   --  Recreate the string representing the accelerator keys.
   --  This should not be needed since the string is automatically updated
   --  whenever accelerators are added or removed from the associated widget.
   --  Always return False.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --  </properties>

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  Name:  Accel_Closure_Property
   --  Type:  Boxed
   --  Descr: The closure to be monitored for accelerator changes
   --
   --  Name:  Accel_Widget_Property
   --  Type:  Object
   --  Descr: The widget to be monitored for accelerator changes
   --
   --  </signals>

   --  Accel_Closure_Property : constant Glib.Properties.Property_Boxed;
   Accel_Widget_Property  : constant Glib.Properties.Property_Object;


private
   type Gtk_Accel_Label_Record is new Gtk.Label.Gtk_Label_Record
   with null record;

   --  Accel_Closure_Property : constant Glib.Properties.Property_Boxed :=
   --   Glib.Properties.Build ("accel-closure");
   Accel_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("accel-widget");

   pragma Import (C, Get_Type, "gtk_accel_label_get_type");

end Gtk.Accel_Label;

--  <example>
--  Creating a simple menu item with an accelerator key.
--
--  Save_Item   : Gtk_Menu_Item;
--  Accel_Group : Gtk_Accel_Group;
--
--  --  Create a Gtk_Accel_Group and add it to the window.
--  Gtk_New (Accel_Group);
--  Add_Accel_Group (Window, Accel_Group);
--
--  --  Create the menu item using the convenience function.
--  Gtk_New (Save_Item, "Save");
--  Show (Save_Item);
--  Add (Menu, Save_Item);
--
--  --  Now add the accelerator to the Gtk_Menu_Item. Note that since we called
--  --  Gtk_New with a label to create the Gtk_Menu_Item the
--  --  Gtk_Accel_Label is automatically set up to display the Gtk_Menu_Item
--  --  accelerators. We just need to make sure we use Accel_Visible here.
--
--  Add_Accelerator
--    (Save_Item, "activate", Accel_Group,
--     GDK_S, Control_Mask, Accel_Visible);
--  </example>

--  No binding: gtk_accel_label_set_accel_closure
