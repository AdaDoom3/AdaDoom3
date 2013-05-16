-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2006-2007 AdaCore                    --
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
--  This package defines the base class for all items that can be added into
--  a toolbar (see gtk-toolbar.ads).
--  See also Gtk.Tool_Button (gtk-tool_button.ads).
--  See also Gtk.Separator_Tool_Item (gtk-separator_tool_item).
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Menus and Toolbars</group>

with Glib.Properties;
with Gtk.Tool_Button;

package Gtk.Toggle_Tool_Button is

   type Gtk_Toggle_Tool_Button_Record is
     new Gtk.Tool_Button.Gtk_Tool_Button_Record with null record;
   type Gtk_Toggle_Tool_Button is
     access all Gtk_Toggle_Tool_Button_Record'Class;

   procedure Gtk_New    (Button : out Gtk_Toggle_Tool_Button);
   procedure Initialize (Button : access Gtk_Toggle_Tool_Button_Record'Class);
   --  Create or initialize a new toggle button

   procedure Gtk_New_From_Stock
     (Button   : out Gtk_Toggle_Tool_Button;
      Stock_Id : String);
   procedure Initialize_From_Stock
     (Button   : access Gtk_Toggle_Tool_Button_Record'Class;
      Stock_Id : String);
   --  Create or initialize a new toggle button that contains a text and image
   --  from a stock item.

   function Get_Type return GType;
   --  Internal type representing this class of widgets

   procedure Set_Active
     (Button    : access Gtk_Toggle_Tool_Button_Record;
      Is_Active : Boolean);
   function Get_Active
     (Button : access Gtk_Toggle_Tool_Button_Record)
      return Boolean;
   --  Sets whether the button should be selected

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "toggled"
   --    procedure Handler
   --      (Button     : access Gtk_Toggle_Tool_Button_Record'Class);
   --    Emitted whenever the toggle button changes state
   --
   --  </signals>

   Signal_Toggled : constant Glib.Signal_Name := "toggled";

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name: Active_Property
   --  Type: Boolean
   --  See : Set_Active / Get_Active
   --
   --  </properties>

   Active_Property : constant Glib.Properties.Property_Boolean;

private
   Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("active");

   pragma Import (C, Get_Type, "gtk_toggle_tool_button_get_type");
end Gtk.Toggle_Tool_Button;
