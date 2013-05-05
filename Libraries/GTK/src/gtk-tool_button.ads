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
--  This package defines a special kind of Gtk.Toolbar child that embeds a
--  button.
--  See also gtk-toggle_tool_button.ads, gtk-radio_tool_button.ads and
--  gtk-menu_tool_button.ads
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Menus and Toolbars</group>

with Glib.Properties;
with Gtk.Tool_Item;
with Gtk.Widget;

package Gtk.Tool_Button is

   type Gtk_Tool_Button_Record is new Gtk.Tool_Item.Gtk_Tool_Item_Record
      with null record;
   type Gtk_Tool_Button is access all Gtk_Tool_Button_Record'Class;

   ----------------------
   -- Creating buttons --
   ----------------------

   procedure Gtk_New
     (Button      : out Gtk_Tool_Button;
      Icon_Widget : Gtk.Widget.Gtk_Widget := null;
      Label       : String := "");
   procedure Initialize
     (Button      : access Gtk_Tool_Button_Record'Class;
      Icon_Widget : Gtk.Widget.Gtk_Widget := null;
      Label       : String := "");
   --  Create or initialize a button, given its icon and label. Any of the
   --  parameters can be left unspecified if the button has none of these.

   procedure Gtk_New_From_Stock
     (Button   : out Gtk_Tool_Button;
      Stock_Id : String);
   procedure Initialize_From_Stock
     (Button   : access Gtk_Tool_Button_Record'Class;
      Stock_Id : String);
   --  Create or initialize a button from a stock icon (see gtk-stock.ads)

   function Get_Type return GType;
   --  Return the internal type used for this widget class

   procedure Set_Icon_Name
     (Button    : access Gtk_Tool_Button_Record;
      Icon_Name : String);
   function Get_Icon_Name
     (Button : access Gtk_Tool_Button_Record)  return String;
   --  Sets the icon for the tool button from a named themed icon.
   --  See the docs for Gtk.Icon_them for more details.
   --  The "icon_name" property only has an effect if not overriden by non-null
   --  "label", "icon_widget" or "stock_id" properties

   procedure Set_Icon_Widget
     (Button      : access Gtk_Tool_Button_Record;
      Icon_Widget : Gtk.Widget.Gtk_Widget := null);
   function Get_Icon_Widget
     (Button : access Gtk_Tool_Button_Record)
      return Gtk.Widget.Gtk_Widget;
   --  Sets or gets the widget used as icon on Button.
   --  If Icon_Widget is null, the icon used for the button is determined by
   --  the "stock_id" property. If the latter is also null, the button has no
   --  icon

   procedure Set_Label
     (Button : access Gtk_Tool_Button_Record;
      Label  : String);
   function Get_Label
     (Button : access Gtk_Tool_Button_Record) return String;
   --  Sets or gets the label used for the button. The "label" property only
   --  has an effect if not overridden by a non-null "label_widget" property.
   --  If both are null, the label comes from the "stock_id" properties. If
   --  also null, the button has no label. Get_Label only returns the value of
   --  the "labeL" property.

   procedure Set_Label_Widget
     (Button       : access Gtk_Tool_Button_Record;
      Label_Widget : Gtk.Widget.Gtk_Widget := null);
   function Get_Label_Widget
     (Button : access Gtk_Tool_Button_Record)
      return Gtk.Widget.Gtk_Widget;
   --  Sets Label_Widget as the widget that will be used as the label for the
   --  button. If this is null, the "label" property is used as label.

   procedure Set_Stock_Id
     (Button   : access Gtk_Tool_Button_Record;
      Stock_Id : String);
   function Get_Stock_Id
     (Button : access Gtk_Tool_Button_Record) return String;
   --  Sets the name of the stock item. This property has no effect if
   --  overriden by non-null "label" or "icon_widget" properties.

   procedure Set_Use_Underline
     (Button        : access Gtk_Tool_Button_Record;
      Use_Underline : Boolean := True);
   function Get_Use_Underline
     (Button : access Gtk_Tool_Button_Record)
      return Boolean;
   --  If Use_Underline is true, an underline in the label property indicates
   --  that the next character should be used a mnemonic accelerator key in the
   --  overflow menu of the toolbar. For instance, if the label is "_Open",
   --  the item in the overflow menu can be activated with alt-O.
   --  Labels shown on tool buttons never have mnemonics on them.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "clicked"
   --    procedure Handler
   --       (Button : access Gtk_Tool_Button_Record'Class);
   --    Emitted when the button is clicked with the mouse or activated with
   --    the keyboard.
   --
   --  </signals>

   Signal_Clicked : constant Glib.Signal_Name := "clicked";

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name: Label_Property
   --  Type: String
   --  See : Set_Label / Get_Label
   --
   --  Name: Use_Underline_Property
   --  Type: Boolean
   --  See : Set_Use_Underline / Get_Use_Underline
   --
   --  Name: Label_Widget_Property
   --  Type: Object
   --  See : Set_Label_Widget / Get_Label_Widget
   --
   --  Name: Stock_Id_Property
   --  Type: String
   --  See : Set_Stock_Id / Get_Stock_Id
   --
   --  Name: Icon_Name_Property
   --  Type: String
   --  See : Set_Icon_Name / Get_Icon_Name
   --
   --  Name: Icon_Widget_Property
   --  Type: Object
   --  See : Set_Icon_Widget / Get_Icon_Widget
   --
   --  </properties>

   Label_Property         : constant Glib.Properties.Property_String;
   Use_Underline_Property : constant Glib.Properties.Property_Boolean;
   Label_Widget_Property  : constant Glib.Properties.Property_Object;
   Stock_Id_Property      : constant Glib.Properties.Property_String;
   Icon_Name_Property     : constant Glib.Properties.Property_String;
   Icon_Widget_Property   : constant Glib.Properties.Property_Object;

private
   Label_Property         : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Use_Underline_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-underline");
   Label_Widget_Property  : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("label-widget");
   Stock_Id_Property      : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("stock-id");
   Icon_Name_Property     : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("icon-name");
   Icon_Widget_Property   : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("icon-widget");

   pragma Import (C, Get_Type, "gtk_tool_button_get_type");
end Gtk.Tool_Button;
