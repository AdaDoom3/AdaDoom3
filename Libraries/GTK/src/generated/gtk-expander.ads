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
--  A container that can hide its child.
--
--  </description>
--  <screenshot>gtk-expanded</screenshot>
--  <group>Layout containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Bin;         use Gtk.Bin;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Expander is

   type Gtk_Expander_Record is new Gtk_Bin_Record with null record;
   type Gtk_Expander is access all Gtk_Expander_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Expander : out Gtk_Expander; Label : UTF8_String);
   procedure Initialize
      (Expander : access Gtk_Expander_Record'Class;
       Label    : UTF8_String);
   --  Creates a new expander using Label as the text of the label.
   --  Since: gtk+ 2.4
   --  "label": the text of the label

   procedure Gtk_New_With_Mnemonic
      (Expander : out Gtk_Expander;
       Label    : UTF8_String);
   procedure Initialize_With_Mnemonic
      (Expander : access Gtk_Expander_Record'Class;
       Label    : UTF8_String);
   --  Creates a new expander using Label as the text of the label. If
   --  characters in Label are preceded by an underscore, they are underlined.
   --  If you need a literal underscore character in a label, use '__' (two
   --  underscores). The first underlined character represents a keyboard
   --  accelerator called a mnemonic. Pressing Alt and that key activates the
   --  button.
   --  Since: gtk+ 2.4
   --  "label": the text of the label with an underscore in front of the
   --  mnemonic character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_expander_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Expanded
      (Expander : access Gtk_Expander_Record) return Boolean;
   procedure Set_Expanded
      (Expander : access Gtk_Expander_Record;
       Expanded : Boolean);
   --  Sets the state of the expander. Set to True, if you want the child
   --  widget to be revealed, and False if you want the child widget to be
   --  hidden.
   --  Since: gtk+ 2.4
   --  "expanded": whether the child widget is revealed

   function Get_Label
      (Expander : access Gtk_Expander_Record) return UTF8_String;
   procedure Set_Label
      (Expander : access Gtk_Expander_Record;
       Label    : UTF8_String);
   --  Sets the text of the label of the expander to Label. This will also
   --  clear any previously set labels.
   --  Since: gtk+ 2.4
   --  "label": a string

   function Get_Label_Fill
      (Expander : access Gtk_Expander_Record) return Boolean;
   procedure Set_Label_Fill
      (Expander   : access Gtk_Expander_Record;
       Label_Fill : Boolean);
   --  Sets whether the label widget should fill all available horizontal
   --  space allocated to Expander.
   --  Since: gtk+ 2.22
   --  "label_fill": True if the label should should fill all available
   --  horizontal space

   function Get_Label_Widget
      (Expander : access Gtk_Expander_Record) return Gtk.Widget.Gtk_Widget;
   procedure Set_Label_Widget
      (Expander     : access Gtk_Expander_Record;
       Label_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Set the label widget for the expander. This is the widget that will
   --  appear embedded alongside the expander arrow.
   --  Since: gtk+ 2.4
   --  "label_widget": the new label widget

   function Get_Spacing (Expander : access Gtk_Expander_Record) return Gint;
   procedure Set_Spacing
      (Expander : access Gtk_Expander_Record;
       Spacing  : Gint);
   --  Sets the spacing field of Expander, which is the number of pixels to
   --  place between expander and the child.
   --  Since: gtk+ 2.4
   --  "spacing": distance between the expander and child in pixels.

   function Get_Use_Markup
      (Expander : access Gtk_Expander_Record) return Boolean;
   procedure Set_Use_Markup
      (Expander   : access Gtk_Expander_Record;
       Use_Markup : Boolean);
   --  Sets whether the text of the label contains markup in <link
   --  linkend="PangoMarkupFormat">Pango's text markup language</link>. See
   --  Gtk.Label.Set_Markup.
   --  Since: gtk+ 2.4
   --  "use_markup": True if the label's text should be parsed for markup

   function Get_Use_Underline
      (Expander : access Gtk_Expander_Record) return Boolean;
   procedure Set_Use_Underline
      (Expander      : access Gtk_Expander_Record;
       Use_Underline : Boolean);
   --  If true, an underline in the text of the expander label indicates the
   --  next character should be used for the mnemonic accelerator key.
   --  Since: gtk+ 2.4
   --  "use_underline": True if underlines in the text indicate mnemonics

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Expander_Record, Gtk_Expander);
   function "+"
     (Widget : access Gtk_Expander_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Expander
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Expanded_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Label_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --
   --  Name: Label_Fill_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Label_Widget_Property
   --  Type: Gtk.Widget.Gtk_Widget
   --  Flags: read-write
   --
   --  Name: Spacing_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Use_Markup_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Use_Underline_Property
   --  Type: Boolean
   --  Flags: read-write

   Expanded_Property : constant Glib.Properties.Property_Boolean;
   Label_Property : constant Glib.Properties.Property_String;
   Label_Fill_Property : constant Glib.Properties.Property_Boolean;
   Label_Widget_Property : constant Glib.Properties.Property_Object;
   Spacing_Property : constant Glib.Properties.Property_Int;
   Use_Markup_Property : constant Glib.Properties.Property_Boolean;
   Use_Underline_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "activate"
   --     procedure Handler (Self : access Gtk_Expander_Record'Class);

   Signal_Activate : constant Glib.Signal_Name := "activate";

private
   Expanded_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("expanded");
   Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Label_Fill_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("label-fill");
   Label_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("label-widget");
   Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("spacing");
   Use_Markup_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-markup");
   Use_Underline_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-underline");
end Gtk.Expander;
