-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--      Copyright (C) 2000 E. Briot, J. Brobecker and A. Charlet     --
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
--  A Gtk_Color_Combo is a widget that ease the selection of colors
--  by the user. It is a special form of a Gtk_Combo_Box, that displays
--  a special popup window, with a list of colors.
--
--  Note that nothing appears in the button, this your responsibility to
--  update it when the user selects a new color (see the "changed" signal).
--
--  The recommended solution is to put a Gtk_Pixmap as the
--  child of the button of the combo box ("Add (Get_Button (Combo), Pixmap)"),
--  and updated it in the handler for this signal.
--  </description>
--  <c_version>gtkextra 2.1.1</c_version>
--  <group>Selectors</group>

with Gdk.Color;
with Gtk.Extra.Combo_Button;

package Gtk.Extra.Color_Combo is

   type Gtk_Color_Combo_Record is
     new Gtk.Extra.Combo_Button.Gtk_Combo_Button_Record with private;
   type Gtk_Color_Combo is access all Gtk_Color_Combo_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Color_Combo);
   --  Create a new default combo box.
   --  It shows a list of 40 default colors.

   procedure Initialize (Widget : access Gtk_Color_Combo_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Gtk_New
     (Widget : out Gtk_Color_Combo;
      Nrows  : Gint;
      Ncols  : Gint;
      Values : Gdk.Color.Gdk_Color_Array);
   --  Create a new combo box with a specific list of colors.
   --  Note that Color_Names must contain at least Nrows * Ncols elements.

   procedure Initialize
     (Widget : access Gtk_Color_Combo_Record;
      Nrows  : Gint;
      Ncols  : Gint;
      Values : Gdk.Color.Gdk_Color_Array);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Color_Combo.

   function Get_Color_At
     (Widget : access Gtk_Color_Combo_Record;
      Row    : Gint;
      Col    : Gint) return Gdk.Color.Gdk_Color;
   --  Return the name of the color at specific coordinates.

   procedure Find_Color
     (Color_Combo : access Gtk_Color_Combo_Record;
      Color       : Gdk.Color.Gdk_Color;
      Row         : out Gint;
      Col         : out Gint);
   --  Return the coordinates in which a color appear in the popup window.
   --  (-1, -1) is returned if the color was not found in the combo box.

   function Get_Selection (Color_Combo : access Gtk_Color_Combo_Record)
      return Gdk.Color.Gdk_Color;
   --  Return the current selection in the combo.

   function Set_Color
     (Color_Combo : access Gtk_Color_Combo_Record;
      Name        : String)
     return Boolean;
   --  Set the new current color. If the color is not found in the list of
   --  colors provided in the popup window, False is returned.

   function Set_Color
     (Color_Combo : access Gtk_Color_Combo_Record;
      Color       : Gdk.Color.Gdk_Color)
     return Boolean;
   --  Set the new current color. Color must have been allocated first.  If the
   --  color is not found in the list of colors provided in the popup window,
   --  False is returned.

   function Get_Ncols (Color_Combo : access Gtk_Color_Combo_Record)
      return Gint;
   --  Return the number of columns in the popup window

   function Get_Nrows (Color_Combo : access Gtk_Color_Combo_Record)
      return Gint;
   --  Return the number of rows in the popup window

   procedure Changed
     (Color_Combo : access Gtk_Color_Combo_Record;
      Row : Gint;
      Col : Gint);
   --  Emit the changed signal for the widget, as if the color at coordinates
   --  (Row, Col) had been selected.
   --  Note that this doesn't change the internal state of the widget (use
   --  Set_Color for that).

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "changed"
   --  procedure Handler (Color_Combo : access Gtk_Color_Combo_Record'Class;
   --                     Selection   : Gint;
   --                     Color       : access Gdk.Color.Gdk_Color);
   --
   --  Emitted when the color has selected a new color.
   --  Selection is the number of the selection (this is the total
   --  row * Ncols + col). Color_Name is the name of the selected color.
   --  </signals>

private
   type Gtk_Color_Combo_Record is
     new Gtk.Extra.Combo_Button.Gtk_Combo_Button_Record with null record;
   pragma Import (C, Get_Type, "gtk_color_combo_get_type");
end Gtk.Extra.Color_Combo;
