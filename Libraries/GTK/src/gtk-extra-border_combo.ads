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
--  A Gtk_Border_Combo is a special kind of combo box that allows the
--  user to select the border to apply to cells in a spreadsheet.
--  Its main usage seems to be with a Gtk_Sheet.
--  </description>
--  <c_version>gtkextra 2.1.1</c_version>
--  <group>Selectors</group>

with Gtk.Extra.Combo_Button;

package Gtk.Extra.Border_Combo is

   type Gtk_Border_Combo_Record is
     new Gtk.Extra.Combo_Button.Gtk_Combo_Button_Record with private;
   type Gtk_Border_Combo is access all Gtk_Border_Combo_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Border_Combo);
   --  Create a new border combo.
   --  The button contains the currently selected border.

   procedure Initialize (Widget : access Gtk_Border_Combo_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Border_Combo.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "changed"
   --  procedure Handler (Combo : access Gtk_Border_Combo_Record'Class;
   --                     Selection : Gint);
   --
   --  Emitted when a new font has been selected.
   --  Selection is the number of the selection font.
   --  </signals>
private
   type Gtk_Border_Combo_Record is
     new Gtk.Extra.Combo_Button.Gtk_Combo_Button_Record with null record;
   pragma Import (C, Get_Type, "gtk_border_combo_get_type");
end Gtk.Extra.Border_Combo;
