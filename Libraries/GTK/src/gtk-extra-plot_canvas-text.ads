-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
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
--  A special kind of child that can be put in a Gtk_Plot_Canvas.
--  </description>
--  <c_version>gtkextra 2.1.1</c_version>
--  <group>Plotting Data</group>

with Gdk.Color;
with Gtk.Enums;
with Gtk.Extra.Plot_Data;

package Gtk.Extra.Plot_Canvas.Text is

   type Gtk_Plot_Canvas_Text_Record is new Gtk_Plot_Canvas_Child_Record
     with private;
   type Gtk_Plot_Canvas_Text is access all Gtk_Plot_Canvas_Text_Record'Class;

   procedure Gtk_New
     (Child    : out Gtk_Plot_Canvas_Text;
      Text     : String;
      Font     : String := "";
      Height   : Gint := 0;
      Angle    : Gtk.Extra.Plot_Data.Plot_Angle := Gtk.Extra.Plot_Data.Angle_0;
      Fg       : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Bg            : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Transparent   : Boolean := True;
      Justification : Gtk.Enums.Gtk_Justification := Gtk.Enums.Justify_Center);
   --  Create a new text child.
   --  If default values are used, the corresponding attribute isn't set

   procedure Set_Attributes
     (Child         : access Gtk_Plot_Canvas_Text_Record;
      Font          : String := "";
      Height        : Gint;
      Angle         : Gtk.Extra.Plot_Data.Plot_Angle;
      Fg            : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Bg            : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Transparent   : Boolean;
      Justification : Gtk.Enums.Gtk_Justification;
      Text          : String);
   --  Change the attribute of Text. The attributes with their default
   --  values are not changed.

private
   type Gtk_Plot_Canvas_Text_Record is new Gtk_Plot_Canvas_Child_Record
     with null record;
end Gtk.Extra.Plot_Canvas.Text;
