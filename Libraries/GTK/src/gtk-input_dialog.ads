-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2007 AdaCore                    --
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

--  <c_version>2.8.17</c_version>
--  <group>Windows</group>

with Gtk.Dialog;

package Gtk.Input_Dialog is

   type Gtk_Input_Dialog_Record is new Dialog.Gtk_Dialog_Record with private;
   type Gtk_Input_Dialog is access all Gtk_Input_Dialog_Record'Class;

   procedure Gtk_New (Input_Dialog : out Gtk_Input_Dialog);

   procedure Initialize (Input_Dialog : access Gtk_Input_Dialog_Record'Class);

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Input_Dialog.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  - "enable_device"
   --  - "disable_device"
   --  </signals>

   Signal_Disable_Device : constant Glib.Signal_Name := "disable_device";
   Signal_Enable_Device  : constant Glib.Signal_Name := "enable_device";

private
   type Gtk_Input_Dialog_Record is new Dialog.Gtk_Dialog_Record
     with null record;

   pragma Import (C, Get_Type, "gtk_input_dialog_get_type");
end Gtk.Input_Dialog;
