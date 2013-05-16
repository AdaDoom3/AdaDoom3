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
--  A Gtk_Check_Button places a discrete Gtk_Toggle_Button next to a widget,
--  (usually a Gtk_Label).
--
--  </description>
--  <screenshot>gtk-check_button</screenshot>
--  <group>Buttons and Toggles</group>
--  <testgtk>create_check_buttons.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.Types;        use Glib.Types;
with Gtk.Action;        use Gtk.Action;
with Gtk.Activatable;   use Gtk.Activatable;
with Gtk.Buildable;     use Gtk.Buildable;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Widget;        use Gtk.Widget;

package Gtk.Check_Button is

   type Gtk_Check_Button_Record is new Gtk_Toggle_Button_Record with null record;
   type Gtk_Check_Button is access all Gtk_Check_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Check_Button : out Gtk_Check_Button;
       Label        : UTF8_String := "");
   procedure Initialize
      (Check_Button : access Gtk_Check_Button_Record'Class;
       Label        : UTF8_String := "");
   --  Create a check button. if Label is null, then no widget is associated
   --  with the button, and any widget can be added to the button (with
   --  Gtk.Container.Add).

   procedure Gtk_New_With_Mnemonic
      (Check_Button : out Gtk_Check_Button;
       Label        : UTF8_String);
   procedure Initialize_With_Mnemonic
      (Check_Button : access Gtk_Check_Button_Record'Class;
       Label        : UTF8_String);
   --  Creates a new Gtk.Check_Button.Gtk_Check_Button containing a label. The
   --  label will be created using Gtk.Label.Gtk_New_With_Mnemonic, so
   --  underscores in Label indicate the mnemonic for the check button.
   --  "label": The text of the button, with an underscore in front of the
   --  mnemonic character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_check_button_get_type");

   ---------------------
   -- Interfaces_Impl --
   ---------------------

   procedure Do_Set_Related_Action
      (Self   : access Gtk_Check_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);

   function Get_Related_Action
      (Self : access Gtk_Check_Button_Record) return Gtk.Action.Gtk_Action;
   procedure Set_Related_Action
      (Self   : access Gtk_Check_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);

   function Get_Use_Action_Appearance
      (Self : access Gtk_Check_Button_Record) return Boolean;
   procedure Set_Use_Action_Appearance
      (Self           : access Gtk_Check_Button_Record;
       Use_Appearance : Boolean);

   procedure Sync_Action_Properties
      (Self   : access Gtk_Check_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Activatable"
   --
   --  - "Buildable"

   package Implements_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Check_Button_Record, Gtk_Check_Button);
   function "+"
     (Widget : access Gtk_Check_Button_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Check_Button
   renames Implements_Activatable.To_Object;

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Check_Button_Record, Gtk_Check_Button);
   function "+"
     (Widget : access Gtk_Check_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Check_Button
   renames Implements_Buildable.To_Object;

end Gtk.Check_Button;
