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
--  A Gtk_Toggle_Button is like a regular button, but can be in one of two
--  states, "active" or "inactive". Its visual aspect is modified when the
--  state is changed.
--
--  You should consider using a Gtk_Check_Button instead, since it looks nicer
--  and provides more visual clues that the button can be toggled.
--
--  </description>
--  <screenshot>gtk-toggle_button</screenshot>
--  <group>Buttons and Toggles</group>
--  <testgtk>create_toggle_buttons.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Action;      use Gtk.Action;
with Gtk.Activatable; use Gtk.Activatable;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Button;      use Gtk.Button;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Toggle_Button is

   type Gtk_Toggle_Button_Record is new Gtk_Button_Record with null record;
   type Gtk_Toggle_Button is access all Gtk_Toggle_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Toggle_Button : out Gtk_Toggle_Button;
       Label         : UTF8_String := "");
   procedure Initialize
      (Toggle_Button : access Gtk_Toggle_Button_Record'Class;
       Label         : UTF8_String := "");
   --  Initialize a button. If Label is "", then no label is created inside
   --  the button and you will have to provide your own child through a call to
   --  Gtk.Container.Add. This is the recommended way to put a pixmap inside a
   --  toggle button.

   procedure Gtk_New_With_Mnemonic
      (Toggle_Button : out Gtk_Toggle_Button;
       Label         : UTF8_String);
   procedure Initialize_With_Mnemonic
      (Toggle_Button : access Gtk_Toggle_Button_Record'Class;
       Label         : UTF8_String);
   --  Creates a new Gtk.Toggle_Button.Gtk_Toggle_Button containing a label.
   --  The label will be created using Gtk.Label.Gtk_New_With_Mnemonic, so
   --  underscores in Label indicate the mnemonic for the button.
   --  "label": the text of the button, with an underscore in front of the
   --  mnemonic character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_toggle_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Active
      (Toggle_Button : access Gtk_Toggle_Button_Record) return Boolean;
   procedure Set_Active
      (Toggle_Button : access Gtk_Toggle_Button_Record;
       Is_Active     : Boolean);
   --  Change the state of the button. When Is_Active is True, the button is
   --  drawn as a pressed button

   function Get_Inconsistent
      (Toggle_Button : access Gtk_Toggle_Button_Record) return Boolean;
   procedure Set_Inconsistent
      (Toggle_Button : access Gtk_Toggle_Button_Record;
       Setting       : Boolean := True);
   --  If the user has selected a range of elements (such as some text or
   --  spreadsheet cells) that are affected by a toggle button, and the current
   --  values in that range are inconsistent, you may want to display the
   --  toggle in an "in between" state. This function turns on "in between"
   --  display. Normally you would turn off the inconsistent state again if the
   --  user toggles the toggle button. This has to be done manually,
   --  Gtk.Toggle_Button.Set_Inconsistent only affects visual appearance, it
   --  doesn't affect the semantics of the button.
   --  "setting": True if state is inconsistent

   function Get_Mode
      (Toggle_Button : access Gtk_Toggle_Button_Record) return Boolean;
   procedure Set_Mode
      (Toggle_Button  : access Gtk_Toggle_Button_Record;
       Draw_Indicator : Boolean);
   --  Sets whether the button is displayed as a separate indicator and label.
   --  You can call this function on a checkbutton or a radiobutton with This
   --  function only affects instances of classes like
   --  Gtk.Check_Button.Gtk_Check_Button and Gtk.Radio_Button.Gtk_Radio_Button
   --  that derive from Gtk.Toggle_Button.Gtk_Toggle_Button, not instances of
   --  Gtk.Toggle_Button.Gtk_Toggle_Button itself.
   --  "draw_indicator": if True, draw the button as a separate indicator and
   --  label; if False, draw the button like a normal button

   procedure Toggled (Toggle_Button : access Gtk_Toggle_Button_Record);

   ---------------------
   -- Interfaces_Impl --
   ---------------------

   procedure Do_Set_Related_Action
      (Self   : access Gtk_Toggle_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);

   function Get_Related_Action
      (Self : access Gtk_Toggle_Button_Record) return Gtk.Action.Gtk_Action;
   procedure Set_Related_Action
      (Self   : access Gtk_Toggle_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);

   function Get_Use_Action_Appearance
      (Self : access Gtk_Toggle_Button_Record) return Boolean;
   procedure Set_Use_Action_Appearance
      (Self           : access Gtk_Toggle_Button_Record;
       Use_Appearance : Boolean);

   procedure Sync_Action_Properties
      (Self   : access Gtk_Toggle_Button_Record;
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
     (Gtk.Activatable.Gtk_Activatable, Gtk_Toggle_Button_Record, Gtk_Toggle_Button);
   function "+"
     (Widget : access Gtk_Toggle_Button_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Toggle_Button
   renames Implements_Activatable.To_Object;

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Toggle_Button_Record, Gtk_Toggle_Button);
   function "+"
     (Widget : access Gtk_Toggle_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Toggle_Button
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Active_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Draw_Indicator_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Inconsistent_Property
   --  Type: Boolean
   --  Flags: read-write

   Active_Property : constant Glib.Properties.Property_Boolean;
   Draw_Indicator_Property : constant Glib.Properties.Property_Boolean;
   Inconsistent_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "toggled"
   --     procedure Handler (Self : access Gtk_Toggle_Button_Record'Class);

   Signal_Toggled : constant Glib.Signal_Name := "toggled";

private
   Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("active");
   Draw_Indicator_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("draw-indicator");
   Inconsistent_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inconsistent");
end Gtk.Toggle_Button;
