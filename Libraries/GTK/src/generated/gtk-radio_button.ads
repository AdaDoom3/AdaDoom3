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
--  A Gtk_Radio_Button is a simple button that has two states, like a
--  Gtk_Toggle_Button. However, Gtk_Radio_Buttons can be grouped together to
--  get a special behavior: only one button in the group can be active at any
--  given time. Thus, when the user selects one of the buttons from the group,
--  the button that was previously selected is disabled.
--
--  The radio buttons always belongs to a group, even if there is only one in
--  this group.
--
--  </description>
--  <screenshot>gtk-radio_button</screenshot>
--  <group>Buttons and Toggles</group>
--  <testgtk>create_radio_button.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Glib.Properties;  use Glib.Properties;
with Glib.Types;       use Glib.Types;
with Gtk.Action;       use Gtk.Action;
with Gtk.Activatable;  use Gtk.Activatable;
with Gtk.Buildable;    use Gtk.Buildable;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Widget;       use Gtk.Widget;

package Gtk.Radio_Button is

   type Gtk_Radio_Button_Record is new Gtk_Check_Button_Record with null record;
   type Gtk_Radio_Button is access all Gtk_Radio_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Radio_Button : out Gtk_Radio_Button;
       Group        : Gtk.Widget.Widget_SList.GSlist := Widget_SList.Null_List;
       Label        : UTF8_String := "");
   procedure Initialize
      (Radio_Button : access Gtk_Radio_Button_Record'Class;
       Group        : Gtk.Widget.Widget_SList.GSlist := Widget_SList.Null_List;
       Label        : UTF8_String := "");
   --  Creates or initializes a new radio button, belonging to Group. If Label
   --  is left as the empty string, then the button will not have any child and
   --  you are free to put any thing you want in it, including a pixmap. To
   --  initialize the group (when creating the first button), leave Group to
   --  the Null_List. You can later get the new group that is created with a
   --  call to the Get_Group subprogram below.
   --  "group": an existing radio button group, or null if you are creating a
   --  new group.
   --  "label": the text label to display next to the radio button.

   procedure Gtk_New
      (Radio_Button : out Gtk_Radio_Button;
       Group        : Gtk_Radio_Button;
       Label        : UTF8_String := "");
   procedure Initialize
      (Radio_Button : access Gtk_Radio_Button_Record'Class;
       Group        : Gtk_Radio_Button;
       Label        : UTF8_String := "");
   --  Creates a new Gtk.Radio_Button.Gtk_Radio_Button with a text label,
   --  adding it to the same group as Radio_Group_Member.
   --  "Group": widget to get radio group from or null
   --  "label": a text string to display next to the radio button.

   procedure Gtk_New_With_Mnemonic
      (Radio_Button : out Gtk_Radio_Button;
       Group        : Gtk.Widget.Widget_SList.GSlist := Widget_SList.Null_List;
       Label        : UTF8_String);
   procedure Initialize_With_Mnemonic
      (Radio_Button : access Gtk_Radio_Button_Record'Class;
       Group        : Gtk.Widget.Widget_SList.GSlist := Widget_SList.Null_List;
       Label        : UTF8_String);
   procedure Gtk_New_With_Mnemonic
      (Radio_Button : out Gtk_Radio_Button;
       Group        : Gtk_Radio_Button;
       Label        : UTF8_String);
   procedure Initialize_With_Mnemonic
      (Radio_Button : access Gtk_Radio_Button_Record'Class;
       Group        : Gtk_Radio_Button;
       Label        : UTF8_String);
   --  Creates a new Gtk.Radio_Button.Gtk_Radio_Button containing a label. The
   --  label will be created using Gtk.Label.Gtk_New_With_Mnemonic, so
   --  underscores in Label indicate the mnemonic for the button.
   --  To initialize a new group (when creating the first button), you should
   --  pass it null or a button that has not been created with Gtk_New, as in
   --  the example below.
   --  "Group": widget to get radio group from or null
   --  "label": the text of the button, with an underscore in front of the
   --  mnemonic character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_radio_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Group
      (Radio_Button : access Gtk_Radio_Button_Record)
       return Gtk.Widget.Widget_SList.GSlist;
   procedure Set_Group
      (Radio_Button : access Gtk_Radio_Button_Record;
       Group        : Gtk.Widget.Widget_SList.GSlist);
   --  Sets a Gtk.Radio_Button.Gtk_Radio_Button's group. It should be noted
   --  that this does not change the layout of your interface in any way, so if
   --  you are changing the group, it is likely you will need to re-arrange the
   --  user interface to reflect these changes.
   --  "group": an existing radio button group, such as one returned from
   --  Gtk.Radio_Button.Get_Group.

   ---------------------
   -- Interfaces_Impl --
   ---------------------

   procedure Do_Set_Related_Action
      (Self   : access Gtk_Radio_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);

   function Get_Related_Action
      (Self : access Gtk_Radio_Button_Record) return Gtk.Action.Gtk_Action;
   procedure Set_Related_Action
      (Self   : access Gtk_Radio_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);

   function Get_Use_Action_Appearance
      (Self : access Gtk_Radio_Button_Record) return Boolean;
   procedure Set_Use_Action_Appearance
      (Self           : access Gtk_Radio_Button_Record;
       Use_Appearance : Boolean);

   procedure Sync_Action_Properties
      (Self   : access Gtk_Radio_Button_Record;
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
     (Gtk.Activatable.Gtk_Activatable, Gtk_Radio_Button_Record, Gtk_Radio_Button);
   function "+"
     (Widget : access Gtk_Radio_Button_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Radio_Button
   renames Implements_Activatable.To_Object;

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Radio_Button_Record, Gtk_Radio_Button);
   function "+"
     (Widget : access Gtk_Radio_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Radio_Button
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Group_Property
   --  Type: Gtk_Radio_Button
   --  Flags: write

   Group_Property : constant Glib.Properties.Property_Object;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "group-changed"
   --     procedure Handler (Self : access Gtk_Radio_Button_Record'Class);
   --  Emitted when the group of radio buttons that a radio button belongs to
   --  changes. This is emitted when a radio button switches from being alone
   --  to being part of a group of 2 or more buttons, or vice-versa, and when a
   --  button is moved from one group of 2 or more buttons to a different one,
   --  but not when the composition of the group that a button belongs to
   --  changes.

   Signal_Group_Changed : constant Glib.Signal_Name := "group-changed";

private
   Group_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("group");
end Gtk.Radio_Button;
