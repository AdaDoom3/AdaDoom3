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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Radio_Action is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Radio_Action_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Action   : out Gtk_Radio_Action;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "";
       Value    : Gint)
   is
   begin
      Action := new Gtk_Radio_Action_Record;
      Gtk.Radio_Action.Initialize (Action, Name, Label, Tooltip, Stock_Id, Value);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Action   : access Gtk_Radio_Action_Record'Class;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "";
       Value    : Gint)
   is
      function Internal
         (Name     : Interfaces.C.Strings.chars_ptr;
          Label    : Interfaces.C.Strings.chars_ptr;
          Tooltip  : Interfaces.C.Strings.chars_ptr;
          Stock_Id : Interfaces.C.Strings.chars_ptr;
          Value    : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_radio_action_new");
      Tmp_Name     : Interfaces.C.Strings.chars_ptr := New_String (Name);
      Tmp_Label    : Interfaces.C.Strings.chars_ptr := New_String (Label);
      Tmp_Tooltip  : Interfaces.C.Strings.chars_ptr := New_String (Tooltip);
      Tmp_Stock_Id : Interfaces.C.Strings.chars_ptr := New_String (Stock_Id);
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Name, Tmp_Label, Tmp_Tooltip, Tmp_Stock_Id, Value);
      Free (Tmp_Name);
      Free (Tmp_Label);
      Free (Tmp_Tooltip);
      Free (Tmp_Stock_Id);
      Set_Object (Action, Tmp_Return);
   end Initialize;

   -----------------------
   -- Get_Current_Value --
   -----------------------

   function Get_Current_Value
      (Action : access Gtk_Radio_Action_Record) return Gint
   is
      function Internal (Action : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_radio_action_get_current_value");
   begin
      return Internal (Get_Object (Action));
   end Get_Current_Value;

   ---------------
   -- Get_Group --
   ---------------

   function Get_Group
      (Action : access Gtk_Radio_Action_Record)
       return Gtk.Widget.Widget_SList.GSList
   is
      function Internal (Action : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_radio_action_get_group");
      Tmp_Return : Gtk.Widget.Widget_SList.GSList;
   begin
      Gtk.Widget.Widget_SList.Set_Object (Tmp_Return, Internal (Get_Object (Action)));
      return Tmp_Return;
   end Get_Group;

   -----------------------
   -- Set_Current_Value --
   -----------------------

   procedure Set_Current_Value
      (Action        : access Gtk_Radio_Action_Record;
       Current_Value : Gint)
   is
      procedure Internal (Action : System.Address; Current_Value : Gint);
      pragma Import (C, Internal, "gtk_radio_action_set_current_value");
   begin
      Internal (Get_Object (Action), Current_Value);
   end Set_Current_Value;

   ---------------
   -- Set_Group --
   ---------------

   procedure Set_Group
      (Action : access Gtk_Radio_Action_Record;
       Group  : Gtk.Widget.Widget_SList.GSList)
   is
      procedure Internal (Action : System.Address; Group : System.Address);
      pragma Import (C, Internal, "gtk_radio_action_set_group");
   begin
      Internal (Get_Object (Action), Gtk.Widget.Widget_SList.Get_Object (Group));
   end Set_Group;

end Gtk.Radio_Action;
