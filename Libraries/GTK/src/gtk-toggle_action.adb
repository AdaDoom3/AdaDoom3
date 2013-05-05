-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2006-2013, AdaCore                  --
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

with Gtkada.Bindings;      use Gtkada.Bindings;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Glib.Type_Conversion_Hooks;

package body Gtk.Toggle_Action is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Toggle_Action_Record);
   pragma Warnings (Off, Type_Conversion);

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active
     (Action : access Gtk_Toggle_Action_Record) return Boolean
   is
      function Internal (Action : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_toggle_action_get_active");
   begin
      return Boolean'Val (Internal (Get_Object (Action)));
   end Get_Active;

   -----------------------
   -- Get_Draw_As_Radio --
   -----------------------

   function Get_Draw_As_Radio
     (Action : access Gtk_Toggle_Action_Record) return Boolean
   is
      function Internal (Action : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_toggle_action_get_draw_as_radio");
   begin
      return Boolean'Val (Internal (Get_Object (Action)));
   end Get_Draw_As_Radio;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Action   : out Gtk_Toggle_Action;
      Name     : String;
      Label    : String := "";
      Tooltip  : String := "";
      Stock_Id : String := "")
   is
   begin
      Action := new Gtk_Toggle_Action_Record;
      Initialize (Action, Name, Label, Tooltip, Stock_Id);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Action   : access Gtk_Toggle_Action_Record'Class;
      Name     : String;
      Label    : String := "";
      Tooltip  : String := "";
      Stock_Id : String := "")
   is
      function Internal
        (Name     : String;
         Label    : chars_ptr;
         Tooltip  : chars_ptr;
         Stock_Id : chars_ptr)
         return System.Address;
      pragma Import (C, Internal, "gtk_toggle_action_new");
      L : chars_ptr := String_Or_Null (Label);
      T : chars_ptr := String_Or_Null (Tooltip);
      S : chars_ptr := String_Or_Null (Stock_Id);
   begin
      Set_Object (Action, Internal (Name & ASCII.NUL, L, T, S));
      Free (L);
      Free (T);
      Free (S);
   end Initialize;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active
     (Action    : access Gtk_Toggle_Action_Record;
      Is_Active : Boolean)
   is
      procedure Internal (Action : System.Address; Is_Active : Gboolean);
      pragma Import (C, Internal, "gtk_toggle_action_set_active");
   begin
      Internal (Get_Object (Action), Boolean'Pos (Is_Active));
   end Set_Active;

   -----------------------
   -- Set_Draw_As_Radio --
   -----------------------

   procedure Set_Draw_As_Radio
     (Action        : access Gtk_Toggle_Action_Record;
      Draw_As_Radio : Boolean)
   is
      procedure Internal (Action : System.Address; Draw_As_Radio : Gboolean);
      pragma Import (C, Internal, "gtk_toggle_action_set_draw_as_radio");
   begin
      Internal (Get_Object (Action), Boolean'Pos (Draw_As_Radio));
   end Set_Draw_As_Radio;

   -------------
   -- Toggled --
   -------------

   procedure Toggled (Action : access Gtk_Toggle_Action_Record) is
      procedure Internal (Action : System.Address);
      pragma Import (C, Internal, "gtk_toggle_action_toggled");
   begin
      Internal (Get_Object (Action));
   end Toggled;

end Gtk.Toggle_Action;
