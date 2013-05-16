-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--      Copyright (C) 2000 E. Briot, J. Brobecker and A. Charlet     --
--                 Copyright (C) 2000-2013, AdaCore                  --
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

with System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Extra.Combo_Button is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Combo_Button_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Combo_Button)
   is
   begin
      Widget := new Gtk_Combo_Button_Record;
      Gtk.Extra.Combo_Button.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Combo_Button_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_combo_button_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   -------------------------
   -- Hide_Popdown_Window --
   -------------------------

   procedure Hide_Popdown_Window (Combo : access Gtk_Combo_Button_Record)
   is
      procedure Internal (Combo : in System.Address);
      pragma Import (C, Internal, "gtk_combo_button_hide_popdown_window");
   begin
      Internal (Get_Object (Combo));
   end Hide_Popdown_Window;

   ----------------
   -- Get_Button --
   ----------------

   function Get_Button (Combo : access Gtk_Combo_Button_Record)
                       return Gtk.Button.Gtk_Button
   is
      function Internal (Combo : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gtk_combo_button_get_button");
      Stub : Gtk.Button.Gtk_Button_Record;
   begin
      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (Combo)), Stub));
   end Get_Button;

   -----------------------
   -- Get_Toggle_Button --
   -----------------------

   function Get_Toggle_Button
     (Combo : access Gtk_Combo_Button_Record)
      return Gtk.Toggle_Button.Gtk_Toggle_Button
   is
      function Internal (Combo : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gtk_combo_button_get_arrow");
      Stub : Gtk.Toggle_Button.Gtk_Toggle_Button_Record;
   begin
      return Gtk.Toggle_Button.Gtk_Toggle_Button
        (Get_User_Data (Internal (Get_Object (Combo)), Stub));
   end Get_Toggle_Button;

   ---------------
   -- Get_Frame --
   ---------------

   function Get_Frame (Combo : access Gtk_Combo_Button_Record)
                      return Gtk.Frame.Gtk_Frame
   is
      function Internal (Combo : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gtk_combo_button_get_frame");
      Stub : Gtk.Frame.Gtk_Frame_Record;
   begin
      return Gtk.Frame.Gtk_Frame
        (Get_User_Data (Internal (Get_Object (Combo)), Stub));
   end Get_Frame;

end Gtk.Extra.Combo_Button;
