-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
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

package body Gtk.Color_Selection_Dialog is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Color_Selection_Dialog_Record);
   pragma Warnings (Off, Type_Conversion);

   ------------------
   -- Get_Colorsel --
   ------------------

   function Get_Colorsel
     (Color_Selection_Dialog : access Gtk_Color_Selection_Dialog_Record)
      return Gtk.Color_Selection.Gtk_Color_Selection
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_colorsel_dialog_get_colorsel");

      Stub : Gtk.Color_Selection.Gtk_Color_Selection_Record;

   begin
      return Gtk.Color_Selection.Gtk_Color_Selection
        (Get_User_Data (Internal (Get_Object (Color_Selection_Dialog)), Stub));
   end Get_Colorsel;

   -------------------
   -- Get_OK_Button --
   -------------------

   function Get_OK_Button
     (Color_Selection_Dialog : access Gtk_Color_Selection_Dialog_Record)
      return Gtk.Button.Gtk_Button
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_colorsel_dialog_get_ok_button");

      Stub : Gtk.Button.Gtk_Button_Record;

   begin
      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (Color_Selection_Dialog)), Stub));
   end Get_OK_Button;

   -----------------------
   -- Get_Cancel_Button --
   -----------------------

   function Get_Cancel_Button
     (Color_Selection_Dialog : access Gtk_Color_Selection_Dialog_Record)
      return Gtk.Button.Gtk_Button
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_colorsel_dialog_get_cancel_button");

      Stub : Gtk.Button.Gtk_Button_Record;

   begin
      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (Color_Selection_Dialog)), Stub));
   end Get_Cancel_Button;

   ---------------------
   -- Get_Help_Button --
   ---------------------

   function Get_Help_Button
     (Color_Selection_Dialog : access Gtk_Color_Selection_Dialog_Record)
      return Gtk.Button.Gtk_Button
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_colorsel_dialog_get_help_button");

      Stub : Gtk.Button.Gtk_Button_Record;

   begin
      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (Color_Selection_Dialog)), Stub));
   end Get_Help_Button;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Color_Selection_Dialog : out Gtk_Color_Selection_Dialog;
      Title                  : UTF8_String) is
   begin
      Color_Selection_Dialog := new Gtk_Color_Selection_Dialog_Record;
      Initialize (Color_Selection_Dialog, Title);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Color_Selection_Dialog : access Gtk_Color_Selection_Dialog_Record'Class;
      Title                  : UTF8_String)
   is
      function Internal (S : UTF8_String) return System.Address;
      pragma Import (C, Internal, "gtk_color_selection_dialog_new");

   begin
      Set_Object (Color_Selection_Dialog, Internal (Title & ASCII.NUL));
   end Initialize;

end Gtk.Color_Selection_Dialog;
