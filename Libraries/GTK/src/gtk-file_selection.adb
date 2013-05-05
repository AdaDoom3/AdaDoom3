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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Gtk.Widget;           use Gtk.Widget;
with GNAT.Strings;         use GNAT.Strings;
with System;               use System;

with Glib.Type_Conversion_Hooks;

package body Gtk.File_Selection is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_File_Selection_Record);
   pragma Warnings (Off, Type_Conversion);

   ---------------------
   -- Get_Action_Area --
   ---------------------

   function Get_Action_Area
     (File_Selection : access Gtk_File_Selection_Record) return Gtk.Box.Gtk_Box
   is
      function Internal
        (File_Selection : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_action_area");

      Stub : Gtk.Box.Gtk_Box_Record;

   begin
      return Gtk.Box.Gtk_Box
        (Get_User_Data (Internal (Get_Object (File_Selection)), Stub));
   end Get_Action_Area;

   ---------------------
   -- Get_Button_Area --
   ---------------------

   function Get_Button_Area
     (File_Selection : access Gtk_File_Selection_Record) return Gtk.Box.Gtk_Box
   is
      function Internal
        (File_Selection : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_button_area");

      Stub : Gtk.Box.Gtk_Box_Record;

   begin
      return Gtk.Box.Gtk_Box
        (Get_User_Data (Internal (Get_Object (File_Selection)), Stub));
   end Get_Button_Area;

   --------------
   -- Complete --
   --------------

   procedure Complete
     (File_Selection : access Gtk_File_Selection_Record;
      Pattern        : UTF8_String)
   is
      procedure Internal
        (File_Selection : System.Address;
         Pattern        : UTF8_String);
      pragma Import (C, Internal, "gtk_file_selection_complete");

   begin
      Internal (Get_Object (File_Selection), Pattern & ASCII.NUL);
   end Complete;

   -----------------------
   -- Get_Cancel_Button --
   -----------------------

   function Get_Cancel_Button
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Button.Gtk_Button
   is
      function Internal
        (File_Selection : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_cancel_button");

      Stub : Gtk.Button.Gtk_Button_Record;

   begin
      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (File_Selection)), Stub));
   end Get_Cancel_Button;

   ------------------
   -- Get_Dir_List --
   ------------------

   function Get_Dir_List
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (File_Selection : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_dir_list");

   begin
      return Convert (Internal (Get_Object (File_Selection)));
   end Get_Dir_List;

   -------------------
   -- Get_File_List --
   -------------------

   function Get_File_List
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (File_Selection : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_file_list");

   begin
      return Convert (Internal (Get_Object (File_Selection)));
   end Get_File_List;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename
     (File_Selection : access Gtk_File_Selection_Record) return UTF8_String
   is
      function Internal
        (File_Selection : System.Address)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_file_selection_get_filename");

   begin
      return Interfaces.C.Strings.Value
        (Internal (Get_Object (File_Selection)));
   end Get_Filename;

   ---------------------
   -- Get_Help_Button --
   ---------------------

   function Get_Help_Button
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Button.Gtk_Button
   is
      function Internal
        (File_Selection : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_help_button");

      Stub : Gtk.Button.Gtk_Button_Record;

   begin
      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (File_Selection)), Stub));
   end Get_Help_Button;

   --------------------------
   -- Get_History_Pulldown --
   --------------------------

   function Get_History_Pulldown
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (File_Selection : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_history_pulldown");
   begin
      return Convert (Internal (Get_Object (File_Selection)));
   end Get_History_Pulldown;

   -------------------
   -- Get_Ok_Button --
   -------------------

   function Get_Ok_Button
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Button.Gtk_Button
   is
      function Internal
        (File_Selection : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_ok_button");

      Stub : Gtk.Button.Gtk_Button_Record;

   begin
      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (File_Selection)), Stub));
   end Get_Ok_Button;

   -------------------------
   -- Get_Selection_Entry --
   -------------------------

   function Get_Selection_Entry
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (File_Selection : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_selection_entry");

   begin
      return Convert (Internal (Get_Object (File_Selection)));
   end Get_Selection_Entry;

   ------------------------
   -- Get_Selection_Text --
   ------------------------

   function Get_Selection_Text
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (File_Selection : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_selection_text");

   begin
      return Convert (Internal (Get_Object (File_Selection)));
   end Get_Selection_Text;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (File_Selection : out Gtk_File_Selection;
      Title          : UTF8_String) is
   begin
      File_Selection := new Gtk_File_Selection_Record;
      Initialize (File_Selection, Title);
   end Gtk_New;

   -------------------------
   -- Hide_Fileop_Buttons --
   -------------------------

   procedure Hide_Fileop_Buttons
     (File_Selection : access Gtk_File_Selection_Record)
   is
      procedure Internal (File_Selection : System.Address);
      pragma Import (C, Internal, "gtk_file_selection_hide_fileop_buttons");

   begin
      Internal (Get_Object (File_Selection));
   end Hide_Fileop_Buttons;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (File_Selection : access Gtk_File_Selection_Record'Class;
      Title          : UTF8_String)
   is
      function Internal (Title : UTF8_String) return System.Address;
      pragma Import (C, Internal, "gtk_file_selection_new");

   begin
      Set_Object (File_Selection, Internal (Title & ASCII.NUL));
   end Initialize;

   ------------------
   -- Set_Filename --
   ------------------

   procedure Set_Filename
     (File_Selection : access Gtk_File_Selection_Record;
      Filename       : UTF8_String)
   is
      procedure Internal
        (File_Selection : System.Address; Filename : UTF8_String);
      pragma Import (C, Internal, "gtk_file_selection_set_filename");

   begin
      Internal (Get_Object (File_Selection), Filename & ASCII.NUL);
   end Set_Filename;

   -------------------------
   -- Show_Fileop_Buttons --
   -------------------------

   procedure Show_Fileop_Buttons
     (File_Selection : access Gtk_File_Selection_Record)
   is
      procedure Internal (File_Selection : System.Address);
      pragma Import (C, Internal, "gtk_file_selection_show_fileop_buttons");

   begin
      Internal (Get_Object (File_Selection));
   end Show_Fileop_Buttons;

   ------------------------------
   -- Set_Show_File_Op_Buttons --
   ------------------------------

   procedure Set_Show_File_Op_Buttons
     (File_Selection : access Gtk_File_Selection_Record; Flag : Boolean) is
   begin
      if Flag then
         Show_Fileop_Buttons (File_Selection);
      else
         Hide_Fileop_Buttons (File_Selection);
      end if;
   end Set_Show_File_Op_Buttons;

   -------------------------
   -- Get_Select_Multiple --
   -------------------------

   function Get_Select_Multiple
     (Filesel : access Gtk_File_Selection_Record)
      return Boolean
   is
      function Internal
        (Filesel : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_file_selection_get_select_multiple");
   begin
      return Boolean'Val (Internal (Get_Object (Filesel)));
   end Get_Select_Multiple;

   --------------------
   -- Get_Selections --
   --------------------

   function Get_Selections
     (Filesel : access Gtk_File_Selection_Record)
      return GNAT.Strings.String_List
   is
      function Internal (Filesel : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_file_selection_get_selections");

      function Get_Length (S : System.Address) return Integer;
      pragma Import (C, Get_Length, "ada_string_array_length");

      function Get (S : System.Address; Index : Natural) return chars_ptr;
      pragma Import (C, Get, "ada_string_array_get");

      Result : constant System.Address := Internal (Get_Object (Filesel));
   begin
      if Result = System.Null_Address then
         return (1 .. 0 => null);
      else
         declare
            Output : String_List (1 .. Get_Length (Result));
            Tmp    : chars_ptr;
         begin
            for L in Output'Range loop
               Tmp := Get (Result, L - Output'First);
               Output (L) := new String'(Value (Tmp));
               Free (Tmp);
            end loop;
            return Output;
         end;
      end if;
   end Get_Selections;

   -------------------------
   -- Set_Select_Multiple --
   -------------------------

   procedure Set_Select_Multiple
     (Filesel         : access Gtk_File_Selection_Record;
      Select_Multiple : Boolean)
   is
      procedure Internal
        (Filesel         : System.Address;
         Select_Multiple : Gboolean);
      pragma Import (C, Internal, "gtk_file_selection_set_select_multiple");
   begin
      Internal (Get_Object (Filesel), Boolean'Pos (Select_Multiple));
   end Set_Select_Multiple;

end Gtk.File_Selection;
