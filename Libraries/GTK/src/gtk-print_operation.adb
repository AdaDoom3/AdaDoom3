-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010-2013, AdaCore               --
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
with Gtkada.Bindings;      use Gtkada.Bindings;

package body Gtk.Print_Operation is

   ------------
   -- Cancel --
   ------------

   procedure Cancel (Op : access Gtk_Print_Operation_Record) is
      procedure Internal (Op : System.Address);
      pragma Import (C, Internal, "gtk_print_operation_cancel");
   begin
      Internal (Get_Object (Op));
   end Cancel;

   ----------------------
   -- Draw_Page_Finish --
   ----------------------

   procedure Draw_Page_Finish (Op : access Gtk_Print_Operation_Record) is
      procedure Internal (Op : System.Address);
      pragma Import (C, Internal, "gtk_print_operation_draw_page_finish");
   begin
      Internal (Get_Object (Op));
   end Draw_Page_Finish;

   ----------------------------
   -- Get_Default_Page_Setup --
   ----------------------------

   function Get_Default_Page_Setup
     (Op : access Gtk_Print_Operation_Record)
      return Gtk.Page_Setup.Gtk_Page_Setup
   is
      function Internal (Op : System.Address) return System.Address;
      pragma Import
        (C, Internal, "gtk_print_operation_get_default_page_setup");
      Stub : Gtk.Page_Setup.Gtk_Page_Setup_Record;
   begin
      return Gtk.Page_Setup.Gtk_Page_Setup (Get_User_Data
        (Internal (Get_Object (Op)), Stub));
   end Get_Default_Page_Setup;

   ---------------
   -- Get_Error --
   ---------------

   procedure Get_Error
     (Op    : access Gtk_Print_Operation_Record;
      Error : Glib.Error.GError)
   is
      procedure Internal
        (Op    : System.Address;
         Error : Glib.Error.GError);
      pragma Import (C, Internal, "gtk_print_operation_get_error");
   begin
      Internal (Get_Object (Op), Error);
   end Get_Error;

   ------------------------
   -- Get_Print_Settings --
   ------------------------

   function Get_Print_Settings
     (Op : access Gtk_Print_Operation_Record)
      return Gtk.Print_Settings.Gtk_Print_Settings
   is
      function Internal (Op : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_operation_get_print_settings");
      Stub : Gtk.Print_Settings.Gtk_Print_Settings_Record;
   begin
      return Gtk.Print_Settings.Gtk_Print_Settings (Get_User_Data
        (Internal (Get_Object (Op)), Stub));
   end Get_Print_Settings;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status
     (Op : access Gtk_Print_Operation_Record) return Gtk_Print_Status
   is
      function Internal (Op : System.Address) return Gtk_Print_Status;
      pragma Import (C, Internal, "gtk_print_operation_get_status");
   begin
      return Internal (Get_Object (Op));
   end Get_Status;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status
     (Op : access Gtk_Print_Operation_Record) return String
   is
      function Internal (Op : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_print_operation_get_status_string");
   begin
      return Value (Internal (Get_Object (Op)));
   end Get_Status;

   -----------------
   -- Is_Finished --
   -----------------

   function Is_Finished
     (Op : access Gtk_Print_Operation_Record) return Boolean
   is
      function Internal (Op : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_print_operation_is_finished");
   begin
      return Boolean'Val (Internal (Get_Object (Op)));
   end Is_Finished;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Print_Operation) is
   begin
      Widget := new Gtk_Print_Operation_Record;
      Gtk.Print_Operation.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Print_Operation_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_print_operation_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   ---------
   -- Run --
   ---------

   function Run
     (Op     : access Gtk_Print_Operation_Record;
      Action : Gtk_Print_Operation_Action;
      Parent : access Gtk.Window.Gtk_Window_Record'Class;
      Error  : Glib.Error.GError := null)
      return Gtk_Print_Operation_Result
   is
      function Internal
        (Op     : System.Address;
         Action : Gtk_Print_Operation_Action;
         Parent : System.Address;
         Error  : Glib.Error.GError)
         return Gtk_Print_Operation_Result;
      pragma Import (C, Internal, "gtk_print_operation_run");
   begin
      return Internal (Get_Object (Op), Action, Get_Object (Parent), Error);
   end Run;

   ---------------------
   -- Set_Allow_Async --
   ---------------------

   procedure Set_Allow_Async
     (Op          : access Gtk_Print_Operation_Record;
      Allow_Async : Boolean)
   is
      procedure Internal
        (Op          : System.Address;
         Allow_Async : Gboolean);
      pragma Import (C, Internal, "gtk_print_operation_set_allow_async");
   begin
      Internal (Get_Object (Op), Boolean'Pos (Allow_Async));
   end Set_Allow_Async;

   ----------------------
   -- Set_Current_Page --
   ----------------------

   procedure Set_Current_Page
     (Op           : access Gtk_Print_Operation_Record;
      Current_Page : Gint)
   is
      procedure Internal
        (Op           : System.Address;
         Current_Page : Gint);
      pragma Import (C, Internal, "gtk_print_operation_set_current_page");
   begin
      Internal (Get_Object (Op), Current_Page);
   end Set_Current_Page;

   --------------------------
   -- Set_Custom_Tab_Label --
   --------------------------

   procedure Set_Custom_Tab_Label
     (Op    : access Gtk_Print_Operation_Record;
      Label : String)
   is
      procedure Internal
        (Op    : System.Address;
         Label : chars_ptr);
      pragma Import (C, Internal, "gtk_print_operation_set_custom_tab_label");
      L : chars_ptr := String_Or_Null (Label);
   begin
      Internal (Get_Object (Op), L);
      Free (L);
   end Set_Custom_Tab_Label;

   ----------------------------
   -- Set_Default_Page_Setup --
   ----------------------------

   procedure Set_Default_Page_Setup
     (Op                 : access Gtk_Print_Operation_Record;
      Default_Page_Setup : access Gtk.Page_Setup.Gtk_Page_Setup_Record'Class)
   is
      procedure Internal
        (Op                 : System.Address;
         Default_Page_Setup : System.Address);
      pragma Import
        (C, Internal, "gtk_print_operation_set_default_page_setup");
   begin
      Internal (Get_Object (Op), Get_Object (Default_Page_Setup));
   end Set_Default_Page_Setup;

   -----------------------
   -- Set_Defer_Drawing --
   -----------------------

   procedure Set_Defer_Drawing (Op : access Gtk_Print_Operation_Record) is
      procedure Internal (Op : System.Address);
      pragma Import (C, Internal, "gtk_print_operation_set_defer_drawing");
   begin
      Internal (Get_Object (Op));
   end Set_Defer_Drawing;

   -------------------------
   -- Set_Export_Filename --
   -------------------------

   procedure Set_Export_Filename
     (Op       : access Gtk_Print_Operation_Record;
      Filename : String)
   is
      procedure Internal
        (Op       : System.Address;
         Filename : chars_ptr);
      pragma Import (C, Internal, "gtk_print_operation_set_export_filename");
      F : chars_ptr := String_Or_Null (Filename);
   begin
      Internal (Get_Object (Op), F);
      Free (F);
   end Set_Export_Filename;

   ------------------
   -- Set_Job_Name --
   ------------------

   procedure Set_Job_Name
     (Op       : access Gtk_Print_Operation_Record;
      Job_Name : String)
   is
      procedure Internal
        (Op       : System.Address;
         Job_Name : String);
      pragma Import (C, Internal, "gtk_print_operation_set_job_name");
   begin
      Internal (Get_Object (Op), Job_Name & ASCII.NUL);
   end Set_Job_Name;

   -----------------
   -- Set_N_Pages --
   -----------------

   procedure Set_N_Pages
     (Op      : access Gtk_Print_Operation_Record;
      N_Pages : Gint)
   is
      procedure Internal
        (Op      : System.Address;
         N_Pages : Gint);
      pragma Import (C, Internal, "gtk_print_operation_set_n_pages");
   begin
      Internal (Get_Object (Op), N_Pages);
   end Set_N_Pages;

   ------------------------
   -- Set_Print_Settings --
   ------------------------

   procedure Set_Print_Settings
     (Op             : access Gtk_Print_Operation_Record;
      Print_Settings : access
                       Gtk.Print_Settings.Gtk_Print_Settings_Record'Class)
   is
      procedure Internal
        (Op             : System.Address;
         Print_Settings : System.Address);
      pragma Import (C, Internal, "gtk_print_operation_set_print_settings");
   begin
      Internal (Get_Object (Op), Get_Object (Print_Settings));
   end Set_Print_Settings;

   -----------------------
   -- Set_Show_Progress --
   -----------------------

   procedure Set_Show_Progress
     (Op            : access Gtk_Print_Operation_Record;
      Show_Progress : Boolean)
   is
      procedure Internal
        (Op            : System.Address;
         Show_Progress : Gboolean);
      pragma Import (C, Internal, "gtk_print_operation_set_show_progress");
   begin
      Internal (Get_Object (Op), Boolean'Pos (Show_Progress));
   end Set_Show_Progress;

   ----------------------------
   -- Set_Track_Print_Status --
   ----------------------------

   procedure Set_Track_Print_Status
     (Op           : access Gtk_Print_Operation_Record;
      Track_Status : Boolean)
   is
      procedure Internal
        (Op           : System.Address;
         Track_Status : Gboolean);
      pragma Import
        (C, Internal, "gtk_print_operation_set_track_print_status");
   begin
      Internal (Get_Object (Op), Boolean'Pos (Track_Status));
   end Set_Track_Print_Status;

   --------------
   -- Set_Unit --
   --------------

   procedure Set_Unit
     (Op   : access Gtk_Print_Operation_Record;
      Unit : Gtk.Enums.Gtk_Unit)
   is
      procedure Internal
        (Op   : System.Address;
         Unit : Gtk.Enums.Gtk_Unit);
      pragma Import (C, Internal, "gtk_print_operation_set_unit");
   begin
      Internal (Get_Object (Op), Unit);
   end Set_Unit;

   -----------------------
   -- Set_Use_Full_Page --
   -----------------------

   procedure Set_Use_Full_Page
     (Op        : access Gtk_Print_Operation_Record;
      Full_Page : Boolean)
   is
      procedure Internal
        (Op        : System.Address;
         Full_Page : Gboolean);
      pragma Import (C, Internal, "gtk_print_operation_set_use_full_page");
   begin
      Internal (Get_Object (Op), Boolean'Pos (Full_Page));
   end Set_Use_Full_Page;

   ---------------------------
   -- Run_Page_Setup_Dialog --
   ---------------------------

   function Run_Page_Setup_Dialog
     (Parent     : access Gtk.Window.Gtk_Window_Record'Class;
      Page_Setup : access Gtk.Page_Setup.Gtk_Page_Setup_Record'Class;
      Settings   : access Gtk.Print_Settings.Gtk_Print_Settings_Record'Class)
      return Gtk.Page_Setup.Gtk_Page_Setup
   is
      function Internal
        (Parent     : System.Address;
         Page_Setup : System.Address;
         Settings   : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_print_run_page_setup_dialog");
      Stub : Gtk.Page_Setup.Gtk_Page_Setup_Record;
   begin
      return Gtk.Page_Setup.Gtk_Page_Setup (Get_User_Data (Internal
        (Get_Object (Parent), Get_Object (Page_Setup), Get_Object (Settings)),
         Stub));
   end Run_Page_Setup_Dialog;

end Gtk.Print_Operation;
