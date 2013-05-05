-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                  Copyright (C) 2001-2006                          --
--                           AdaCore                                 --
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

with Gtk; use Gtk;
with Interfaces.C.Strings;
with System;

package body Gnome.Client is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Widget          : out Gnome_Client;
      With_Connection : Boolean := True) is
   begin
      Widget := new Gnome_Client_Record;
      Initialize (Widget, With_Connection);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget          : access Gnome_Client_Record'Class;
      With_Connection : Boolean)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gnome_client_new");

      function Internal2 return System.Address;
      pragma Import (C, Internal2, "gnome_client_new_without_connection");

   begin
      if With_Connection then
         Set_Object (Widget, Internal);
      else
         Set_Object (Widget, Internal2);
      end if;
   end Initialize;

   -------------
   -- Connect --
   -------------

   procedure Connect (Client : access Gnome_Client_Record) is
      procedure Internal (Client : System.Address);
      pragma Import (C, Internal, "gnome_client_connect");
   begin
      Internal (Get_Object (Client));
   end Connect;

   -------------------------------
   -- Disable_Master_Connection --
   -------------------------------

   procedure Disable_Master_Connection is
      procedure Internal;
      pragma Import (C, Internal, "gnome_client_disable_master_connection");
   begin
      Internal;
   end Disable_Master_Connection;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (Client : access Gnome_Client_Record) is
      procedure Internal (Client : System.Address);
      pragma Import (C, Internal, "gnome_client_disconnect");
   begin
      Internal (Get_Object (Client));
   end Disconnect;

   -----------
   -- Flush --
   -----------

   procedure Flush (Client : access Gnome_Client_Record) is
      procedure Internal (Client : System.Address);
      pragma Import (C, Internal, "gnome_client_flush");
   begin
      Internal (Get_Object (Client));
   end Flush;

   -----------------------
   -- Get_Config_Prefix --
   -----------------------

   function Get_Config_Prefix
     (Client : access Gnome_Client_Record) return String
   is
      function Internal
        (Client : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gnome_client_get_config_prefix");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Client)));
   end Get_Config_Prefix;

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags
     (Client : access Gnome_Client_Record) return Gnome_Client_Flags
   is
      function Internal (Client : System.Address) return Gint;
      pragma Import (C, Internal, "gnome_client_get_flags");
   begin
      return Gnome_Client_Flags'Val (Internal (Get_Object (Client)));
   end Get_Flags;

   ------------------------------
   -- Get_Global_Config_Prefix --
   ------------------------------

   function Get_Global_Config_Prefix
     (Client : access Gnome_Client_Record) return String
   is
      function Internal
        (Client : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gnome_client_get_global_config_prefix");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Client)));
   end Get_Global_Config_Prefix;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (Client : access Gnome_Client_Record) return String is
      function Internal
        (Client : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gnome_client_get_id");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Client)));
   end Get_Id;

   ---------------------
   -- Get_Previous_Id --
   ---------------------

   function Get_Previous_Id
     (Client : access Gnome_Client_Record) return String
   is
      function Internal
        (Client : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gnome_client_get_previous_id");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Client)));
   end Get_Previous_Id;

   ----------------------------
   -- Interaction_Key_Return --
   ----------------------------

   procedure Interaction_Key_Return
     (Key             : Gint;
      Cancel_Shutdown : Boolean)
   is
      procedure Internal
        (Key             : Gint;
         Cancel_Shutdown : Gint);
      pragma Import (C, Internal, "gnome_interaction_key_return");
   begin
      Internal (Key,
                Boolean'Pos (Cancel_Shutdown));
   end Interaction_Key_Return;

   -------------------
   -- Master_Client --
   -------------------

   function Master_Client return Gnome_Client is
      function Internal return System.Address;
      pragma Import (C, Internal, "gnome_master_client");

      Stub : Gnome_Client_Record;
   begin
      return Gnome_Client (Get_User_Data (Internal, Stub));
   end Master_Client;

   -------------------------
   -- Request_Interaction --
   -------------------------

   --  procedure Request_Interaction
   --    (Client      : access Gnome_Client_Record;
   --     Dialog      : Gnome_Dialog_Type;
   --     Func        : Gnome_Interact_Function;
   --     Client_Data : System.Address)
   --  is
   --     procedure Internal
   --       (Client  : System.Address;
   --        Dialog  : Gint;
   --        Func    : Gint;
   --        Data    : System.Address;
   --        Destroy : Gint);
   --     pragma Import
   --       (C, Internal, "gnome_client_request_interaction_interp");
   --  begin
   --     Internal (Get_Object (Client),
   --               Gnome_Dialog_Type'Pos (Dialog),
   --               Gnome_Interact_Function'Pos (Func),
   --               Client_Data);
   --  end Request_Interaction;

   ---------------------
   -- Request_Phase_2 --
   ---------------------

   procedure Request_Phase_2 (Client : access Gnome_Client_Record) is
      procedure Internal (Client : System.Address);
      pragma Import (C, Internal, "gnome_client_request_phase_2");
   begin
      Internal (Get_Object (Client));
   end Request_Phase_2;

   ------------------
   -- Request_Save --
   ------------------

   procedure Request_Save
     (Client         : access Gnome_Client_Record;
      Save_Style     : Gnome_Save_Style;
      Shutdown       : Boolean;
      Interact_Style : Gnome_Interact_Style;
      Fast           : Boolean;
      Global         : Boolean)
   is
      procedure Internal
        (Client         : System.Address;
         Save_Style     : Gint;
         Shutdown       : Gint;
         Interact_Style : Gint;
         Fast           : Gint;
         Global         : Gint);
      pragma Import (C, Internal, "gnome_client_request_save");
   begin
      Internal (Get_Object (Client),
                Gnome_Save_Style'Pos (Save_Style),
                Boolean'Pos (Shutdown),
                Gnome_Interact_Style'Pos (Interact_Style),
                Boolean'Pos (Fast),
                Boolean'Pos (Global));
   end Request_Save;

   ---------------------
   -- Save_Any_Dialog --
   ---------------------

   procedure Save_Any_Dialog
     (Client : access Gnome_Client_Record;
      Dialog : access Gnome.Dialog.Gnome_Dialog_Record'Class)
   is
      procedure Internal
        (Client : System.Address;
         Dialog : System.Address);
      pragma Import (C, Internal, "gnome_client_save_any_dialog");
   begin
      Internal (Get_Object (Client),
                Get_Object (Dialog));
   end Save_Any_Dialog;

   -----------------------
   -- Save_Error_Dialog --
   -----------------------

   procedure Save_Error_Dialog
     (Client : access Gnome_Client_Record;
      Dialog : access Gnome.Dialog.Gnome_Dialog_Record'Class)
   is
      procedure Internal
        (Client : System.Address;
         Dialog : System.Address);
      pragma Import (C, Internal, "gnome_client_save_error_dialog");
   begin
      Internal (Get_Object (Client),
                Get_Object (Dialog));
   end Save_Error_Dialog;

   -----------------------
   -- Set_Clone_Command --
   -----------------------

   procedure Set_Clone_Command
     (Client : access Gnome_Client_Record;
      Argc   : Gint;
      Argv   : Chars_Ptr_Array)
   is
      procedure Internal
        (Client : System.Address;
         Argc   : Gint;
         Argv   : Chars_Ptr_Array);
      pragma Import (C, Internal, "gnome_client_set_clone_command");
   begin
      Internal (Get_Object (Client), Argc, Argv);
   end Set_Clone_Command;

   ---------------------------
   -- Set_Current_Directory --
   ---------------------------

   procedure Set_Current_Directory
     (Client : access Gnome_Client_Record;
      Dir    : String)
   is
      procedure Internal
        (Client : System.Address;
         Dir    : String);
      pragma Import (C, Internal, "gnome_client_set_current_directory");
   begin
      Internal (Get_Object (Client), Dir & ASCII.NUL);
   end Set_Current_Directory;

   -------------------------
   -- Set_Discard_Command --
   -------------------------

   procedure Set_Discard_Command
     (Client : access Gnome_Client_Record;
      Argc   : Gint;
      Argv   : Chars_Ptr_Array)
   is
      procedure Internal
        (Client : System.Address;
         Argc   : Gint;
         Argv   : Chars_Ptr_Array);
      pragma Import (C, Internal, "gnome_client_set_discard_command");
   begin
      Internal (Get_Object (Client), Argc, Argv);
   end Set_Discard_Command;

   ---------------------
   -- Set_Environment --
   ---------------------

   procedure Set_Environment
     (Client : access Gnome_Client_Record;
      Name   : String;
      Value  : String)
   is
      procedure Internal
        (Client : System.Address;
         Name   : String;
         Value  : String);
      pragma Import (C, Internal, "gnome_client_set_environment");
   begin
      Internal (Get_Object (Client), Name & ASCII.NUL, Value & ASCII.NUL);
   end Set_Environment;

   ------------------------------
   -- Set_Global_Config_Prefix --
   ------------------------------

   procedure Set_Global_Config_Prefix
     (Client : access Gnome_Client_Record;
      Prefix : String)
   is
      procedure Internal
        (Client : System.Address;
         Prefix : String);
      pragma Import (C, Internal, "gnome_client_set_global_config_prefix");
   begin
      Internal (Get_Object (Client), Prefix & ASCII.NUL);
   end Set_Global_Config_Prefix;

   ------------
   -- Set_Id --
   ------------

   procedure Set_Id
     (Client    : access Gnome_Client_Record;
      Client_Id : String)
   is
      procedure Internal
        (Client    : System.Address;
         Client_Id : String);
      pragma Import (C, Internal, "gnome_client_set_id");
   begin
      Internal (Get_Object (Client), Client_Id & ASCII.NUL);
   end Set_Id;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (Client   : access Gnome_Client_Record;
      Priority : Guint)
   is
      procedure Internal
        (Client   : System.Address;
         Priority : Guint);
      pragma Import (C, Internal, "gnome_client_set_priority");
   begin
      Internal (Get_Object (Client), Priority);
   end Set_Priority;

   --------------------
   -- Set_Process_Id --
   --------------------

   procedure Set_Process_Id
     (Client : access Gnome_Client_Record;
      Pid    : Gint)
   is
      procedure Internal
        (Client : System.Address;
         Pid    : Gint);
      pragma Import (C, Internal, "gnome_client_set_process_id");
   begin
      Internal (Get_Object (Client), Pid);
   end Set_Process_Id;

   -----------------
   -- Set_Program --
   -----------------

   procedure Set_Program
     (Client  : access Gnome_Client_Record;
      Program : String)
   is
      procedure Internal
        (Client  : System.Address;
         Program : String);
      pragma Import (C, Internal, "gnome_client_set_program");
   begin
      Internal (Get_Object (Client), Program & ASCII.NUL);
   end Set_Program;

   ------------------------
   -- Set_Resign_Command --
   ------------------------

   procedure Set_Resign_Command
     (Client : access Gnome_Client_Record;
      Argc   : Gint;
      Argv   : Chars_Ptr_Array)
   is
      procedure Internal
        (Client : System.Address;
         Argc   : Gint;
         Argv   : Chars_Ptr_Array);
      pragma Import (C, Internal, "gnome_client_set_resign_command");
   begin
      Internal (Get_Object (Client), Argc, Argv);
   end Set_Resign_Command;

   -------------------------
   -- Set_Restart_Command --
   -------------------------

   procedure Set_Restart_Command
     (Client : access Gnome_Client_Record;
      Argc   : Gint;
      Argv   : Chars_Ptr_Array)
   is
      procedure Internal
        (Client : System.Address;
         Argc   : Gint;
         Argv   : Chars_Ptr_Array);
      pragma Import (C, Internal, "gnome_client_set_restart_command");
   begin
      Internal (Get_Object (Client), Argc, Argv);
   end Set_Restart_Command;

   -----------------------
   -- Set_Restart_Style --
   -----------------------

   procedure Set_Restart_Style
     (Client : access Gnome_Client_Record;
      Style  : Gnome_Restart_Style)
   is
      procedure Internal
        (Client : System.Address;
         Style  : Gint);
      pragma Import (C, Internal, "gnome_client_set_restart_style");
   begin
      Internal (Get_Object (Client), Gnome_Restart_Style'Pos (Style));
   end Set_Restart_Style;

   --------------------------
   -- Set_Shutdown_Command --
   --------------------------

   procedure Set_Shutdown_Command
     (Client : access Gnome_Client_Record;
      Argc   : Gint;
      Argv   : Chars_Ptr_Array)
   is
      procedure Internal
        (Client : System.Address;
         Argc   : Gint;
         Argv   : Chars_Ptr_Array);
      pragma Import (C, Internal, "gnome_client_set_shutdown_command");
   begin
      Internal (Get_Object (Client), Argc, Argv);
   end Set_Shutdown_Command;

   -----------------
   -- Set_User_Id --
   -----------------

   procedure Set_User_Id
     (Client  : access Gnome_Client_Record;
      User_Id : String)
   is
      procedure Internal
        (Client  : System.Address;
         User_Id : String);
      pragma Import (C, Internal, "gnome_client_set_user_id");
   begin
      Internal (Get_Object (Client), User_Id & ASCII.NUL);
   end Set_User_Id;

end Gnome.Client;
