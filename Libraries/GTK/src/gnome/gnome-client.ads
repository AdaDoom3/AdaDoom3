-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                  Copyright (C) 2001-2013, AdaCore                 --
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

with Glib; use Glib;
with Gnome.Dialog;
with Glib.Object;
with Gtkada.Types; use Gtkada.Types;
with Gtk;

package Gnome.Client is

   type Gnome_Client_Record is new Glib.Object.GObject_Record with private;
   type Gnome_Client is access all Gnome_Client_Record'Class;

   type Gnome_Interact_Style is (
      Interact_None,
      Interact_Errors,
      Interact_Any);

   type Gnome_Dialog_Type is (
      Dialog_Error,
      Dialog_Normal);

   type Gnome_Save_Style is (
      Save_Global,
      Save_Local,
      Save_Both);

   type Gnome_Restart_Style is (
      Restart_If_Running,
      Restart_Anyway,
      Restart_Immediately,
      Restart_Never);

   type Gnome_Client_State is (
      Idle,
      Saving_Phase_1,
      Waiting_For_Phase_2,
      Saving_Phase_2,
      Frozen,
      Disconnected,
      Registering);

   type Gnome_Client_Flags is mod 2 ** 32;
   Is_Connected : constant Gnome_Client_Flags := 2 ** 0;
   Restarted : constant Gnome_Client_Flags := 2 ** 1;
   Restored : constant Gnome_Client_Flags := 2 ** 2;

   procedure Gnome_New
     (Widget          : out Gnome_Client;
      With_Connection : Boolean := True);

   procedure Initialize
     (Widget          : access Gnome_Client_Record'Class;
      With_Connection : Boolean);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   --  gnome_client_add_static_arg not bound: variable number of arguments

   procedure Connect (Client : access Gnome_Client_Record);

   procedure Disable_Master_Connection;

   procedure Disconnect (Client : access Gnome_Client_Record);

   procedure Flush (Client : access Gnome_Client_Record);

   function Get_Config_Prefix (Client : access Gnome_Client_Record)
                               return String;

   function Get_Flags (Client : access Gnome_Client_Record)
                       return Gnome_Client_Flags;

   function Get_Global_Config_Prefix (Client : access Gnome_Client_Record)
                                      return String;

   function Get_Id (Client : access Gnome_Client_Record)
                    return String;

   function Get_Previous_Id (Client : access Gnome_Client_Record)
                             return String;

   procedure Interaction_Key_Return
     (Key             : Gint;
      Cancel_Shutdown : Boolean);

   function Master_Client return Gnome_Client;

   type Gnome_Interact_Function is access procedure
     (Client      : access Gnome_Client_Record'Class;
      Key         : Gint;
      Dialog_Type : Gnome_Dialog_Type);

   --  procedure Request_Interaction
   --    (Client      : access Gnome_Client_Record;
   --     Dialog      : Gnome_Dialog_Type;
   --     Func        : Gnome_Interact_Function;
   --     Client_Data : System.Address);

   procedure Request_Phase_2 (Client : access Gnome_Client_Record);

   procedure Request_Save
     (Client         : access Gnome_Client_Record;
      Save_Style     : Gnome_Save_Style;
      Shutdown       : Boolean;
      Interact_Style : Gnome_Interact_Style;
      Fast           : Boolean;
      Global         : Boolean);

   procedure Save_Any_Dialog
     (Client : access Gnome_Client_Record;
      Dialog : access Gnome.Dialog.Gnome_Dialog_Record'Class);

   procedure Save_Error_Dialog
     (Client : access Gnome_Client_Record;
      Dialog : access Gnome.Dialog.Gnome_Dialog_Record'Class);

   procedure Set_Clone_Command
     (Client : access Gnome_Client_Record;
      Argc   : Gint;
      Argv   : Chars_Ptr_Array);

   procedure Set_Current_Directory
     (Client : access Gnome_Client_Record;
      Dir    : String);

   procedure Set_Discard_Command
     (Client : access Gnome_Client_Record;
      Argc   : Gint;
      Argv   : Chars_Ptr_Array);

   procedure Set_Environment
     (Client : access Gnome_Client_Record;
      Name   : String;
      Value  : String);

   procedure Set_Global_Config_Prefix
     (Client : access Gnome_Client_Record;
      Prefix : String);

   procedure Set_Id
     (Client    : access Gnome_Client_Record;
      Client_Id : String);

   procedure Set_Priority
     (Client   : access Gnome_Client_Record;
      Priority : Guint);

   procedure Set_Process_Id
     (Client : access Gnome_Client_Record;
      Pid    : Gint);

   procedure Set_Program
     (Client  : access Gnome_Client_Record;
      Program : String);

   procedure Set_Resign_Command
     (Client : access Gnome_Client_Record;
      Argc   : Gint;
      Argv   : Chars_Ptr_Array);

   procedure Set_Restart_Command
     (Client : access Gnome_Client_Record;
      Argc   : Gint;
      Argv   : Chars_Ptr_Array);

   procedure Set_Restart_Style
     (Client : access Gnome_Client_Record;
      Style  : Gnome_Restart_Style);

   procedure Set_Shutdown_Command
     (Client : access Gnome_Client_Record;
      Argc   : Gint;
      Argv   : Chars_Ptr_Array);

   procedure Set_User_Id
     (Client  : access Gnome_Client_Record;
      User_Id : String);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "save_yourself"
   --    function Handler (Widget : access Gnome_Client_Record'Class;
   --       Phase : Gint;
   --       Save_Style : Gnome_Save_Style;
   --       Shutdown : Gint;
   --       Interact_Style : Gnome_Interact_Style;
   --       Fast : Gint)
   --       return Gboolean;
   --
   --  - "die"
   --    procedure Handler (Widget : access Gnome_Client_Record'Class);
   --
   --  - "save_complete"
   --    procedure Handler (Widget : access Gnome_Client_Record'Class);
   --
   --  - "shutdown_cancelled"
   --    procedure Handler (Widget : access Gnome_Client_Record'Class);
   --
   --  - "connect"
   --    procedure Handler (Widget : access Gnome_Client_Record'Class;
   --       Restarted : Gint);
   --
   --  - "disconnect"
   --    procedure Handler (Widget : access Gnome_Client_Record'Class);
   --
   --  </signals>

private
   type Gnome_Client_Record is new
     Glib.Object.GObject_Record with null record;

   pragma Import (C, Get_Type, "gnome_client_get_type");
end Gnome.Client;
