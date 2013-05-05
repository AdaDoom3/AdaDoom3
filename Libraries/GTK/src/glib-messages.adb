-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2003-2013, AdaCore                --
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

package body Glib.Messages is

   procedure C_Log_Func
     (Log_Domain : chars_ptr;
      Log_Level  : Log_Level_Flags;
      Message    : chars_ptr;
      Ada_Func   : Log_Function);
   --  Low level log wrapper
   pragma Convention (C, C_Log_Func);

   ----------------
   -- C_Log_Func --
   ----------------

   procedure C_Log_Func
     (Log_Domain : chars_ptr;
      Log_Level  : Log_Level_Flags;
      Message    : chars_ptr;
      Ada_Func   : Log_Function) is
   begin
      if Log_Domain = Null_Ptr then
         Ada_Func ("", Log_Level, Value (Message));
      else
         Ada_Func (Value (Log_Domain), Log_Level, Value (Message));
      end if;
   end C_Log_Func;

   ---------------------
   -- Log_Set_Handler --
   ---------------------

   function Log_Set_Handler
     (Log_Domain : String;
      Log_Levels : Log_Level_Flags;
      Log_Func   : Log_Function) return Log_Handler_Id
   is
      function Internal
        (Log_Domain : String;
         Log_Levels : Log_Level_Flags;
         Log_Func   : System.Address;
         User_Data  : System.Address) return Log_Handler_Id;
      pragma Import (C, Internal, "g_log_set_handler");

   begin
      return Internal
        (Log_Domain & ASCII.NUL, Log_Levels, C_Log_Func'Address,
         Log_Func.all'Address);
   end Log_Set_Handler;

   ------------------------
   -- Log_Remove_Handler --
   ------------------------

   procedure Log_Remove_Handler
     (Log_Domain : String;
      Handler_Id : Log_Handler_Id)
   is
      procedure Internal
        (Log_Domain : String;
         Handler_Id : Log_Handler_Id);
      pragma Import (C, Internal, "g_log_remove_handler");

   begin
      Internal (Log_Domain & ASCII.NUL, Handler_Id);
   end Log_Remove_Handler;

   -------------------------
   -- Log_Default_Handler --
   -------------------------

   procedure Log_Default_Handler
     (Log_Domain : String;
      Log_Levels : Log_Level_Flags;
      Message    : UTF8_String)
   is
      procedure Internal
        (Log_Domain : String;
         Log_Levels : Log_Level_Flags;
         Message    : UTF8_String);
      pragma Import (C, Internal, "g_log_default_handler");

   begin
      Internal (Log_Domain & ASCII.NUL, Log_Levels, Message & ASCII.NUL);
   end Log_Default_Handler;

   ---------
   -- Log --
   ---------

   procedure Log
     (Log_Domain : String;
      Log_Levels : Log_Level_Flags;
      Message    : UTF8_String)
   is
      procedure Internal
        (Log_Domain : String;
         Log_Levels : Log_Level_Flags;
         Format     : UTF8_String);
      pragma Import (C, Internal, "ada_g_log");

   begin
      Internal (Log_Domain & ASCII.NUL, Log_Levels, Message & ASCII.NUL);
   end Log;

   ------------------------
   -- Log_Set_Fatal_Mask --
   ------------------------

   function Log_Set_Fatal_Mask
     (Log_Domain : String;
      Fatal_Mask : Log_Level_Flags) return Log_Level_Flags
   is
      function Internal
        (Log_Domain : String;
         Fatal_Mask : Log_Level_Flags) return Log_Level_Flags;
      pragma Import (C, Internal, "g_log_set_fatal_mask");

   begin
      return Internal (Log_Domain & ASCII.NUL, Fatal_Mask);
   end Log_Set_Fatal_Mask;

end Glib.Messages;
