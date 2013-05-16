-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2003 ACT-Europe                   --
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
--  This package provides low level routines for enabling, disabling
--  and modifying the way log messages are handled in glib/gdk/gtk.
--  </description>
--  <group>Glib, the general-purpose library</group>

package Glib.Messages is
   pragma Preelaborate;

   type Log_Level_Flags is mod 2 ** 32;
   --  log levels and flags.

   ---------------
   -- log flags --
   ---------------

   Log_Flag_Recursion : constant Log_Level_Flags := 2 ** 0;
   Log_Flag_Fatal     : constant Log_Level_Flags := 2 ** 1;

   ----------------
   -- log levels --
   ----------------

   Log_Level_Error    : constant Log_Level_Flags := 2 ** 2;
   --  always fatal

   Log_Level_Critical : constant Log_Level_Flags := 2 ** 3;
   Log_Level_Warning  : constant Log_Level_Flags := 2 ** 4;
   Log_Level_Message  : constant Log_Level_Flags := 2 ** 5;
   Log_Level_Info     : constant Log_Level_Flags := 2 ** 6;
   Log_Level_Debug    : constant Log_Level_Flags := 2 ** 7;

   Log_Level_Mask     : constant Log_Level_Flags :=
     not (Log_Flag_Recursion or Log_Flag_Fatal);

   Log_Fatal_Mask     : constant Log_Level_Flags :=
     Log_Flag_Recursion or Log_Level_Error;
   --  log levels that are considered fatal by default

   type Log_Function is access procedure
     (Log_Domain : String;
      Log_Level  : Log_Level_Flags;
      Message    : UTF8_String);

   type Log_Handler_Id is new Guint;

   --  Logging mechanism

   function Log_Set_Handler
     (Log_Domain : String;
      Log_Levels : Log_Level_Flags;
      Log_Func   : Log_Function) return Log_Handler_Id;
   --  Set a log function for the given log levels, and return its id.

   procedure Log_Remove_Handler
     (Log_Domain : String;
      Handler_Id : Log_Handler_Id);
   --  Unset a given handler.

   procedure Log_Default_Handler
     (Log_Domain : String;
      Log_Levels : Log_Level_Flags;
      Message    : UTF8_String);
   --  The default log handler.
   --  Can be called e.g. within a user defined log handler.

   procedure Log
     (Log_Domain : String;
      Log_Levels : Log_Level_Flags;
      Message    : UTF8_String);
   --  Log a message through the glib logging facility.

   function Log_Set_Fatal_Mask
     (Log_Domain : String;
      Fatal_Mask : Log_Level_Flags) return Log_Level_Flags;
   --  Set the level at which messages are considered fatal for a given domain.

   function Log_Set_Always_Fatal
     (Fatal_Mask : Log_Level_Flags) return Log_Level_Flags;
   --  Set the level at which messages are considered fatal for any domain.

private

   pragma Import (C, Log_Set_Always_Fatal, "g_log_set_always_fatal");

end Glib.Messages;
