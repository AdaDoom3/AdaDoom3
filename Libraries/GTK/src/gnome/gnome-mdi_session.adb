-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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
with System;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Gnome.MDI_Session is

   -----------------------
   -- MDI_Restore_State --
   -----------------------

   function MDI_Restore_State
     (MDI               : access Gnome.MDI.Gnome_MDI_Record'Class;
      Section           : String;
      Child_Create_Func : Gnome_MDI_Child_Creator) return Boolean
   is
      function Internal
        (MDI               : System.Address;
         Section           : String;
         Child_Create_Func : System.Address) return Gint;
      pragma Import (C, Internal, "gnome_mdi_restore_state");

      function Stub_Child_Create_Func (Str : chars_ptr) return System.Address;
      pragma Convention (C, Stub_Child_Create_Func);
      --  The real handler

      function Stub_Child_Create_Func
        (Str : chars_ptr) return System.Address is
      begin
         return Get_Object (Child_Create_Func (Value (Str)));
      end Stub_Child_Create_Func;

   begin
      return Boolean'Val (Internal
        (Get_Object (MDI),
         Section & ASCII.NUL,
         Stub_Child_Create_Func'Address));
   end MDI_Restore_State;

   --------------------
   -- MDI_Save_State --
   --------------------

   procedure MDI_Save_State
     (MDI     : access Gnome.MDI.Gnome_MDI_Record'Class;
      Section : String)
   is
      procedure Internal
        (MDI     : System.Address;
         Section : String);
      pragma Import (C, Internal, "gnome_mdi_save_state");
   begin
      Internal (Get_Object (MDI), Section & ASCII.NUL);
   end MDI_Save_State;

end Gnome.MDI_Session;
