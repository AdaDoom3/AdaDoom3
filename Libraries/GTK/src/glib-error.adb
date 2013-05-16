-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2001-2006 AdaCore                    --
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
with Ada.Unchecked_Conversion;

package body Glib.Error is

   type GError_Struct is record
      Domain  : GQuark;
      Code    : Gint;
      Message : Interfaces.C.Strings.chars_ptr;
   end record;
   pragma Convention (C, GError_Struct);
   --  The underlying C struct as defined in gerror.h

   type GError_Struct_Access is access all GError_Struct;
   pragma Convention (C, GError_Struct_Access);

   pragma Warnings (Off);
   --  Kill non relevant warnings about To_Gerror

   function To_Gerror is new Ada.Unchecked_Conversion
     (GError, GError_Struct_Access);

   ---------------
   -- Error_New --
   ---------------

   function Error_New
     (Domain : GQuark; Code : Gint; Message : String) return GError
   is
      function Internal
        (Domain : GQuark; Code : Gint; Message : String) return GError;
      pragma Import (C, Internal, "g_error_new_literal");

   begin
      return Internal (Domain, Code, Message & ASCII.NUL);
   end Error_New;

   -------------------
   -- Error_Matches --
   -------------------

   function Error_Matches
     (Error : GError; Domain : GQuark; Code : Gint) return Boolean
   is
      function Internal
        (Error : GError; Domain : GQuark; Code : Gint) return Gboolean;
      pragma Import (C, Internal, "g_error_matches");

   begin
      return Internal (Error, Domain, Code) /= 0;
   end Error_Matches;

   ----------------
   -- Get_Domain --
   ----------------

   function Get_Domain (Error : GError) return GQuark is
   begin
      return To_Gerror (Error).Domain;
   end Get_Domain;

   --------------
   -- Get_Code --
   --------------

   function Get_Code (Error : GError) return Gint is
   begin
      return To_Gerror (Error).Code;
   end Get_Code;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message (Error : GError) return String is
   begin
      return Value (To_Gerror (Error).Message);
   end Get_Message;

end Glib.Error;
