-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2001-2002 ACT-Europe                 --
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
--
--  This package provides definitions for the error handling mechanism used in
--  Glib, Gdk and Gtk.
--
--  </description>
--  <c_version>1.3.11</c_version>
--  <group>Glib, the general-purpose library</group>

package Glib.Error is
   pragma Preelaborate;

   type GError is new C_Proxy;

   type GError_Access is access all GError;

   function Error_New
     (Domain : GQuark; Code : Gint; Message : String) return GError;
   --  Create a new GError object.

   procedure Error_Free (Error : GError);
   --  Free the memory associated with a GError.

   function Error_Copy (Error : GError) return GError;
   --  Duplicate a GError object.

   function Error_Matches
     (Error : GError; Domain : GQuark; Code : Gint) return Boolean;
   --  Return whether a given GError matches a domain/code.

   function Get_Domain (Error : GError) return GQuark;
   --  Return the domain associated with a GError.

   function Get_Code (Error : GError) return Gint;
   --  Return the code associated with a GError.

   function Get_Message (Error : GError) return String;
   --  Return the message associated with a GError.

private

   pragma Import (C, Error_Free, "g_error_free");
   pragma Import (C, Error_Copy, "g_error_copy");

end Glib.Error;
