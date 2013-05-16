-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--               Copyright (C) 2000-2001 ACT-Europe                  --
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
--  This is the root of the Gnome hierarchy.
--  It provides initialization routines.
--  </description>

pragma Warnings (Off);
with Glib.Object; use Glib.Object;
pragma Warnings (On);

package Gnome is
   pragma Elaborate_Body;

   function Init (App_Id : String; App_Version : String) return Boolean;
   --  Initialize Gnome.
   --  You should call this function before anything other gnome related
   --  actions.
   --  Return True in case of success, False otherwise.

   type Gnome_Preferences_Type is
     (Preferences_Never, Preferences_User, Preferences_Always);
   --  Do something never, only when the user wants, or always.

end Gnome;
