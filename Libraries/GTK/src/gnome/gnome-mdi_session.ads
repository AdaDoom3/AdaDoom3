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

with Gnome.MDI;
with Gnome.MDI_Child;

package Gnome.MDI_Session is

   type Gnome_MDI_Child_Creator is access
     function (Str : String) return Gnome.MDI_Child.Gnome_MDI_Child;
   --  This function should parse the config string and return a newly
   --  created GnomeMDIChild.

   function MDI_Restore_State
     (MDI               : access Gnome.MDI.Gnome_MDI_Record'Class;
      Section           : String;
      Child_Create_Func : Gnome_MDI_Child_Creator) return Boolean;

   procedure MDI_Save_State
     (MDI     : access Gnome.MDI.Gnome_MDI_Record'Class;
      Section : String);

end Gnome.MDI_Session;
