-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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

with System;

package body Gnome is

   ----------
   -- Init --
   ----------

   function Init (App_Id : String; App_Version : String) return Boolean is
      gnat_argc : Integer;
      pragma Import (C, gnat_argc);

      gnat_argv : System.Address;
      pragma Import (C, gnat_argv);

      function Internal
        (App_Id      : String;
         App_Version : String;
         Argc        : Integer;
         Argv        : System.Address;
         Popt_Option : System.Address;
         Flags       : Integer;
         Context     : System.Address) return Integer;
      pragma Import (C, Internal, "gnome_init_with_popt_table");

   begin
      return Internal
        (App_Id & ASCII.NUL,
         App_Version & ASCII.NUL,
         gnat_argc,
         gnat_argv, System.Null_Address, 0, System.Null_Address) /= 0;
   end Init;

end Gnome;
