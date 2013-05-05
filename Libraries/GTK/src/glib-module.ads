-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                        Copyright (C) 2000                         --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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
--  This package provides wrapper code for dynamic module loading
--  </description>
--  <group>Glib, the general-purpose library</group>

package Glib.Module is
   pragma Preelaborate;

   type Module_Flags is mod 2 ** 32;
   Module_Bind_Lazy : constant Module_Flags := 2 ** 0;
   Module_Bind_Mask : constant Module_Flags := 16#1#;

   type G_Module is new C_Proxy;

   function Module_Supported return Boolean;
   --  Return True if dynamic module loading is supported

   function Module_Open
     (File_Name : String;
      Flags     : Module_Flags := Module_Bind_Lazy) return G_Module;
   --  Open a module `file_name' and return handle, which is null on error.

   function Module_Close (Module : G_Module) return Boolean;
   --  Close a previously opened module, return True on success.

   procedure Module_Make_Resident (Module : G_Module);
   --  Make a module resident so Module_Close on it will be ignored

   function Module_Error return String;
   --  Query the last module error as a string

   generic
      type Pointer is private;
      --  This is typically a pointer to procedure/function.

   procedure Generic_Module_Symbol
     (Module      : G_Module;
      Symbol_Name : String;
      Symbol      : out Pointer;
      Success     : out Boolean);
   --  Retrieve a symbol pointer from `module'.
   --  Success is set to True on success.

   function Module_Name (Module : G_Module) return String;
   --  Retrieve the file name from an existing module

   function Module_Build_Path
     (Directory   : String;
      Module_Name : String) return String;
   --  Build the actual file name containing a module.
   --  `directory' is the directory where the module file is supposed to be, or
   --  the null string in which case it should either be in the current
   --  directory or, on some operating systems, in some standard place, for
   --  instance on the PATH. Hence, to be absolutely sure to get the correct
   --  module, always pass in a directory. The file name consists of the
   --  directory, if supplied, and `module_name' suitably decorated accoring to
   --  the operating system's conventions (for instance lib*.so or *.dll).
   --
   --  No checks are made that the file exists, or is of correct type.

private
   pragma Import (C, Module_Make_Resident, "g_module_make_resident");
end Glib.Module;
