-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--      Copyright (C) 2000 E. Briot, J. Brobecker and A. Charlet     --
--                Copyright (C) 2000-2003 ACT-Europe                 --
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
--  This package provides a high level support for creating file selection
--  dialogs by handling the signals internally.
--  </description>
--  <group>Selectors</group>

with Glib;

package Gtkada.File_Selection is

   function File_Selection_Dialog
     (Title       : Glib.UTF8_String := "Select File";
      Default_Dir : String := "";
      Dir_Only    : Boolean := False;
      Must_Exist  : Boolean := False) return String;
   --  Open a file selection dialog and make it modal.
   --  Return when either the Cancel button is clicked or when a file is
   --  selected.
   --  Default_Dir is the directory to display in dialog initially. Note that
   --  it must end with a directory separator ('/' or '\', depending on your
   --  system). You can use GNAT.Os_Lib.Directory_Separator to get the correct
   --  value for your system.
   --  If Must_Exist is True, then the file (or directory if Dir_Only is True)
   --  must exist.
   --  If Dir_Only is True, then the dialog is modified so that the user can
   --  only choose a directory name, but not a file name.
   --  The value returned is the name of the file selected, or "" if none.

end Gtkada.File_Selection;
