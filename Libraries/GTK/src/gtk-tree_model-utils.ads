-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2008-2013, AdaCore              --
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

package Gtk.Tree_Model.Utils is

   function Init_Tree_Iter
     (Stamp       : Glib.Gint;
      User_Data_1 : System.Address := System.Null_Address;
      User_Data_2 : System.Address := System.Null_Address;
      User_Data_3 : System.Address := System.Null_Address)
      return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Is_Null (Self : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  Returns True if specified iterator is null (its internal stamp equals
   --  to zero).

   function Is_Valid
     (Self  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Stamp : Glib.Gint) return Boolean;
   --  Returns True if specified iterator is null iterator or its internal
   --  stamp is equal to the specified stamp.

   function Get_Stamp (Self : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gint;

   function Get_User_Data_1
     (Self : Gtk.Tree_Model.Gtk_Tree_Iter) return System.Address;

   function Get_User_Data_2
     (Self : Gtk.Tree_Model.Gtk_Tree_Iter) return System.Address;

   function Get_User_Data_3
     (Self : Gtk.Tree_Model.Gtk_Tree_Iter) return System.Address;

end Gtk.Tree_Model.Utils;
