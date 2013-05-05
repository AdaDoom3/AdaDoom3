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

with Gtk.Tree_Model;
with Gtkada.Abstract_Tree_Model;

package Gtkada.Abstract_List_Model is

   type Gtk_Abstract_List_Model_Record is
   abstract new Gtkada.Abstract_Tree_Model.Gtk_Abstract_Tree_Model_Record
   with private;

   type Gtk_Abstract_List_Model is
      access all Gtk_Abstract_List_Model_Record'Class;

   procedure Initialize (Self : access Gtk_Abstract_List_Model_Record'Class);

   function Get_Flags
     (Self : access Gtk_Abstract_List_Model_Record)
      return Gtk.Tree_Model.Tree_Model_Flags;
   --  Return a set of flags supported by this interface.

   function Has_Child
     (Self : access Gtk_Abstract_List_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  Return True if Iter has children, False otherwise.

   function Children
     (Self   : access Gtk_Abstract_List_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Returns the first child of Parent.

   function Parent
     (Self  : access Gtk_Abstract_List_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Returns the parent of Child. For the list it always returns Null_Iter.

private

   type Gtk_Abstract_List_Model_Record is
   abstract new Gtkada.Abstract_Tree_Model.Gtk_Abstract_Tree_Model_Record
   with null record;

end Gtkada.Abstract_List_Model;
