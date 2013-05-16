-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                 Copyright (C) 2008-2013, AdaCore                  --
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

with Gtk.Tree_Model; use Gtk.Tree_Model;

package body Gtkada.Abstract_List_Model is

   --------------
   -- Children --
   --------------

   function Children
     (Self   : access Gtk_Abstract_List_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
   begin
      if Parent = Null_Iter then
         declare
            Path : constant Gtk.Tree_Model.Gtk_Tree_Path
              := Gtk.Tree_Model.Gtk_New_First;
            Iter : Gtk.Tree_Model.Gtk_Tree_Iter;

         begin
            Iter :=
              Gtk.Tree_Model.Get_Iter
                (Gtk.Tree_Model.Gtk_Tree_Model (Self), Path);
            Gtk.Tree_Model.Path_Free (Path);

            return Iter;
         end;
      end if;

      return Gtk.Tree_Model.Null_Iter;
   end Children;

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags
     (Self : access Gtk_Abstract_List_Model_Record)
      return Gtk.Tree_Model.Tree_Model_Flags
   is
      pragma Unreferenced (Self);

   begin
      return Gtk.Tree_Model.Tree_Model_List_Only;
   end Get_Flags;

   ---------------
   -- Has_Child --
   ---------------

   function Has_Child
     (Self : access Gtk_Abstract_List_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
   begin
      if Iter = Null_Iter then
         return
           Gtk.Tree_Model.Children
             (Gtk.Tree_Model.Gtk_Tree_Model (Self), (Iter)) /= Null_Iter;

      else
         return False;
      end if;
   end Has_Child;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : access Gtk_Abstract_List_Model_Record'Class) is
   begin
      Gtkada.Abstract_Tree_Model.Initialize (Self);
   end Initialize;

   ------------
   -- Parent --
   ------------

   function Parent
     (Self  : access Gtk_Abstract_List_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      pragma Unreferenced (Self, Child);

   begin
      return Gtk.Tree_Model.Null_Iter;
   end Parent;

end Gtkada.Abstract_List_Model;
