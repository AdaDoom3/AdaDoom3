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

with Glib.Values;
with Gtk.Tree_Model;

package Gtkada.Abstract_Tree_Model is

   type Gtk_Abstract_Tree_Model_Record is
      abstract new Gtk.Tree_Model.Gtk_Tree_Model_Record with private;

   type Gtk_Abstract_Tree_Model is
      access all Gtk_Abstract_Tree_Model_Record'Class;

   procedure Initialize (Self : access Gtk_Abstract_Tree_Model_Record'Class);

   ------------------------------
   -- Interface implementation --
   ------------------------------

   --  The following subprograms can be overridden to implement the custom
   --  tree model.
   --  Note that they are called from C (wrapped through calls to the
   --  Dispatch_* functions defined in the body of this package) so it is
   --  advised to add exception handlers in these subprograms, just like in
   --  regular GtkAda callbacks.

   function Get_Flags
     (Self : access Gtk_Abstract_Tree_Model_Record)
      return Gtk.Tree_Model.Tree_Model_Flags;
   --  Override this to return a set of flags supported by this interface.
   --  The flags supported should not change during the lifecycle of the
   --  tree_model.

   function Get_N_Columns
     (Self : access Gtk_Abstract_Tree_Model_Record)
      return Glib.Gint is abstract;
   --  Override this to return the number of columns supported by Tree_Model.

   function Get_Column_Type
     (Self  : access Gtk_Abstract_Tree_Model_Record;
      Index : Glib.Gint) return Glib.GType is abstract;
   --  Override this to return the type of the Index-th column in the model.

   function Get_Iter
     (Self : access Gtk_Abstract_Tree_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter is abstract;
   --  Override this return an iterator pointing to Path.
   --  Null_Iter is returned if Path was invalid or no iterator could be set.

   function Get_Path
     (Self : access Gtk_Abstract_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path is abstract;
   --  Override this to return a newly created Gtk_Tree_Path referenced by
   --  Iter. This path will be freed with Path_Free by the caller.

   procedure Get_Value
     (Self   : access Gtk_Abstract_Tree_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue) is abstract;
   --  Override this get a value from the model, at column Column and line
   --  Iter. Value must be freed by the caller.

   procedure Next
     (Self : access Gtk_Abstract_Tree_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter) is abstract;
   --  Override this to set Iter to point to the node following it at the
   --  current level. If there is none, Iter is set to Null_Iter.

   function Children
     (Self   : access Gtk_Abstract_Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter is abstract;
   --  Override this to return the first child of Parent. If Parent has no
   --  children, return Null_Iter. Parent will remain a valid node after this
   --  function has been called.

   function Has_Child
     (Self : access Gtk_Abstract_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean is abstract;
   --  Override this to return True if Iter has children, False otherwise.

   function N_Children
     (Self : access Gtk_Abstract_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint is abstract;
   --  Override this to return the number of children that Iter has.
   --  As a special case, if Iter is Null_Iter, then the number of toplevel
   --  nodes is returned.

   function Nth_Child
     (Self   : access Gtk_Abstract_Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter is abstract;
   --  Override this to return the child of Parent, using the given index.
   --  The First index is 0. If Index is too big, or Parent has no children,
   --  return Null_Iter. If Parent is Null_Iter, then the nth root node is set.

   function Parent
     (Self  : access Gtk_Abstract_Tree_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter is abstract;
   --  Override this to return the parent of Child. If Child is at the
   --  toplevel, and doesn't have a parent, then Null_Iter is returned.

   procedure Ref_Node
     (Self : access Gtk_Abstract_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Let the tree reference the node.
   --  This is an optional method for models to implement.
   --  To be more specific, models may ignore this call as it exists primarily
   --  for performance reasons. This function is primarily meant as a way for
   --  views to let caching model know when nodes are being displayed (and
   --  hence, whether or not to cache that node). For example, a file-system
   --  based model would not want to keep the entire file-hierarchy in memory,
   --  just the sections that are currently being displayed by every current
   --  view.
   --  Technically, the idea is to increase the refcount for the node itself,
   --  not for any data associated with it (should you want to associate a
   --  reference counted type with the rows). Most of the time you will not
   --  need to do anything here.
   --  Every time the view makes a row visible (for instance when you expand
   --  a node), it calls Ref_Node for that row. When the row is hidden again,
   --  it calls Unref_Node.

   procedure Unref_Node
     (Self : access Gtk_Abstract_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Let the tree unref the node.
   --  This is an optional method for models to implement. To be more specific,
   --  models may ignore this call as it exists primarily for performance
   --  reasons. For more information on what this means, please see
   --  Tree_Model_Ref_Node. Please note that nodes that are deleted are not
   --  unreferenced.
   --  Technically, your model is the one deleting a row (and it should do so
   --  only if the refcount for the row is not 1, see Ref_Node). Thus gtk+
   --  avoids a potential callback to your application by not emitting
   --  Unref_Node in such a case.

private

   type Gtk_Abstract_Tree_Model_Record is
      abstract new Gtk.Tree_Model.Gtk_Tree_Model_Record with null record;

end Gtkada.Abstract_Tree_Model;
