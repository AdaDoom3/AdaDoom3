-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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
--  This package implements a generic double-linked list.
--  Such lists are used throughout GtkAda to contain lists of widgets
--  (for the children of containers, or for the list of selected widgets
--  in a Gtk_Clist for instance), list of strings (for Gtk_Combo_Box),...
--
--  They provide a common interface to traverse these lists.
--
--  One useful note: you should only Free the lists that you have allocated
--  yourself, and not the lists that are returned by the subprograms in
--  GtkAda and should be left under GtkAda's control.
--
--  See the example below for an example on how to traverse a list.
--
--  Instantiating the package Generic_List requires two functions to convert
--  back and forth between your data type and a System.Address which is the
--  type stored at the C level.
--  Note that the lists used in GtkAda already have associated packages, like
--  Gtk.Enums.Gint_List, Gtk.Enums.String_List or Gtk.Widget.Widget_List.
--
--  </description>
--  <c_version>1.2.6</c_version>
--  <group>Glib, the general-purpose library</group>

with System;

package Glib.Glist is

   generic
      --  <doc_ignore>

      type Gpointer (<>) is private;
      with function Convert (P : Gpointer) return System.Address is <>;
      with function Convert (S : System.Address) return Gpointer is <>;

      --  </doc_ignore>

   package Generic_List is

      type Glist is private;
      --  This type is both a list and an item in the list.
      --  Each item points to its successor.

      Null_List : constant Glist;

      procedure Alloc (List : out Glist);
      --  Allocate a new item in the list.
      --  This item isn't associated with any data.
      --  You probably don't have to use this subprogram, since Append,
      --  Insert, Prepend, etc. already handle the allocation for you and
      --  give a new value to the item.

      procedure Append (List : in out Glist; Data : Gpointer);
      --  Add a new item at the end of the list, and stores the new list
      --  directly back in List.
      --  The complexity of this operation is O(n)

      function Concat (List1 : Glist; List2 : Glist) return Glist;
      --  Concatenate two lists, and return the result.
      --  List2 is added at the end of List1.
      --  The complexity is O(n1) (depends on the size of List1).

      procedure Insert
        (List     : in out Glist;
         Data     : Gpointer;
         Position : Gint);
      --  Insert an item in the middle of a list.
      --  If Position is 0, the item is added at the beginning of the list, if
      --  it is negative the item is added at the end.
      --  The complexity is O(Position).

      function Find (List : Glist; Data : Gpointer) return Glist;
      --  Find a value in the list, and return the first item that contains it.
      --  Note that this function will not work if the function Convert does
      --  not return the same value for two identical values.

      function First (List : Glist) return Glist;
      --  Return the first item in the list.
      --  Note that if List is in fact an item of a larger list, the return
      --  value is the first item in the larger list itself.

      procedure Free (List : in out Glist);
      --  Free the list (but does not free the data in each of its elements).
      --  This only frees the memory associated with the list itself.
      --  You should only use this function on the lists that
      --  you have created yourself, not on the list that are returned by some
      --  functions in GtkAda (like Gtk.Clist.Get_Selection). These functions
      --  return directly the list managed by the underlying C widget, and you
      --  should never free the result yourself.
      --
      --  Note also that the memory might not be actually freed. For efficiency
      --  reasons, GtkAda will keep the memory allocated and try to reuse it as
      --  much as possible.

      function Get_Data (List : Glist) return Gpointer;
      --  Return the value pointed to by List.
      --  The System.Address container in the C list is converted to a Gpointer
      --  through a call to Convert.

      function Get_Data_Address (List : Glist) return System.Address;
      --  Return directly the System.Address contained in the C list.
      --  This is used mainly internally in GtkAda to implement String lists,
      --  and you should not have to use this subprogram yourself.

      --  <doc_ignore>
      function Get_Gpointer (List : Glist) return Gpointer;
      --  Sometimes, the data is not stored in the "data" field
      --  of each cell, but rather at each cell. In such cases,
      --  to retrieve the address of the data, we need to return
      --  the address of the cell itself, insted of the address
      --  pointed to by data.
      --
      --  Ex: the Gtk_Ctree row_list.
      --  </doc_ignore>

      function Index (List : Glist; Data : Gpointer) return Gint;
      --  Return the index of the first element in List that contains Data.
      --  Note that this function is irrelevant if Convert does not return the
      --  same value for two identical data.

      function Last (List : Glist) return Glist;
      --  Return the last element in the list.

      function Length (List : Glist) return Guint;
      --  Return the number of elements in the list.
      --  The last item's index is Length - 1.

      procedure List_Reverse (List : in out Glist);
      --  Reverse the order of the list (the last item becomes the first, etc.)

      function Next (List : Glist) return Glist;
      --  Returns the Item following List in the global list that contains
      --  both.
      --  If there is no such item, return Null_List. This is how you
      --  stop iterating over a list.

      function Nth (List : Glist; N : Guint) return Glist;
      --  Give the nth item following LIST in the global list that
      --  contains both.
      --  If there is no such item, return Null_List.

      function Nth_Data (List : Glist; N : Guint) return Gpointer;
      --  Return the Data contained in the N-th item of List.
      --  The result is undefined if there is no such item in the list.
      --  The actual result in that case is the result of
      --      Convert (System.Null_Address);
      --  which might not mean anything.

      function Position (List : Glist; Link : Glist) return Gint;
      --  Return the position of Link in the List.
      --  If Link is not contained in the list, -1 is returned.

      procedure Prepend (List : in out Glist; Data : Gpointer);
      --  Add an item at the beginning of the list.
      --  This operation always succeed.

      function Prev (List : Glist) return Glist;
      --  Return the item before List in the global list that contains both.
      --  Return Null_List if there is no such item.

      procedure Remove (List : in out Glist; Data : Gpointer);
      --  Remove the first item in List that contains Data.
      --  Note that this operation can succeed only if Convert always return
      --  the same address for a given value.

      procedure Remove_Link (List : in out Glist; Link : Glist);
      --  Remove Link from the list to which it belongs.
      --  If that list is not List, no error is returned, but Link is removed
      --  anyway.

      function Is_Created (List : Glist) return Boolean;
      --  Return True if there is a C widget associated with List.

      ------------------------
      -- Internal functions --
      ------------------------
      --  Please do not use the following functions. They are used internally
      --  by GtkAda.
      --  <doc_ignore>

      function Get_Object (Obj : Glist) return System.Address;
      --  Returns the C object contained in Obj.
      pragma Inline (Get_Object);

      procedure Set_Object (Obj : in out Glist; Value : System.Address);
      --  Modifies the C object contained in Obj.
      pragma Inline (Set_Object);

      --  </doc_ignore>

   private

      type Glist is record
         Ptr : System.Address := System.Null_Address;
      end record;

      Null_List : constant Glist := (Ptr => System.Null_Address);
   end Generic_List;

end Glib.Glist;

--  <example>
--  <include>../examples/documentation/glist_traverse.adb</include>
--  </example>
