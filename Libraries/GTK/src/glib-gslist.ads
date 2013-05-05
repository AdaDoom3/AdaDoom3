-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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
--
--  This packages provides the implementation of a generic single-linked
--  list.
--  One instantiation is found in Gtk.Widget.Widget_Slist for a list of
--  widgets.
--
--  See the documentation of Glib.Glist for more information, it provides
--  the same API as this package.
--  Single linked lists are traversed the same way as double-linked lists,
--  even though most subprograms are less efficient than their
--  double-linked counterparts.
--
--  </description>
--  <c_version>1.2.6</c_version>
--  <group>Glib, the general-purpose library</group>

with System;

package Glib.GSlist is

   --  <doc_ignore>

   generic
      type Gpointer (<>) is private;
      with function Convert (P : Gpointer) return System.Address is <>;
      with function Convert (S : System.Address) return Gpointer is <>;
   package Generic_SList is

      type GSlist is private;
      Null_List : constant GSlist;

      procedure Alloc (List : out GSlist);
      procedure Append (List : in out GSlist;
                        Data : in Gpointer);
      function Concat (List1 : in GSlist;
                       List2 : in GSlist)
                       return GSlist;
      procedure Insert (List : in out GSlist;
                        Data : in Gpointer;
                        Position : in Gint);
      function Find (List : in GSlist;
                     Data : in Gpointer)
                     return GSlist;
      procedure Free (List : in out GSlist);
      function Get_Data (List : in GSlist)
                         return Gpointer;

      function Get_Data_Address (List : GSlist) return System.Address;
      --  Return directly the System.Address contained in the C list.
      --  This is used mainly internally in GtkAda to implement String lists,
      --  and you should not have to use this subprogram yourself.

      function Index (List : in GSlist;
                      Data : in Gpointer)
                      return Gint;
      function Last (List : in GSlist)
                     return GSlist;
      function Length (List : in GSlist)
                       return Guint;
      procedure List_Reverse (List : in out GSlist);
      function Next (List : in GSlist)
                     return GSlist;
      function Nth (List : in GSlist;
                    N    : in Guint)
                    return GSlist;
      function Nth_Data (List : in GSlist;
                         N : in Guint)
                         return Gpointer;
      function Position (List : in GSlist;
                         Link : in GSlist)
                         return Gint;
      procedure Prepend (List : in out GSlist;
                         Data : in Gpointer);
      procedure Remove (List : in out GSlist;
                        Data : in Gpointer);
      procedure Remove_Link (List : in out GSlist;
                             Link : in GSlist);
      function Get_Object (Obj : in GSlist)
                           return System.Address;
      pragma Inline (Get_Object);
      procedure Set_Object (Obj    : in out GSlist;
                            Value  : in     System.Address);
      pragma Inline (Set_Object);
   private

      type GSlist is
         record
            Ptr : System.Address := System.Null_Address;
         end record;
      Null_List : constant GSlist := (Ptr => System.Null_Address);
   end Generic_SList;

   --  </doc_ignore>

end Glib.GSlist;
