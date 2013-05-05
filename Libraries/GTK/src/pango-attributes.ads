-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2002 ACT-Europe                 --
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
--  This package provides a set of types and subprograms to manipulate the
--  attributes of text displayed in a pango_layout
--
--  </description>
--  <group>Pango, font handling</group>

with Glib;
with Pango.Enums;

package Pango.Attributes is

   ----------------
   -- Attributes --
   ----------------

   type Pango_Attribute is new Glib.C_Proxy;

   function Attr_Underline_New
     (Underline : Pango.Enums.Underline) return Pango_Attribute;
   --  Create a new underline attribute

   ---------------------
   -- Attributes list --
   ---------------------

   type Pango_Attr_List is new Glib.C_Proxy;

   procedure Gdk_New (Attr_List : out Pango_Attr_List);
   --  Create a new empty list of attributes

   procedure Ref (Attr_List : Pango_Attr_List);
   --  Increment the reference count of the attribute list

   procedure Unref (Attr_List : Pango_Attr_List);
   --  Decrement the reference count of the attribute list. When it reaches 0,
   --  the list is destroyed.

   procedure Insert
     (Attr_List : Pango_Attr_List;
      Attribute : Pango_Attribute);
   --  Insert a new attribute in the list

private
   pragma Import (C, Ref,   "pango_attr_list_ref");
   pragma Import (C, Unref, "pango_attr_list_unref");
   pragma Import (C, Insert, "pango_attr_list_insert");
   pragma Import (C, Attr_Underline_New, "pango_attr_underline_new");

end Pango.Attributes;
