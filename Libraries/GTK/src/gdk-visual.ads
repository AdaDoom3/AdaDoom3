-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
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

--  <c_version>1.3.6</c_version>
--  <group>Gdk, the low-level API</group>

with Glib; use Glib;
with Glib.Glist;
pragma Elaborate_All (Glib.Glist);

with System;
with Unchecked_Conversion;

package Gdk.Visual is

   subtype Gdk_Visual is Gdk.Gdk_Visual;
   Null_Visual : constant Gdk_Visual;
   --  This type is not private because we need the full declaration
   --  to instanciate Glib.Glist.Generic_List with it.

   type Gdk_Visual_Type is
     (Visual_Static_Gray,
      Visual_Grayscale,
      Visual_Static_Color,
      Visual_Pseudo_Color,
      Visual_True_Color,
      Visual_Direct_Color);
   pragma Convention (C, Gdk_Visual_Type);

   type Gdk_Visual_Type_Array is array (Natural range <>) of Gdk_Visual_Type;

   function Get_Type return Glib.GType;
   --  Return the internal value associated with Gdk_Visual.

   function Get_Best_Depth return Gint;

   function Get_Best_Type return Gdk_Visual_Type;

   function Get_System return Gdk_Visual;

   function Get_Best return Gdk_Visual;

   procedure Get_Best (Visual : out Gdk_Visual);

   function Get_Best (Depth  : Gint) return Gdk_Visual;

   function Get_Best (Visual_Type : Gdk_Visual_Type) return Gdk_Visual;

   function Get_Best
     (Depth       : Gint;
      Visual_Type : Gdk_Visual_Type) return Gdk_Visual;

   function Query_Depths return Gint_Array;

   function Query_Visual_Types return Gdk_Visual_Type_Array;

   function Convert is new Unchecked_Conversion (Gdk_Visual, System.Address);
   function Convert is new Unchecked_Conversion (System.Address, Gdk_Visual);

   package Gdk_Visual_List is new Glib.Glist.Generic_List (Gdk_Visual);

   function List_Visuals return Gdk_Visual_List.Glist;

private
   Null_Visual : constant Gdk_Visual := null;
   pragma Import (C, Get_Type, "gdk_visual_get_type");
   pragma Import (C, Get_Best_Depth, "gdk_visual_get_best_depth");
   pragma Import (C, Get_Best_Type, "gdk_visual_get_best_type");
   pragma Import (C, Get_System, "gdk_visual_get_system");

end Gdk.Visual;
