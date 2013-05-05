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

--  <group>Pango, font handling</group>

with Glib;
with Glib.Object;
with Pango.Font;

package Pango.Context is

   type Pango_Context_Record is new Glib.Object.GObject_Record with private;
   type Pango_Context is access all Pango_Context_Record'Class;
   --  A pango context is somewhat similar to a graphic context, and groups all
   --  the information on how to handle the various international scripts
   --  (font, color, direction,...)
   --  Such a context is destroyed by calling Glib.Object.Unref on it.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Pango_Context.

   procedure Set_Font_Description
     (Context     : Pango_Context;
      Description : Pango.Font.Pango_Font_Description);

   function Get_Font_Description (Context : Pango_Context)
      return Pango.Font.Pango_Font_Description;

   function Load_Font
     (Context : access Pango_Context_Record'Class;
      Descr   : Pango.Font.Pango_Font_Description)
      return Pango.Font.Pango_Font;
   --  Load a new font from its description

private
   type Pango_Context_Record is new Glib.Object.GObject_Record
     with null record;
   pragma Import (C, Get_Type, "pango_context_get_type");
end Pango.Context;

--  missing:
--  pango_context_list_families
--  pango_context_load_fontset
--  pango_context_get_metrics
--  pango_context_get_language
--  pango_context_set_language
--  pango_context_set_base_dir
--  pango_context_get_base_dir
