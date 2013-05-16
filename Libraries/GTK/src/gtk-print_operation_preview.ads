-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010-2013, AdaCore               --
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
--  This package contains functionality for composing a custom print preview
--  facility.
--  </description>
--  <c_version>2.16.6</c_version>

with Glib.Types;

package Gtk.Print_Operation_Preview is

   type Gtk_Print_Operation_Preview is new Glib.Types.GType_Interface;

   function Get_Type return GType;

   procedure End_Preview (Preview : Gtk_Print_Operation_Preview);
   --  Ends a preview.
   --  This function must be called to finish a custom print preview.

   function Is_Selected
     (Preview : Gtk_Print_Operation_Preview;
      Page_Nr : Gint)
      return Boolean;
   --  Returns whether the given page is included in the set of pages that
   --  have been selected for printing.

   procedure Render_Page
     (Preview : Gtk_Print_Operation_Preview;
      Page_Nr : Gint);
   --  Renders a page to the preview, using the print context that
   --  was passed to the "preview" handler together with Preview.
   --
   --  A custom iprint preview should use this function in its "expose"
   --  handler to render the currently selected page.
   --
   --  Note that this function requires a suitable cairo context to
   --  be associated with the print context.

private

   pragma Import (C, Get_Type, "gtk_print_operation_preview_get_type");
   pragma Import (C, End_Preview, "gtk_print_operation_preview_end_preview");
   pragma Import (C, Render_Page, "gtk_print_operation_preview_render_page");

end Gtk.Print_Operation_Preview;
