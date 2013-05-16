/*
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
*/

#define GTK_ENABLE_BROKEN
#include <glib.h>
#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include <gtk/gtktext.h>

/******************************************
 ** Functions for Editable
 ******************************************/

guint
ada_editable_get_editable (GtkOldEditable* widget)
{
  return widget->editable;
}

void
ada_editable_set_editable (GtkOldEditable* widget, guint val)
{
  widget->editable = val;
}

gchar*
ada_editable_get_clipboard_text (GtkOldEditable* widget)
{
   return widget->clipboard_text;
}

guint
ada_editable_get_has_selection (GtkOldEditable* widget)
{
   return widget->has_selection;
}

guint
ada_editable_get_selection_end_pos (GtkOldEditable* widget)
{
   return widget->selection_end_pos;
}

guint
ada_editable_get_selection_start_pos (GtkOldEditable* widget)
{
   return widget->selection_start_pos;
}

/******************************************
 ** Functions for Text
 ******************************************/

GdkWindow*
ada_text_get_text_area (GtkText* widget)
{
  return widget->text_area;
}

guint
ada_text_get_gap_position (GtkText* widget)
{
   return widget->gap_position;
}

guint
ada_text_get_gap_size (GtkText* widget)
{
   return widget->gap_size;
}

guchar*
ada_text_get_text (GtkText* widget)
{
   return widget->text.ch;
}

guint
ada_text_get_text_end (GtkText* widget)
{
   return widget->text_end;
}

GtkAdjustment*
ada_text_get_hadj (GtkText* widget)
{
  return widget->hadj;
}

GtkAdjustment*
ada_text_get_vadj (GtkText* widget)
{
  return widget->vadj;
}

