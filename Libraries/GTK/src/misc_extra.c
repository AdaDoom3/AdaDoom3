/*
-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--      Copyright (C) 2000 E. Briot, J. Brobecker and A. Charlet     --
--                Copyright (C) 2000-2006 AdaCore                    --
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

#include <gtk/gtk.h>

#include "gtkextra/gtkpsfont.h"
#include "gtkextra/gtkfontcombo.h"
#include "gtkextra/gtkcombobutton.h"
#include "gtkextra/gtkcolorcombo.h"
#include "gtkextra/gtksheet.h"
#include "gtkextra/gtkplot.h"
#include "gtkextra/gtkplotcanvas.h"
#include "gtkextra/gtkplotcanvasplot.h"

/********************************************************************
 **
 **  Gtk_Plot widget
 **
 ********************************************************************/

void
ada_gtk_plot_set_color (GtkPlotLine* line, GdkColor* color) {
  line->color = *color;
}

void
ada_gtk_plot_set_line_style (GtkPlotLine* line, GtkPlotLineStyle style) {
  line->line_style = style;
}

void
ada_gtk_plot_set_line_width (GtkPlotLine* line, gfloat width) {
  line->line_width = width;
}

char*
ada_gtk_dataset_get_name (GtkPlotData* data) {
  return data->name;
}

GList*
ada_gtk_plot_get_datasets (GtkPlot* plot) {
  return plot->data_sets;
}

GList*
ada_gtk_plot_get_texts (GtkPlot* plot) {
  return plot->text;
}

gchar*
ada_gtk_plot_get_text_string (GtkPlotText* text) {
  return text->text;
}

void
ada_gtk_plot_get_text_position (GtkPlotText* text,
				gdouble* x,
				gdouble* y) {
  *x = text->x;
  *y = text->y;
}

/********************************************************************
 **
 **  Gtk_Plot_Canvas widget
 **
 ********************************************************************/

guint
ada_gtk_plot_canvas_flag_is_set (GtkPlotCanvas* canvas, guint16 flag) {
  return ((GTK_PLOT_CANVAS_FLAGS (canvas) & flag) != 0);
}

void
ada_gtk_plot_canvas_set_flags (GtkPlotCanvas* canvas, guint16 flags) {
  GTK_PLOT_CANVAS_SET_FLAGS (canvas, flags);
}

void
ada_gtk_plot_canvas_unset_flags (GtkPlotCanvas* canvas, guint16 flags) {
  GTK_PLOT_CANVAS_UNSET_FLAGS (canvas, flags);
}

/********************************************************************
 **
 **  Gtk_Sheet widget
 **
 ********************************************************************/

GtkWidget*
ada_gtk_sheet_get_widget (GtkSheetChild* child) {
  return child->widget;
}

GtkSheetRange*
ada_gtk_sheet_get_range (GtkSheet* sheet) {
  return &(sheet->range);
}

gint
ada_gtk_sheet_get_column_width (GtkSheet* sheet, gint col) {
  return sheet->column[col].width;
}

gint
ada_gtk_sheet_get_row_height (GtkSheet* sheet, gint row) {
  return sheet->row[row].height;
}

/********************************************************************
 **
 **  Gtk_Combo_Button widget
 **
 ********************************************************************/

GtkWidget*
ada_gtk_combo_button_get_button (GtkComboButton* combo) {
  return combo->button;
}

GtkWidget*
ada_gtk_combo_button_get_arrow (GtkComboButton* combo) {
  return combo->arrow;
}

GtkWidget*
ada_gtk_combo_button_get_frame (GtkComboButton* combo) {
  return combo->frame;
}

/********************************************************************
 **
 **  Gtk_Color_Combo widget
 **
 ********************************************************************/

gint ada_gtk_extra_color_combo_get_ncols (GtkColorCombo* combo) {
  return combo->ncols;
}

gint ada_gtk_extra_color_combo_get_nrows (GtkColorCombo* combo) {
  return combo->nrows;
}

void ada_gtk_extra_color_combo_set_row (GtkColorCombo* combo, gint row) {
  combo->row = row;
}

void ada_gtk_extra_color_combo_set_column (GtkColorCombo* combo, gint col) {
  combo->column = col;
}

/********************************************************************
 **
 **  PsFont
 **
 ********************************************************************/

char*
ada_gtk_psfont_get_psname (GtkPSFont* font) {
  return font->psname;
}

GtkPlotCanvasPlotPos
ada_gtk_plot_canvas_plot_get_pos (GtkPlotCanvasPlot* plot) {
  return plot->pos;
}

GtkPlotData*
ada_gtk_plot_canvas_plot_get_data (GtkPlotCanvasPlot* plot) {
  return plot->data;
}

gint
ada_gtk_plot_canvas_plot_get_datapoint (GtkPlotCanvasPlot* plot) {
  return plot->datapoint;
}

void
ada_gtk_plot_canvas_plot_set_flags (GtkPlotCanvasPlot* plot, int flags) {
   GTK_PLOT_CANVAS_PLOT_SET_FLAGS(plot, flags);
}

void
ada_gtk_plot_canvas_plot_unset_flags (GtkPlotCanvasPlot* plot, int flags) {
   GTK_PLOT_CANVAS_PLOT_UNSET_FLAGS(plot, flags);
}
