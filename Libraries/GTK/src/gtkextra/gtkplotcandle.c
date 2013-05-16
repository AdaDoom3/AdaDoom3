/* gtkplotbox - box plots widget for gtk+
 * Copyright 1999-2001  Adrian E. Feiguin <feiguin@ifir.edu.ar>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <gtk/gtk.h>
#include "gtkplot.h"
#include "gtkplot3d.h"
#include "gtkplotdata.h"
#include "gtkplotcandle.h"
#include "gtkpsfont.h"

static void gtk_plot_candle_class_init 	(GtkPlotCandleClass *klass);
static void gtk_plot_candle_init 	(GtkPlotCandle *data);
static void gtk_plot_candle_draw_legend	(GtkPlotData *data, 
					 gint x, gint y);
static void gtk_plot_candle_draw_symbol	(GtkPlotData *data,
                                     	 gdouble x, 
					 gdouble y, 
					 gdouble z, 
					 gdouble a,
                                     	 gdouble dx, 
					 gdouble dy, 
					 gdouble dz, 
					 gdouble da);

extern inline gint roundint (gdouble x);


static GtkPlotDataClass *parent_class = NULL;

GtkType
gtk_plot_candle_get_type (void)
{
  static GtkType data_type = 0;

  if (!data_type)
    {
      GtkTypeInfo data_info =
      {
	"GtkPlotCandle",
	sizeof (GtkPlotCandle),
	sizeof (GtkPlotCandleClass),
	(GtkClassInitFunc) gtk_plot_candle_class_init,
	(GtkObjectInitFunc) gtk_plot_candle_init,
	/* reserved 1*/ NULL,
        /* reserved 2 */ NULL,
        (GtkClassInitFunc) NULL,
      };

      data_type = gtk_type_unique (gtk_plot_data_get_type(), &data_info);
    }
  return data_type;
}

static void
gtk_plot_candle_class_init (GtkPlotCandleClass *klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkPlotDataClass *data_class;

  parent_class = gtk_type_class (gtk_plot_data_get_type ());

  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;
  data_class = (GtkPlotDataClass *) klass;

  data_class->draw_legend = gtk_plot_candle_draw_legend;
  data_class->draw_symbol = gtk_plot_candle_draw_symbol;
/*
  data_class->draw_data = gtk_plot_candle_draw;
*/
}


static void
gtk_plot_candle_init (GtkPlotCandle *dataset)
{
  GtkWidget *widget;
  GdkColor black, white;
  GdkColormap *colormap;
  GtkPlotArray *dim;

  widget = GTK_WIDGET(dataset);

  colormap = gdk_colormap_get_system();

  gdk_color_black(colormap, &black);
  gdk_color_white(colormap, &white);

  GTK_PLOT_DATA(dataset)->symbol.symbol_style = GTK_PLOT_SYMBOL_FILLED;
  GTK_PLOT_DATA(dataset)->symbol.color = white;
  GTK_PLOT_DATA(dataset)->line.line_style = GTK_PLOT_LINE_SOLID;
  GTK_PLOT_DATA(dataset)->line.line_width = 1;
  GTK_PLOT_DATA(dataset)->line.color = black;
  
  dim = gtk_plot_data_find_dimension(GTK_PLOT_DATA(dataset), "y");
  gtk_plot_array_set_label(dim, "Open");
  gtk_plot_array_set_description(dim, "Open");
  dim = gtk_plot_data_find_dimension(GTK_PLOT_DATA(dataset), "z");
  gtk_plot_array_set_required(dim, TRUE);
  gtk_plot_array_set_label(dim, "Close");
  gtk_plot_array_set_description(dim, "Close");
  dim = gtk_plot_data_find_dimension(GTK_PLOT_DATA(dataset), "dy");
  gtk_plot_array_set_required(dim, TRUE);
  gtk_plot_array_set_label(dim, "Min");
  gtk_plot_array_set_description(dim, "Minimum");
  dim = gtk_plot_data_find_dimension(GTK_PLOT_DATA(dataset), "dz");
  gtk_plot_array_set_required(dim, TRUE);
  gtk_plot_array_set_label(dim, "Max");
  gtk_plot_array_set_description(dim, "Maximum");
}

GtkWidget*
gtk_plot_candle_new (void)
{
  GtkWidget *widget;

  widget = gtk_type_new (gtk_plot_candle_get_type ());

  return (widget);
}

static void
gtk_plot_candle_draw_symbol(GtkPlotData *dataset,
                         gdouble x, gdouble y, gdouble z, gdouble a,
                         gdouble dx, gdouble dy, gdouble dz, gdouble da)
{
  GtkPlot *plot;
  GtkPlotCandle *box = NULL;
  gdouble px, py, pz, ex, ey, ez;
  gdouble x1 = 0.0, y1 = 0.0, width = 0.0, height = 0.0;
  gdouble m;
  gboolean filled;
  gdouble a_scale;


  g_return_if_fail(GTK_IS_PLOT_CANDLE(dataset));

  box = GTK_PLOT_CANDLE(dataset);

  g_return_if_fail(dataset->plot != NULL);

  plot = dataset->plot;

  a_scale = gtk_plot_data_get_a_scale(dataset);
  m = plot->magnification * a_scale;

  gtk_plot_pc_set_lineattr (plot->pc, dataset->symbol.border.line_width, 
                            0, 0, 0);
  gtk_plot_pc_set_dash (plot->pc, 0, 0, 0); 

  if(x >= plot->xmin && x <= plot->xmax){
    if(GTK_IS_PLOT3D(plot)){
    }else if(dataset->show_zerrbars){
       gtk_plot_pc_set_color(plot->pc, &dataset->symbol.border.color);
       gtk_plot_get_pixel(plot, x, y, &px, &py);
       gtk_plot_get_pixel(plot, x, z, &px, &pz);
       width = m * dataset->symbol.size / 2;
       x1 = px - width;
       y1 = MIN(py, pz);
       filled = z < y;
       height = abs(py - pz);

       gtk_plot_get_pixel(plot, x, dy, &ex, &ey);
       gtk_plot_get_pixel(plot, x, dz, &ex, &ez);

       gtk_plot_pc_draw_line(plot->pc, px, py, px, ey);
       gtk_plot_pc_draw_line(plot->pc, px, pz, px, ez);

       if(!filled && dataset->symbol.symbol_style != GTK_PLOT_SYMBOL_EMPTY){
         gtk_plot_pc_set_color(plot->pc, &plot->background);
         gtk_plot_pc_draw_rectangle (plot->pc,
                                     TRUE,
                                     x1, y1, width * 2, height);
       }


       if(dataset->symbol.symbol_style == GTK_PLOT_SYMBOL_EMPTY){
         GtkPlotPoint lines[4];
         gtk_plot_pc_set_color(plot->pc, &dataset->symbol.color);
         lines[0].x = px - width;
         lines[0].y = py;
         lines[1].x = px;
         lines[1].y = py;
         lines[2].x = px;
         lines[2].y = pz;
         lines[3].x = px + width;
         lines[3].y = pz;
         gtk_plot_pc_draw_lines(plot->pc, lines, 4);
       } else {
         if(filled){
           gtk_plot_pc_set_color(plot->pc, &dataset->symbol.color);
           gtk_plot_pc_draw_rectangle (plot->pc,
                                       TRUE,
                                       x1, y1, width * 2, height);
         }
         gtk_plot_pc_set_color(plot->pc, &dataset->symbol.border.color);
         gtk_plot_pc_draw_rectangle (plot->pc,
                                     FALSE,
                                     x1, y1, width * 2, height);
       }

    }  
  }

}


static void
gtk_plot_candle_draw_legend(GtkPlotData *data, gint x, gint y)
{
  GtkPlotCandle *box;
  GtkPlot *plot = NULL;
  GtkPlotText legend;
  GdkRectangle area;
  gint lascent, ldescent, lheight, lwidth;
  gdouble m;

  box = GTK_PLOT_CANDLE(data);

  g_return_if_fail(data->plot != NULL);
  g_return_if_fail(GTK_IS_PLOT(data->plot));
  if(!GTK_WIDGET_REALIZED(data->plot)) return;

  plot = data->plot;
  area.x = GTK_WIDGET(plot)->allocation.x;
  area.y = GTK_WIDGET(plot)->allocation.y;
  area.width = GTK_WIDGET(plot)->allocation.width;
  area.height = GTK_WIDGET(plot)->allocation.height;

  m = plot->magnification;
  legend = plot->legends_attr;

  if(data->legend)
    legend.text = data->legend;
  else
    legend.text = "";

  gtk_plot_text_get_size(legend.text, legend.angle, legend.font,
                         roundint(legend.height * m), 
                         &lwidth, &lheight,
                         &lascent, &ldescent);


  legend.x = (gdouble)(area.x + x + roundint((plot->legends_line_width + 4) * m))
             / (gdouble)area.width;
  legend.y = (gdouble)(area.y + y + lascent) / (gdouble)area.height;

  gtk_plot_draw_text(plot, legend);

  gtk_plot_pc_set_lineattr (plot->pc, data->symbol.border.line_width, 0, 0, 0);
  gtk_plot_pc_set_dash (plot->pc, 0, 0, 0);

  gtk_plot_pc_set_color(plot->pc, &data->symbol.color);

  if(data->symbol.symbol_style == GTK_PLOT_SYMBOL_EMPTY){
    gtk_plot_pc_draw_line(plot->pc, 
                          x,  
                          y + (lascent + ldescent) / 2,
                          x + roundint(plot->legends_line_width * m), 
                          y + (lascent + ldescent) / 2); 
  } else {
    gtk_plot_pc_draw_rectangle(plot->pc, TRUE, 
                               x, y,
                               roundint(plot->legends_line_width * m), 
                               lascent + ldescent);
    gtk_plot_pc_set_color(plot->pc, &data->symbol.border.color);
    gtk_plot_pc_draw_rectangle(plot->pc, FALSE, 
                               x, y,
                               roundint(plot->legends_line_width * m), 
                               lascent + ldescent);

  }

}

