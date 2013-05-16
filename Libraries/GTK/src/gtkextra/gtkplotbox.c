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
#include "gtkplotbox.h"
#include "gtkpsfont.h"

#define P_(string) string

static void gtk_plot_box_class_init 	(GtkPlotBoxClass *klass);
static void gtk_plot_box_init 		(GtkPlotBox *data);
static void gtk_plot_box_set_property   (GObject *object,
                                         guint prop_id,
                                         const GValue *value,
                                         GParamSpec *pspec);
static void gtk_plot_box_get_property   (GObject *object,
                                         guint prop_id,
                                         GValue *value,
                                         GParamSpec *pspec);
static void gtk_plot_box_draw_legend	(GtkPlotData *data,
					 gint x, gint y);
static void gtk_plot_box_draw_symbol	(GtkPlotData *data,
                                     	 gdouble x,
					 gdouble y,
					 gdouble z,
					 gdouble a,
                                     	 gdouble dx,
					 gdouble dy,
					 gdouble dz,
					 gdouble da);
static void gtk_plot_box_clone          (GtkPlotData *data, GtkPlotData *copy);

extern inline gint roundint (gdouble x);

enum {
  ARG_0,
  ARG_ORIENTATION,
};

static GtkPlotDataClass *parent_class = NULL;

GtkType
gtk_plot_box_get_type (void)
{
  static GtkType data_type = 0;

  if (!data_type)
    {
      GtkTypeInfo data_info =
      {
	"GtkPlotBox",
	sizeof (GtkPlotBox),
	sizeof (GtkPlotBoxClass),
	(GtkClassInitFunc) gtk_plot_box_class_init,
	(GtkObjectInitFunc) gtk_plot_box_init,
	/* reserved 1*/ NULL,
        /* reserved 2 */ NULL,
        (GtkClassInitFunc) NULL,
      };

      data_type = gtk_type_unique (gtk_plot_data_get_type(), &data_info);
    }
  return data_type;
}

static void
gtk_plot_box_class_init (GtkPlotBoxClass *klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkPlotDataClass *data_class;
  GObjectClass *gobject_class = G_OBJECT_CLASS(klass);

  parent_class = gtk_type_class (gtk_plot_data_get_type ());

  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;
  data_class = (GtkPlotDataClass *) klass;

  gobject_class->set_property = gtk_plot_box_set_property;
  gobject_class->get_property = gtk_plot_box_get_property;

  g_object_class_install_property(gobject_class,
                           ARG_ORIENTATION,
  g_param_spec_enum ("orientation",
                     P_("Orientation"),
                     P_("Orientation"),
                     GTK_TYPE_ORIENTATION, 0,
                     G_PARAM_READABLE|G_PARAM_WRITABLE));

  data_class->clone = gtk_plot_box_clone;
  data_class->draw_legend = gtk_plot_box_draw_legend;
  data_class->draw_symbol = gtk_plot_box_draw_symbol;
/*
  data_class->draw_data = gtk_plot_box_draw;
*/
}

static void
gtk_plot_box_set_property (GObject      *object,
                         guint prop_id,
                         const GValue *value,
                         GParamSpec *pspec)
{
  GtkPlotBox *data;

  data = GTK_PLOT_BOX (object);

  switch (prop_id)
    {
      case ARG_ORIENTATION:
        data->orientation  = g_value_get_enum(value);
        break;
    }
}

static void
gtk_plot_box_get_property (GObject      *object,
                         guint prop_id,
                         GValue *value,
                         GParamSpec *pspec)
{
  GtkPlotBox *data;

  data = GTK_PLOT_BOX (object);

  switch (prop_id)
    {
      case ARG_ORIENTATION:
        g_value_set_enum(value, data->orientation);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
        break;
    }
}


static void
gtk_plot_box_init (GtkPlotBox *dataset)
{
  GtkWidget *widget;
  GdkColor black, white;
  GdkColormap *colormap;

  widget = GTK_WIDGET(dataset);

  colormap = gdk_colormap_get_system();

  gdk_color_black(colormap, &black);
  gdk_color_white(colormap, &white);

  GTK_PLOT_DATA(dataset)->symbol.symbol_style = GTK_PLOT_SYMBOL_FILLED;
  GTK_PLOT_DATA(dataset)->symbol.color = white;
  GTK_PLOT_DATA(dataset)->line.line_style = GTK_PLOT_LINE_SOLID;
  GTK_PLOT_DATA(dataset)->line.line_width = 1;
  GTK_PLOT_DATA(dataset)->line.color = black;
}

GtkWidget*
gtk_plot_box_new (GtkOrientation orientation)
{
  GtkWidget *widget;

  widget = gtk_type_new (gtk_plot_box_get_type ());

  gtk_plot_box_construct(GTK_PLOT_BOX(widget), orientation);

  return (widget);
}

void
gtk_plot_box_construct (GtkPlotBox *box, GtkOrientation orientation)
{
  box->orientation = orientation;
}

static void
gtk_plot_box_clone(GtkPlotData *data, GtkPlotData *copy)
{
  GTK_PLOT_DATA_CLASS(parent_class)->clone(data, copy);

  GTK_PLOT_BOX(copy)->orientation = GTK_PLOT_BOX(data)->orientation;
}

static void
gtk_plot_box_draw_symbol(GtkPlotData *dataset,
                         gdouble x, gdouble y, gdouble z, gdouble a,
                         gdouble dx, gdouble dy, gdouble dz, gdouble da)
{
  GtkPlot *plot;
  GtkPlotBox *box = NULL;
  GtkPlotPoint errbar[6];
  gdouble px, py;
  gdouble px0, py0;
  gdouble px1, py1;
  gdouble x1 = 0.0, y1 = 0.0, width = 0.0, height = 0.0;
  gdouble el_x, el_y, er_x, er_y, ed_x, ed_y, eu_x, eu_y;
  gdouble m;
  gdouble a_scale;


  g_return_if_fail(GTK_IS_PLOT_BOX(dataset));

  box = GTK_PLOT_BOX(dataset);

  g_return_if_fail(dataset->plot != NULL);

  plot = dataset->plot;

  a_scale = gtk_plot_data_get_a_scale(dataset);
  m = plot->magnification * a_scale;

  gtk_plot_pc_set_lineattr (plot->pc, dataset->symbol.border.line_width,
                            0, 0, 0);
  gtk_plot_pc_set_dash (plot->pc, 0, 0, 0);

  if(x >= plot->xmin && x <= plot->xmax){
    if(GTK_IS_PLOT3D(plot)){
    }else{
      if(dataset->show_zerrbars){
        gtk_plot_pc_set_color(plot->pc, &dataset->symbol.border.color);
        switch(box->orientation){
          case GTK_ORIENTATION_VERTICAL:
            gtk_plot_get_pixel(plot, x, y, &px, &py);
            gtk_plot_get_pixel(plot, x, y, &px0, &py0);
            gtk_plot_get_pixel(plot, x, z, &px1, &py1);
            width = roundint(dataset->symbol.size * m);
            height = abs(py1 - py0);

            gtk_plot_get_pixel(plot, x, z + dy, &eu_x, &eu_y);
            gtk_plot_get_pixel(plot, x, y - dy, &ed_x, &ed_y);

            errbar[0].x = px-roundint(m * dataset->symbol.size/2);
            errbar[0].y = eu_y;
            errbar[1].x = px+roundint(m * dataset->symbol.size/2);
            errbar[1].y = eu_y;
            gtk_plot_pc_draw_lines(plot->pc, errbar, 2);

            errbar[0].x = px;
            errbar[0].y = eu_y;
            errbar[1].x = px;
            errbar[1].y = py1;
            gtk_plot_pc_draw_lines(plot->pc, errbar, 2);

            errbar[0].x = px-roundint(m * dataset->symbol.size/2);
            errbar[0].y = ed_y;
            errbar[1].x = px+roundint(m * dataset->symbol.size/2);
            errbar[1].y = ed_y;
            gtk_plot_pc_draw_lines(plot->pc, errbar, 2);

            errbar[0].x = px;
            errbar[0].y = ed_y;
            errbar[1].x = px;
            errbar[1].y = py0;
            gtk_plot_pc_draw_lines(plot->pc, errbar, 2);

            break;
          case GTK_ORIENTATION_HORIZONTAL:
            gtk_plot_get_pixel(plot, y, x, &px, &py);
            gtk_plot_get_pixel(plot, y, x, &px0, &py0);
            gtk_plot_get_pixel(plot, z, x, &px1, &py1);
            height = roundint(dataset->symbol.size * m);

            gtk_plot_get_pixel(plot, z + dy, x, &er_x, &er_y);
            gtk_plot_get_pixel(plot, y - dy, x, &el_x, &el_y);

            errbar[0].x = el_x;
            errbar[0].y = py-roundint(m * dataset->symbol.size / 2.);
            errbar[1].x = el_x;
            errbar[1].y = py+roundint(m * dataset->symbol.size / 2.);
            gtk_plot_pc_draw_lines(plot->pc, errbar, 2);

            errbar[0].x = el_x;
            errbar[0].y = py;
            errbar[1].x = px0;
            errbar[1].y = py;
            gtk_plot_pc_draw_lines(plot->pc, errbar, 2);

            errbar[0].x = er_x;
            errbar[0].y = py-roundint(m * dataset->symbol.size / 2.);
            errbar[1].x = er_x;
            errbar[1].y = py+roundint(m * dataset->symbol.size / 2.);
            gtk_plot_pc_draw_lines(plot->pc, errbar, 2);

            errbar[0].x = er_x;
            errbar[0].y = py;
            errbar[1].x = px1;
            errbar[1].y = py;
            gtk_plot_pc_draw_lines(plot->pc, errbar, 2);

            break;
        }
      }

      switch(box->orientation){
        case GTK_ORIENTATION_VERTICAL:
          gtk_plot_get_pixel(plot, x, y, &px, &py);
          gtk_plot_get_pixel(plot, x, y, &px0, &py0);
          gtk_plot_get_pixel(plot, x, z, &px1, &py1);
          y1 = MIN(py0,py1);
          height = fabs(py0-py1);
          width = roundint(dataset->symbol.size * m);
          x1 = px0 - width / 2;
          break;
        case GTK_ORIENTATION_HORIZONTAL:
          gtk_plot_get_pixel(plot, y, x, &px, &py);
          gtk_plot_get_pixel(plot, y, x, &px0, &py0);
          gtk_plot_get_pixel(plot, z, x, &px1, &py1);
          x1 = MIN(px0,px1);
          width = fabs(px0-px1);
          height = roundint(dataset->symbol.size * m);
          y1 = py0 - height / 2;
          break;
      }

      if(dataset->symbol.symbol_style == GTK_PLOT_SYMBOL_OPAQUE){
        gtk_plot_pc_set_color(plot->pc, &plot->background);
        gtk_plot_pc_draw_rectangle (plot->pc,
                                    TRUE,
                                    x1, y1, width, height);
      }

      if(dataset->symbol.symbol_style == GTK_PLOT_SYMBOL_FILLED){
        gtk_plot_pc_set_color(plot->pc, &dataset->symbol.color);
        gtk_plot_pc_draw_rectangle (plot->pc,
                                    TRUE,
                                    x1, y1, width, height);
      }

      gtk_plot_pc_set_color(plot->pc, &dataset->symbol.border.color);
      gtk_plot_pc_draw_rectangle (plot->pc,
                                  FALSE,
                                  x1, y1, width, height);

      switch(box->orientation){
        case GTK_ORIENTATION_VERTICAL:
          gtk_plot_pc_draw_line(plot->pc, px-width/2, py, px+width/2, py);
          break;
        case GTK_ORIENTATION_HORIZONTAL:
          gtk_plot_pc_draw_line(plot->pc, px, py-height/2, px, py+height/2);
          break;
      }

    }
  }

}


static void
gtk_plot_box_draw_legend(GtkPlotData *data, gint x, gint y)
{
  GtkPlotBox *box;
  GtkPlot *plot = NULL;
  GtkPlotText legend;
  GdkRectangle area;
  gint lascent, ldescent, lheight, lwidth;
  gdouble m;

  box = GTK_PLOT_BOX(data);

  g_return_if_fail(data->plot != NULL);
  g_return_if_fail(GTK_IS_PLOT(data->plot));
  g_return_if_fail(GTK_WIDGET_REALIZED(data->plot));

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

  if(data->symbol.symbol_style == GTK_PLOT_SYMBOL_OPAQUE){
    gtk_plot_pc_set_color(plot->pc, &plot->background);
    gtk_plot_pc_draw_rectangle(plot->pc, TRUE,
                               x, y,
                               roundint(plot->legends_line_width * m),
                               lascent + ldescent);
  }

  gtk_plot_pc_set_lineattr (plot->pc, data->symbol.border.line_width, 0, 0, 0);
  gtk_plot_pc_set_dash (plot->pc, 0, 0, 0);

  if(data->symbol.symbol_style == GTK_PLOT_SYMBOL_FILLED){
    gtk_plot_pc_set_color(plot->pc, &data->symbol.color);
    gtk_plot_pc_draw_rectangle(plot->pc, TRUE,
                               x, y,
                               roundint(plot->legends_line_width * m),
                               lascent + ldescent);
  }

  gtk_plot_pc_set_color(plot->pc, &data->symbol.border.color);
  gtk_plot_pc_draw_rectangle(plot->pc, FALSE,
                             x, y,
                             roundint(plot->legends_line_width * m),
                             lascent + ldescent);

}

