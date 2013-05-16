/* gtkplotflux - flux plots widget for gtk+
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
#include "gtkplotflux.h"
#include "gtkpsfont.h"

#define P_(string) string

static void gtk_plot_flux_class_init 	(GtkPlotFluxClass *klass);
static void gtk_plot_flux_init 		(GtkPlotFlux *data);
static void gtk_plot_flux_destroy 	(GtkObject *data);
static void gtk_plot_flux_get_property  (GObject      *object,
                                         guint            prop_id,
                                         GValue          *value,
                                         GParamSpec      *pspec);
static void gtk_plot_flux_set_property  (GObject      *object,
                                         guint            prop_id,
                                         const GValue          *value,
                                         GParamSpec      *pspec);

static void gtk_plot_flux_get_legend_size(GtkPlotData *data, 
					 gint *width, gint *height);
static void gtk_plot_flux_draw_legend	(GtkPlotData *data, 
					 gint x, gint y);
static void gtk_plot_flux_draw_symbol	(GtkPlotData *data,
                                         gdouble x, 
					 gdouble y, 
					 gdouble z,
					 gdouble a,
                                         gdouble dx, 
					 gdouble dy, 
					 gdouble dz, 
					 gdouble da);
static void gtk_plot_flux_draw_arrow	(GtkPlotFlux *flux, 
                                         gdouble x1, gdouble y1, 
                                         gdouble x2, gdouble y2);


extern inline gint roundint (gdouble x);

enum {
  ARG_0,
  ARG_CENTERED,
  ARG_STYLE,
  ARG_WIDTH,
  ARG_LENGTH,
  ARG_SCALE_MAX,
  ARG_SIZE_MAX,
  ARG_SHOW_SCALE,
  ARG_LABEL_PRECISION,
  ARG_LABEL_STYLE,
  ARG_LABEL_PREFIX,
  ARG_LABEL_SUFFIX,
};

static GtkPlotDataClass *parent_class = NULL;

GtkType
gtk_plot_flux_get_type (void)
{
  static GtkType data_type = 0;

  if (!data_type)
    {
      GtkTypeInfo data_info =
      {
	"GtkPlotFlux",
	sizeof (GtkPlotFlux),
	sizeof (GtkPlotFluxClass),
	(GtkClassInitFunc) gtk_plot_flux_class_init,
	(GtkObjectInitFunc) gtk_plot_flux_init,
	/* reserved 1*/ NULL,
        /* reserved 2 */ NULL,
        (GtkClassInitFunc) NULL,
      };

      data_type = gtk_type_unique (gtk_plot_data_get_type(), &data_info);
    }
  return data_type;
}

static void
gtk_plot_flux_class_init (GtkPlotFluxClass *klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkPlotDataClass *data_class;
  GObjectClass *gobject_class = G_OBJECT_CLASS(klass);

  parent_class = gtk_type_class (gtk_plot_data_get_type ());

  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;
  data_class = (GtkPlotDataClass *) klass;

  gobject_class->set_property = gtk_plot_flux_set_property;
  gobject_class->get_property = gtk_plot_flux_get_property;
  object_class->destroy = gtk_plot_flux_destroy;

  g_object_class_install_property (gobject_class,
                           ARG_CENTERED,
  g_param_spec_boolean ("centered",
                           P_(""),
                           P_(""),
                           FALSE,
                           G_PARAM_READABLE|G_PARAM_WRITABLE));
  g_object_class_install_property (gobject_class,
                           ARG_STYLE,
  g_param_spec_int ("style",
                           P_(""),
                           P_(""),
                           0,G_MAXINT,0,
                           G_PARAM_READABLE|G_PARAM_WRITABLE));
  g_object_class_install_property (gobject_class,
                           ARG_WIDTH,
  g_param_spec_int ("width",
                           P_(""),
                           P_(""),
                           0,G_MAXINT,0,
                           G_PARAM_READABLE|G_PARAM_WRITABLE));
  g_object_class_install_property (gobject_class,
                           ARG_LENGTH,
  g_param_spec_int ("length",
                           P_(""),
                           P_(""),
                           0,G_MAXINT,0,
                           G_PARAM_READABLE|G_PARAM_WRITABLE));
  g_object_class_install_property (gobject_class,
                           ARG_SCALE_MAX,
  g_param_spec_double ("scale_max",
                           P_(""),
                           P_(""),
                           0,G_MAXDOUBLE,0,
                           G_PARAM_READABLE|G_PARAM_WRITABLE));
  g_object_class_install_property (gobject_class,
                           ARG_SIZE_MAX,
  g_param_spec_int ("size_max",
                           P_(""),
                           P_(""),
                           0,G_MAXINT,0,
                           G_PARAM_READABLE|G_PARAM_WRITABLE));
  g_object_class_install_property (gobject_class,
                           ARG_SHOW_SCALE,
  g_param_spec_boolean ("show_scale",
                           P_(""),
                           P_(""),
                           FALSE,
                           G_PARAM_READABLE|G_PARAM_WRITABLE));
  g_object_class_install_property (gobject_class,
                           ARG_LABEL_PRECISION,
  g_param_spec_int ("labels_precision",
                           P_(""),
                           P_(""),
                           0,G_MAXINT,0,
                           G_PARAM_READABLE|G_PARAM_WRITABLE));
  g_object_class_install_property (gobject_class,
                           ARG_LABEL_STYLE,
  g_param_spec_int ("labels_style",
                           P_(""),
                           P_(""),
                           0,G_MAXINT,0,
                           G_PARAM_READABLE|G_PARAM_WRITABLE));
  g_object_class_install_property (gobject_class,
                           ARG_LABEL_PREFIX,
  g_param_spec_string ("labels_prefix",
                           P_(""),
                           P_(""),
                           NULL,
                           G_PARAM_READABLE|G_PARAM_WRITABLE));
  g_object_class_install_property (gobject_class,
                           ARG_LABEL_SUFFIX,
  g_param_spec_string ("labels_suffix",
                           P_(""),
                           P_(""),
                           NULL,
                           G_PARAM_READABLE|G_PARAM_WRITABLE));

  data_class->draw_legend = gtk_plot_flux_draw_legend;
  data_class->get_legend_size = gtk_plot_flux_get_legend_size;
  data_class->draw_symbol = gtk_plot_flux_draw_symbol;
}


static void
gtk_plot_flux_init (GtkPlotFlux *dataset)
{
  GtkWidget *widget;
  GdkColor black, white;
  GdkColormap *colormap;
  GtkPlotArray *dim;

  widget = GTK_WIDGET(dataset);

  colormap = gdk_colormap_get_system();

  gdk_color_black(colormap, &black);
  gdk_color_white(colormap, &white);

  GTK_PLOT_DATA(dataset)->symbol.symbol_style = GTK_PLOT_SYMBOL_EMPTY;
  GTK_PLOT_DATA(dataset)->symbol.color = black;
  GTK_PLOT_DATA(dataset)->line.line_style = GTK_PLOT_LINE_SOLID;
  GTK_PLOT_DATA(dataset)->line.line_width = 1;
  GTK_PLOT_DATA(dataset)->line.color = black;

  dataset->centered = TRUE;
  dataset->arrow_length = 8;
  dataset->arrow_width = 8;
  dataset->arrow_style = GTK_PLOT_SYMBOL_FILLED;

  dataset->size_max = 1;
  dataset->scale_max = 1.;
  dataset->show_scale = TRUE;

  dataset->labels_precision = 3;
  dataset->labels_style = GTK_PLOT_LABEL_FLOAT;
  dataset->labels_prefix = NULL;
  dataset->labels_suffix = NULL;

  dim = gtk_plot_data_find_dimension(GTK_PLOT_DATA(dataset), "x");
  gtk_plot_array_set_description(dim, "Position X");
  dim = gtk_plot_data_find_dimension(GTK_PLOT_DATA(dataset), "y");
  gtk_plot_array_set_description(dim, "Position Y");
  gtk_plot_array_set_required(dim, TRUE);
  gtk_plot_array_set_independent(dim, TRUE);
  dim = gtk_plot_data_find_dimension(GTK_PLOT_DATA(dataset), "z");
  gtk_plot_array_set_description(dim, "Position Z");
  gtk_plot_array_set_required(dim, TRUE);
  gtk_plot_array_set_independent(dim, TRUE);
  dim = gtk_plot_data_find_dimension(GTK_PLOT_DATA(dataset), "dx");
  gtk_plot_array_set_required(dim, TRUE);
  gtk_plot_array_set_label(dim, "DX");
  gtk_plot_array_set_description(dim, "Size X");
  dim = gtk_plot_data_find_dimension(GTK_PLOT_DATA(dataset), "dy");
  gtk_plot_array_set_required(dim, TRUE);
  gtk_plot_array_set_label(dim, "DY");
  gtk_plot_array_set_description(dim, "Size Y");
  dim = gtk_plot_data_find_dimension(GTK_PLOT_DATA(dataset), "dz");
  gtk_plot_array_set_required(dim, TRUE);
  gtk_plot_array_set_label(dim, "DZ");
  gtk_plot_array_set_description(dim, "Size Z");
}

static void
gtk_plot_flux_set_property (GObject      *object,
                             guint            prop_id,
                             const GValue          *value,
                             GParamSpec      *pspec)
{
  GtkPlotFlux *data;

  data = GTK_PLOT_FLUX (object);

  switch (prop_id)
    {
      case ARG_CENTERED:
        data->centered  = g_value_get_boolean(value);
        break;
      case ARG_WIDTH:
        data->arrow_width  = g_value_get_int(value);
        break;
      case ARG_LENGTH:
        data->arrow_length  = g_value_get_int(value);
        break;
      case ARG_STYLE:
        data->arrow_style  = g_value_get_int(value);
        break;
      case ARG_SCALE_MAX:
        data->scale_max  = g_value_get_double(value);
        break;
      case ARG_SIZE_MAX:
        data->size_max  = g_value_get_int(value);
        break;
      case ARG_SHOW_SCALE:
        data->show_scale  = g_value_get_boolean(value);
        break;
      case ARG_LABEL_PRECISION:
        data->labels_precision  = g_value_get_int(value);
        break;
      case ARG_LABEL_STYLE:
        data->labels_style  = g_value_get_int(value);
        break;
      case ARG_LABEL_PREFIX:
        gtk_plot_flux_set_labels_prefix(data, g_value_get_string(value));
        break;
      case ARG_LABEL_SUFFIX:
        gtk_plot_flux_set_labels_suffix(data, g_value_get_string(value));
        break;
    }
}

static void
gtk_plot_flux_get_property (GObject      *object,
                             guint            prop_id,
                             GValue          *value,
                             GParamSpec      *pspec)
{
  GtkPlotFlux *data;

  data = GTK_PLOT_FLUX (object);

  switch (prop_id)
    {
      case ARG_CENTERED:
        g_value_set_boolean(value, data->centered);
        break;
      case ARG_WIDTH:
        g_value_set_int(value, data->arrow_width);
        break;
      case ARG_LENGTH:
        g_value_set_int(value, data->arrow_length);
        break;
      case ARG_STYLE:
        g_value_set_int(value, data->arrow_style);
        break;
      case ARG_SCALE_MAX:
        g_value_set_double(value, data->scale_max);
        break;
      case ARG_SIZE_MAX:
        g_value_set_int(value, data->size_max);
        break;
      case ARG_SHOW_SCALE:
        g_value_set_boolean(value, data->show_scale);
        break;
      case ARG_LABEL_PRECISION:
        g_value_set_int(value, data->labels_precision);
        break;
      case ARG_LABEL_STYLE:
        g_value_set_int(value, data->labels_style);
        break;
      case ARG_LABEL_PREFIX:
        g_value_set_string(value, data->labels_prefix);
        break;
      case ARG_LABEL_SUFFIX:
        g_value_set_string(value, data->labels_suffix);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
        break;
    }
}

GtkWidget*
gtk_plot_flux_new ()
{
  GtkWidget *widget;

  widget = gtk_type_new (gtk_plot_flux_get_type ());

  return (widget);
}

static void
gtk_plot_flux_destroy(GtkObject *object)
{
  GtkPlotFlux *flux = GTK_PLOT_FLUX(object);

  if(flux->labels_prefix) g_free(flux->labels_prefix);
  flux->labels_prefix = NULL;
  if(flux->labels_suffix) g_free(flux->labels_suffix);
  flux->labels_suffix = NULL;

  if (GTK_OBJECT_CLASS (parent_class)->destroy)
    (*GTK_OBJECT_CLASS (parent_class)->destroy) (object);
}

static void
gtk_plot_flux_draw_symbol(GtkPlotData *dataset,
                          gdouble x, gdouble y, gdouble z, gdouble a,
                          gdouble dx, gdouble dy, gdouble dz, gdouble da)
{
  GtkPlot *plot;
  GtkPlotFlux *flux = NULL;
  GdkRectangle area, clip_area;
  gdouble m;
  gdouble x1 = 0.0, y1 = 0.0, x2 = 0.0, y2=0.0;
  gdouble factor, size, xm;

  g_return_if_fail(GTK_IS_PLOT_FLUX(dataset));

  flux = GTK_PLOT_FLUX(dataset);

  g_return_if_fail(dataset->plot != NULL);
  g_return_if_fail(GTK_WIDGET_VISIBLE(dataset->plot));

  plot = dataset->plot;

  m = plot->magnification;
  area.x = GTK_WIDGET(plot)->allocation.x;
  area.y = GTK_WIDGET(plot)->allocation.y;
  area.width = GTK_WIDGET(plot)->allocation.width;
  area.height = GTK_WIDGET(plot)->allocation.height;

  clip_area.x = area.x + roundint(plot->x * area.width);
  clip_area.y = area.y + roundint(plot->y * area.height);
  clip_area.width = roundint(plot->width * area.width);
  clip_area.height = roundint(plot->height * area.height);

/*
  gtk_plot_pc_clip(plot->pc, &clip_area);
*/


  if(GTK_IS_PLOT3D(plot)){
       gdouble z1, z2 = 0;

       xm = sqrt(dx * dx + dy * dy + dz * dz);
       factor = xm / flux->scale_max;
       size = factor * flux->size_max;
       x2 = size * dx / xm;    
       y2 = size * dy / xm;    
       z2 = size * dz / xm;    

       gtk_plot3d_get_pixel(GTK_PLOT3D(plot), x, y, z,
                            &x1, &y1, &z1);
  }else{
       if(plot->clip_data && 
          (x < plot->xmin || x > plot->xmax || y <plot->ymin || y > plot->ymax))
            return;

       xm = sqrt(dx * dx + dy * dy);
       factor = xm / flux->scale_max;
       size = factor * flux->size_max;
       x2 = size * dx / xm;    
       y2 = size * dy / xm;    
  
       gtk_plot_get_pixel(plot, x, y, &x1, &y1);

       gtk_plot_flux_draw_arrow (flux, x1, y1, x1+x2*m, y1-y2*m);
       gtk_plot_data_draw_symbol(dataset, x1, y1);
  }

/*
  gtk_plot_pc_clip(plot->pc, NULL);
*/
}

static void
gtk_plot_flux_get_legend_size(GtkPlotData *data, gint *width, gint *height)
{
  GtkPlotFlux *flux;
  GtkPlot *plot = NULL;
  GtkPlotText legend;
  gint lascent, ldescent, lheight, lwidth;
  gdouble m;
  gchar new_label[100], text[100];

  flux = GTK_PLOT_FLUX(data);
  plot = data->plot;

  m = plot->magnification;
  legend = plot->legends_attr;

  if(data->legend)
    legend.text = data->legend;
  else
    legend.text = "";

  *width = *height = 0;
  if(data->show_legend)
    gtk_plot_text_get_size(legend.text, legend.angle, legend.font,
                           roundint(legend.height * m), 
                           width, height,
                           &lascent, &ldescent);
  

  if(flux->show_scale){
    gchar aux_text[100];
    gtk_plot_axis_parse_label(data->gradient, flux->scale_max, flux->labels_precision, flux->labels_style, text);
    if(flux->labels_prefix){
      g_snprintf(aux_text, 100, "%s%s", flux->labels_prefix, text);
      g_snprintf(text, 100, aux_text);
    }
    if(flux->labels_suffix) {
      g_snprintf(aux_text, 100, "%s%s", text, flux->labels_suffix);
      g_snprintf(text, 100, aux_text);
    }

    g_snprintf(new_label, 100, "%s", text);

    legend.text = new_label;
    gtk_plot_text_get_size(legend.text, 0, legend.font,
                           roundint(legend.height * m), 
                           &lwidth, &lheight,
                           &lascent, &ldescent);

    *width = MAX(*width, lwidth + roundint(m*(flux->size_max + 8)));
    *height += MAX(lheight , roundint(m*flux->arrow_width));
  }
}


static void
gtk_plot_flux_draw_legend(GtkPlotData *data, gint x, gint y)
{
  GtkPlotFlux *flux;
  GtkPlot *plot = NULL;
  GtkPlotText legend;
  GdkRectangle area;
  gint lascent, ldescent, lheight, lwidth;
  gdouble m;
  gint line_width;
  gboolean centered;

  flux = GTK_PLOT_FLUX(data);
  centered = flux->centered;

  g_return_if_fail(data->plot != NULL);
  g_return_if_fail(GTK_IS_PLOT(data->plot));
  g_return_if_fail(GTK_WIDGET_VISIBLE(data->plot));

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

  if(data->show_legend){
    line_width = plot->legends_line_width;

    legend.x = (gdouble)(area.x + x) / (gdouble)area.width;
    legend.y = (gdouble)(area.y + y + lascent) / (gdouble)area.height;

    gtk_plot_draw_text(plot, legend);
  }
  if(flux->show_scale){
    gchar new_label[100], text_max[100];
 
    flux->centered = FALSE;
     
    gtk_plot_flux_draw_arrow(flux, 
                             area.x + x, 
                             area.y + y + lheight * 3 / 2,
                             area.x + x + roundint(flux->size_max*m),
                             area.y + y + lheight * 3 / 2);
    y += MAX(lheight, roundint(m*flux->arrow_width));  

    gtk_plot_axis_parse_label(data->gradient, flux->scale_max, flux->labels_precision, flux->labels_style, text_max);
    if(flux->labels_prefix){
      gchar aux_text[100];
      g_snprintf(aux_text, 100, "%s%s", flux->labels_prefix, text_max);
      g_snprintf(text_max, 100, aux_text);
    }
    if(flux->labels_suffix){
      gchar aux_text[100];
      g_snprintf(aux_text, 100, "%s%s", text_max, flux->labels_suffix);
      g_snprintf(text_max, 100, aux_text);
    }
    g_snprintf(new_label, 100, "%s", text_max);

    legend.x = (gdouble)(area.x + x + m*(flux->size_max + 4)) / (gdouble)area.width;
    legend.y = (gdouble)(area.y + y + lascent) / (gdouble)area.height;
    legend.text = new_label;
    gtk_plot_draw_text(plot, legend);

    flux->centered = centered;
    y += lheight;  

  } else
    y += lheight;  
}

static void
gtk_plot_flux_draw_arrow(GtkPlotFlux *flux, gdouble x1, gdouble y1, gdouble x2, gdouble y2)
{
  GtkPlot *plot;
  GtkPlotData *data;
  GtkPlotPoint arrow[3];
  gdouble xm, ym;
  gdouble width, height;
  gdouble arrow_width;
  gdouble line_width;
  gdouble angle;
  gdouble length;
  gdouble m;

  data = GTK_PLOT_DATA(flux);
  plot = data->plot;

  m = plot->magnification;

  width = fabs(x2 - x1);
  height = fabs(y2 - y1);

  if(width == 0 && height == 0) return;
  if(width != 0)
      angle = atan2((y2 - y1), (x2 - x1));
  else
      angle = asin((y2 - y1)/height);

  length = (y2 - y1)*(y2 - y1) + (x2 - x1)*(x2 - x1);
  if(length > 0.0) length = sqrt(length);

  arrow_width = flux->arrow_width;
  line_width = data->symbol.border.line_width;
  gtk_plot_pc_set_color(plot->pc, &data->symbol.color);
  gtk_plot_pc_set_lineattr (plot->pc, line_width, 0, 0, 0);
  gtk_plot_pc_set_dash (plot->pc, 0, 0, 0);

  if(flux->centered && width != 0){
    x1 -= cos(angle) * length / 2.0;
    x2 -= cos(angle) * length / 2.0;
  }
  if(flux->centered && height != 0){
    y1 -= sin(angle) * length / 2.0;
    y2 -= sin(angle) * length / 2.0;
  }


  if(flux->arrow_style == GTK_PLOT_SYMBOL_EMPTY)
    gtk_plot_pc_draw_line(plot->pc, x1, y1, x2, y2); 
  else
    gtk_plot_pc_draw_line(plot->pc, x1, y1, 
                          x2 - flux->arrow_length * m * cos(angle) / 2., 
                          y2 - flux->arrow_length * m * sin(angle) / 2.);

  arrow[1].x = x2;
  arrow[1].y = y2;
  xm = x2 - cos(angle) * flux->arrow_length * m;
  ym = y2 - sin(angle) * flux->arrow_length * m;
  arrow[0].x = xm - sin(angle)* arrow_width * m / 2.0;
  arrow[0].y = ym + cos(angle)* arrow_width * m / 2.0;
  arrow[2].x = xm + sin(angle)* arrow_width * m / 2.0;
  arrow[2].y = ym - cos(angle)* arrow_width * m / 2.0;

  switch(flux->arrow_style){
    case GTK_PLOT_SYMBOL_EMPTY:
      gtk_plot_pc_draw_lines (plot->pc, arrow, 3);
      break;
    case GTK_PLOT_SYMBOL_OPAQUE:
      gtk_plot_pc_set_color(plot->pc, &plot->background);
      gtk_plot_pc_draw_polygon (plot->pc, TRUE, arrow, 3);
      gtk_plot_pc_set_color(plot->pc, &data->symbol.color);
      gtk_plot_pc_draw_polygon (plot->pc, FALSE, arrow, 3);
      break;
    case GTK_PLOT_SYMBOL_FILLED:
      gtk_plot_pc_draw_polygon (plot->pc, TRUE, arrow, 3);
  }
}

void
gtk_plot_flux_set_arrow (GtkPlotFlux *flux, 
                         gint arrow_length, 
                         gint arrow_width, 
                         GtkPlotSymbolStyle arrow_style)
{
  flux->arrow_length = arrow_length;
  flux->arrow_width = arrow_width;
  flux->arrow_style = arrow_style;
}

void
gtk_plot_flux_get_arrow (GtkPlotFlux *flux, 
                         gint *arrow_length, 
                         gint *arrow_width, 
                         GtkPlotSymbolStyle *arrow_style)
{
  *arrow_length = flux->arrow_length;
  *arrow_width = flux->arrow_width;
  *arrow_style = flux->arrow_style;
}

void
gtk_plot_flux_center (GtkPlotFlux *flux, gboolean center)
{
  flux->centered = center;
} 

gboolean
gtk_plot_flux_is_centered (GtkPlotFlux *flux)
{
  return(flux->centered);
} 

void            
gtk_plot_flux_show_scale        (GtkPlotFlux *flux, gboolean show)
{
  flux->show_scale = show;
}

void            
gtk_plot_flux_set_scale_max     (GtkPlotFlux *flux, gdouble scale_max)
{
  flux->scale_max = fabs(scale_max);
}

void            
gtk_plot_flux_set_size_max      (GtkPlotFlux *flux, guint size_max)
{
  flux->size_max = size_max;
}

void
gtk_plot_flux_set_labels_precision (GtkPlotFlux *flux, gint precision)
{
  flux->labels_precision = precision;
}

void
gtk_plot_flux_set_labels_style (GtkPlotFlux *flux, GtkPlotLabelStyle style)
{
  flux->labels_style = style;
}

void
gtk_plot_flux_set_labels_prefix (GtkPlotFlux *flux, const gchar *prefix)
{
  if(flux->labels_prefix) g_free(flux->labels_prefix);
  flux->labels_prefix = NULL;
  if(prefix) 
    flux->labels_prefix = g_strdup(prefix);
}

void
gtk_plot_flux_set_labels_suffix (GtkPlotFlux *flux, const gchar *suffix)
{
  if(flux->labels_suffix) g_free(flux->labels_suffix);
  flux->labels_suffix = NULL;
  if(suffix) 
    flux->labels_suffix = g_strdup(suffix);
}

