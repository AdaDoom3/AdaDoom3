/* gtkplotpixmap - pixmap plots widget for gtk+
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
#include "gtkplotpixmap.h"
#include "gtkplotpc.h"
#include "gtkplotps.h"
#include "gtkpsfont.h"

#define P_(string) string

enum {
  ARG_0,
  ARG_PIXMAP,
  ARG_MASK,
};
                                                                                
static void gtk_plot_pixmap_class_init 		(GtkPlotPixmapClass *klass);
static void gtk_plot_pixmap_init 		(GtkPlotPixmap *data);
static void gtk_plot_pixmap_destroy             (GtkObject *object);
static void gtk_plot_pixmap_draw_symbol		(GtkPlotData *data,
                                                 gdouble x, 
                                                 gdouble y, 
                                                 gdouble z, 
                                                 gdouble a,
                                                 gdouble dx, 
                                                 gdouble dy, 
                                                 gdouble dz, 
                                                 gdouble da);
static void gtk_plot_pixmap_draw_legend		(GtkPlotData *data, 
					 	 gint x, gint y);
static void gtk_plot_pixmap_get_legend_size	(GtkPlotData *data, 
						 gint *width, gint *height);
static void gtk_plot_pixmap_clone               (GtkPlotData *data,
                                                 GtkPlotData *copy);
static void gtk_plot_pixmap_get_property	(GObject      *object,
                                                 guint            prop_id,
                                                 GValue          *value,
                                                 GParamSpec      *pspec);
static void gtk_plot_pixmap_set_property	(GObject      *object,
                                                 guint            prop_id,
                                                 const GValue          *value,
                                                 GParamSpec      *pspec);



extern inline gint roundint 			(gdouble x);

static GtkPlotDataClass *parent_class = NULL;

GtkType
gtk_plot_pixmap_get_type (void)
{
  static GtkType data_type = 0;

  if (!data_type)
    {
      GtkTypeInfo data_info =
      {
	"GtkPlotPixmap",
	sizeof (GtkPlotPixmap),
	sizeof (GtkPlotPixmapClass),
	(GtkClassInitFunc) gtk_plot_pixmap_class_init,
	(GtkObjectInitFunc) gtk_plot_pixmap_init,
	/* reserved 1*/ NULL,
        /* reserved 2 */ NULL,
        (GtkClassInitFunc) NULL,
      };

      data_type = gtk_type_unique (gtk_plot_data_get_type(), &data_info);
    }
  return data_type;
}

static void
gtk_plot_pixmap_class_init (GtkPlotPixmapClass *klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkPlotDataClass *data_class;
  GObjectClass *gobject_class = G_OBJECT_CLASS(klass);

  parent_class = gtk_type_class (gtk_plot_data_get_type ());

  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;
  data_class = (GtkPlotDataClass *) klass;

  data_class->clone = gtk_plot_pixmap_clone;
  data_class->draw_legend = gtk_plot_pixmap_draw_legend;
  data_class->get_legend_size = gtk_plot_pixmap_get_legend_size;
  data_class->draw_symbol = gtk_plot_pixmap_draw_symbol;

  object_class->destroy = gtk_plot_pixmap_destroy;

  gobject_class->get_property = gtk_plot_pixmap_get_property;
  gobject_class->set_property = gtk_plot_pixmap_set_property;
                                                                                
  g_object_class_install_property (gobject_class,
                           ARG_PIXMAP,
  g_param_spec_pointer ("pixmap",
                           P_("Pixmap"),
                           P_("Pixmap"),
                           G_PARAM_READABLE|G_PARAM_WRITABLE));
  g_object_class_install_property (gobject_class,
                           ARG_MASK,
  g_param_spec_pointer ("mask_bitmap",
                           P_("Mask"),
                           P_("Mask"),
                           G_PARAM_READABLE|G_PARAM_WRITABLE));

}

static void
gtk_plot_pixmap_get_property (GObject      *object,
                                    guint            prop_id,
                                    GValue          *value,
                                    GParamSpec      *pspec)
{
  GtkPlotPixmap *pixmap = GTK_PLOT_PIXMAP (object);
                                                                                
  switch(prop_id){
    case ARG_PIXMAP:
      g_value_set_pointer(value, pixmap->pixmap);
      break;
    case ARG_MASK:
      g_value_set_pointer(value, pixmap->mask);
      break;
  }
}
                                                                                
static void
gtk_plot_pixmap_set_property (GObject      *object,
                                    guint            prop_id,
                                    const GValue          *value,
                                    GParamSpec      *pspec)
{
  GtkPlotPixmap *pixmap = GTK_PLOT_PIXMAP (object);
                                                                                
  switch(prop_id){
    case ARG_PIXMAP:
      if(pixmap->pixmap) gdk_pixmap_unref(pixmap->pixmap);
      pixmap->pixmap = (GdkPixmap *)g_value_get_pointer(value);
      if(pixmap->pixmap) gdk_pixmap_ref(pixmap->pixmap);
      break;
    case ARG_MASK:
      if(pixmap->mask) gdk_bitmap_unref(pixmap->mask);
      pixmap->mask = (GdkBitmap *)g_value_get_pointer(value);
      if(pixmap->mask) gdk_bitmap_ref(pixmap->mask);
      break;
  }
}

static void
gtk_plot_pixmap_init (GtkPlotPixmap *dataset)
{
  dataset->pixmap = NULL;
}

GtkWidget*
gtk_plot_pixmap_new (GdkPixmap *pixmap, GdkBitmap *mask)
{
  GtkWidget *widget;

  widget = gtk_type_new (gtk_plot_pixmap_get_type ());

  gtk_plot_pixmap_construct(GTK_PLOT_PIXMAP(widget), pixmap, mask);

  return (widget);
}

void
gtk_plot_pixmap_construct(GtkPlotPixmap *data, GdkPixmap *pixmap, GdkBitmap *mask)
{
  data->pixmap = pixmap;
  data->mask = mask;

  if(pixmap)
    gdk_pixmap_ref(pixmap);
  if(mask)
    gdk_bitmap_ref(mask);
}

static void
gtk_plot_pixmap_destroy(GtkObject *object)
{
  GtkPlotPixmap *pixmap = GTK_PLOT_PIXMAP(object);
                                                                                
  if(pixmap->pixmap) gdk_pixmap_unref(pixmap->pixmap);
  if(pixmap->mask) gdk_bitmap_unref(pixmap->mask);
  pixmap->pixmap = NULL;
  pixmap->mask = NULL;
}

static void
gtk_plot_pixmap_clone(GtkPlotData *data, GtkPlotData *copy)
{
  GTK_PLOT_DATA_CLASS(parent_class)->clone(data, copy);

  GTK_PLOT_PIXMAP(copy)->pixmap = GTK_PLOT_PIXMAP(data)->pixmap;
  gdk_pixmap_ref(GTK_PLOT_PIXMAP(data)->pixmap);
  GTK_PLOT_PIXMAP(copy)->mask = GTK_PLOT_PIXMAP(data)->mask;
  gdk_bitmap_ref(GTK_PLOT_PIXMAP(data)->mask);
}

static void
gtk_plot_pixmap_draw_symbol(GtkPlotData *data,
                         gdouble x, gdouble y, gdouble z, gdouble a,
                         gdouble dx, gdouble dy, gdouble dz, gdouble da)
{
  GtkPlot *plot = NULL;
  GtkPlotPixmap *image;
  gdouble scale_x, scale_y;
  gdouble px, py, pz;
  gint width, height;

  image = GTK_PLOT_PIXMAP(data);
  if(!image->pixmap) return;

  plot = data->plot;

  scale_x = scale_y = data->plot->magnification;;

  gdk_window_get_size(image->pixmap, &width, &height);

  width = roundint(scale_x * width);
  height = roundint(scale_y * height);

  if(GTK_IS_PLOT3D(plot))
       gtk_plot3d_get_pixel(GTK_PLOT3D(plot), x, y, z,
                            &px, &py, &pz);
  else
       gtk_plot_get_pixel(plot, x, y, &px, &py);

/*
  gtk_plot_pc_clip_mask(data->plot->pc, 
                        px - width / 2., 
                        py - height / 2., 
                        image->mask);
*/
  gtk_plot_pc_draw_pixmap(data->plot->pc,
                          image->pixmap,
                          image->mask,
                          0, 0,
                          px - width / 2., py - height / 2.,
                          width, height,
                          scale_x, scale_y);
/*
  gtk_plot_pc_clip_mask(data->plot->pc, 
                        px - width / 2., 
                        py - height / 2., 
                        NULL);
*/
}

static void
gtk_plot_pixmap_draw_legend(GtkPlotData *data, gint x, gint y)
{
  GtkPlotPixmap *pixmap;
  GtkPlot *plot = NULL;
  GtkPlotText legend;
  GdkRectangle area;
  gint lascent, ldescent, lheight, lwidth;
  gdouble m;
  gint width, height;

  g_return_if_fail(data->plot != NULL);
  g_return_if_fail(GTK_IS_PLOT(data->plot));

  pixmap = GTK_PLOT_PIXMAP(data);

  plot = data->plot;
  area.x = GTK_WIDGET(plot)->allocation.x;
  area.y = GTK_WIDGET(plot)->allocation.y;
  area.width = GTK_WIDGET(plot)->allocation.width;
  area.height = GTK_WIDGET(plot)->allocation.height;

  m = plot->magnification;
  legend = plot->legends_attr;

  gdk_window_get_size(pixmap->pixmap, &width, &height);
  width = roundint(m * width);
  height = roundint(m * height);

  if(data->legend)
    legend.text = data->legend;
  else
    legend.text = "";

  legend.x = (gdouble)(area.x + x);
  legend.y = (gdouble)(area.y + y);

/*
  gtk_plot_pc_clip_mask(data->plot->pc, 
                        legend.x, 
                        legend.y, 
                        pixmap->mask);
*/
  gtk_plot_pc_draw_pixmap(data->plot->pc,
                          pixmap->pixmap,
                          pixmap->mask,
                          0, 0,
                          legend.x, legend.y,
                          width, height,
                          m, m);
/*
  gtk_plot_pc_clip_mask(data->plot->pc, 
                        legend.x, 
                        legend.y, 
                        NULL);
*/

  gtk_plot_text_get_size(legend.text, legend.angle, legend.font,
                         roundint(legend.height * m), 
                         &lwidth, &lheight,
                         &lascent, &ldescent);

  legend.x = (gdouble)(area.x + x + width + roundint(4*m)) / (gdouble)area.width;
  legend.y = (gdouble)(area.y + y + MAX(lheight, height) - lascent / 2) / (gdouble)area.height;

  gtk_plot_draw_text(plot, legend);
}

static void
gtk_plot_pixmap_get_legend_size(GtkPlotData *data, gint *width, gint *height)
{
  GtkPlotPixmap *pixmap;
  GtkPlot *plot = NULL;
  GtkPlotText legend;
  gint lascent, ldescent, lheight, lwidth;
  gint pwidth, pheight;
  gdouble m;

  g_return_if_fail(data->plot != NULL);
  g_return_if_fail(GTK_IS_PLOT(data->plot));

  pixmap = GTK_PLOT_PIXMAP(data);

  plot = data->plot;
  m = plot->magnification;

  legend = plot->legends_attr;

  if(data->legend)
    legend.text = data->legend;
  else
    legend.text = "";

  gdk_window_get_size(pixmap->pixmap, &pwidth, &pheight);
  pwidth = roundint(m * pwidth);
  pheight = roundint(m * pheight);

  gtk_plot_text_get_size(legend.text, legend.angle, legend.font,
                         roundint(legend.height * m),
                         &lwidth, &lheight,
                         &lascent, &ldescent);

  *width = lwidth + pwidth + roundint(12 * m);
  *height = MAX(lascent + ldescent, pheight);
}
/******************************************************
 * Public methods
 ******************************************************/

GdkPixmap *
gtk_plot_pixmap_get_pixmap (GtkPlotPixmap *pixmap)
{
  return(pixmap->pixmap);
}

GdkBitmap *
gtk_plot_pixmap_get_mask (GtkPlotPixmap *pixmap)
{
  return(pixmap->mask);
}


